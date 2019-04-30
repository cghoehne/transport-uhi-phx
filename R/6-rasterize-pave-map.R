###########################################################################
## RASTERIZE OSM ROAD NETWORK, PARKING DATA, AND VEHICLE TRAVEL DENSITY  ##
###########################################################################

# ** It is recommended to use Microsoft Open R (v3.5.1) for improved performance without compromsing compatibility **

## SCRIPT PREPERATION

# clear space and allocate memory
gc()
memory.limit(size = 50000) 
script.start <- Sys.time() # start script timestamp

# first make sure checkpoint is installed locally
# this is the only package that is ok to not use a 'checkpointed' (i.e. archived version of a package)
# checkpoint does not archive itself and it should not create dependency issues
if (!require("checkpoint")){
  install.packages("checkpoint")
  library(checkpoint, quietly = T)
}

# load all other dependant packages from the local repo
.libPaths(paste0(getwd(),"/.checkpoint/2019-01-01/lib/x86_64-w64-mingw32/3.5.1"))
library(doParallel)
library(foreach)
library(sf)
library(sp)
library(raster)
library(rgdal)
library(rgeos)
library(maptools)
library(data.table)
library(here)

# archive/update snapshot of packages at checkpoint date
checkpoint("2019-01-01", # Sys.Date() - 1  this calls the MRAN snapshot from yestersday
           R.version = "3.5.1", # will only work if using the same version of R
           checkpointLocation = here(), # calls here package
           verbose = F) 

# IMPORT DATA
osm <- shapefile(here("data/osm/maricopa_county_osm_roads.shp")) # raw OSM data for Maricopa County
parking <- readRDS(here("data/parking/phx-parking.rds")) # phoenix off-street parking space data by parcel centriod xy coords in EPSG:2223
fclass.info <- fread(here("data/osm_fclass_info.csv")) # additional OSM info by roadway functional class (fclass)
my.extent <- shapefile(here("data/shapefiles/boundaries/maricopa_county_uza.shp")) # Maricopa UZA (non-buffered) in EPSG:2223

# define name of run
run.name <- "metro-phx"
#run.name <- "phx-dwntwn"
#run.name <- "north-tempe"

# alternatively, define 2 bounding coordinates for a different extent that is within the spatial extent of the available data
# phx dwntwn: UL 33.487158, -112.122746; LR 33.419871, -112.018541
# tempe: UL 33.465809, -111.987157; LR 33.378679, -111.877737
my.crs <- crs(my.extent) # store desired CRS (EPSG:2223)
my.extent <- st_as_sf(my.extent) # store desired extent as sf obj
if(run.name != "metro-phx"){ # if not doing the full region run, adjust the extent based on lat lon boundings
  if(run.name == "phx-dwntwn"){my.coords <- data.table(lon = c(-112.122746,-112.018541), lat = c(33.487158,33.419871))}
  if(run.name == "north-tempe"){my.coords <- data.table(lon = c(-111.987157,-111.877737), lat = c(33.465809,33.378679))}
  my.extent <- as(extent(spTransform(SpatialPoints(coords = my.coords,
                                                   proj4string = crs("+proj=longlat +datum=WGS84")), my.crs)), "SpatialPolygons")
  proj4string(my.extent) <- crs(my.crs)
  my.extent <- st_as_sf(my.extent)
}

# define resolution 
#res <- 164.042  #  ~50m x 50m
#res <- 328.084  # ~100m x 100m
#res <- 820.21  # ~250m x 250m
res <- 1640.42 # ~500m x 500m
#res <- 3280.84 # ~1000 x 1000 

# raster area in sq ft and sq meters
r.area.ft2 <- res ^ 2
r.area.m2 <- (res / 3.28084) ^ 2
  
# define extent for rasterization by buffering desired extent by resolution
my.buffer <- st_buffer(my.extent, dist = res)

# create empty raster depending on buffered extent and desired resolution
r <- raster(ext = extent(my.buffer), crs = crs(my.crs), res = res) 

# PARKING DATA FORMAT
# create a SPDF from parking coords and data
parking.pts <- SpatialPointsDataFrame(parking[,.(X,Y)], # coords from EPSG:2223
                                      proj4string = crs(my.crs), # CRS EPSG:2223
                                      data = parking[, .(APN, spaces, type)]) # other data
rm(parking) # remove unused obj

# clip parking data to desired extent
parking.pts <- intersect(parking.pts, extent(my.buffer))

# calculate min and max parking area by property type and other assumptions (proj is in ft)

# for max parking area in both commerical and residentail areas, 
# assume both have 330 sq ft per space dedicated 
# this is based on Shoup and others estiamtes - Phx parking assumed this too
parking.pts$raw.area <- parking.pts$spaces * 330

# store data in data.table to adjust
#parking.dt <- as.data.table(parking.pts@data)

# adjust parking area such that as spaces per parcel are higher, there is higher amount shaded/covered/garaged
# create function that scales with number of spaces non-linearly as desired
min.shade.ratio <- function (x) {(100 - (2.17 * log(x + 1))) / 100}
max.shade.ratio <- function (x) {(100 - (6.51 * log(x + 1))) / 100}

# check log growth
min.shade.ratio(c(0,10,100,1000,10000,100000)) # at 100,000 spaces, ~25% are not visisble to sky
max.shade.ratio(c(0,10,100,1000,10000,100000)) # at 100,000 spaces, ~75% are not visisble to sky

parking.pts$min.area <- parking.pts$raw.area * max.shade.ratio(parking.pts$spaces) # max shade for min area case
parking.pts$max.area <- parking.pts$raw.area * min.shade.ratio(parking.pts$spaces) # min shade for max area case
rm(min.shade.ratio, max.shade.ratio) # remove unused obj

# OSM DATA FORMAT
# **Curently, OSM data in Phoenix does not have lane data. in light of this:
# assign minimum & maximum roadway widths based on roadway fclass 
# ranges of roadway width are based on typical regional street design & observation
# this data is all proj in EPSG 2223 and units are in feet

# remove unnecessary variables to reduce size
osm$ref <- NULL # irrelevant variable
osm$layer <- NULL # irrelevant variable
osm$maxspeed <- NULL # unused, most are 0 or missing so unuseable in this case
osm$bridge <- NULL # unused

# store osm data for quick calcs of new variables and rebind later
osm.dt <- as.data.table(osm@data)

# merge fclass.info with osm data
osm.dt <- merge(osm.dt, fclass.info, by = "fclass", all.x = T)
rm(fclass.info) # remove unused obj

# calc min/max road width based on 2way widths and 1way/2way ("B" == both, "T"/"F" True/False for driving in the opposite dir of linestring)
osm.dt[oneway == "B", max.width := as.numeric(max.2w.width.m)]
osm.dt[oneway == "B", min.width := as.numeric(min.2w.width.m)]
osm.dt[oneway == "T" | oneway == "F", max.width := as.numeric(max.2w.width.m / 2)]
osm.dt[oneway == "T" | oneway == "F", min.width := as.numeric(min.2w.width.m / 2)]

# calc min/max buffer radius (in ft from m) of each road
osm.dt[, min.r.buf := min.width * 3.28084 * 0.5] # raduis = 0.5 * diameter
osm.dt[, max.r.buf := max.width * 3.28084 * 0.5] # 3.28084 ft per meter

# drop tunnels, and non pavement fclass
osm.dt <- osm.dt[tunnel == "F" & min.2w.width.m > 0 
                 & !(fclass %in% c("bridleway", "path", "track_grade3",
                                   "track_grade4", "track_grade5", "unknown")),]

# because the fclass of "service" roads is inconsistent but does often trace parking lots,
# we ignore this in place of parking estimates which will be more consistent.
# NOTE: it also traces alleyways consistently but there is no easy way to disentangle
osm.dt <- osm.dt[fclass != "service",]

# merge filtered data with min/max roadway widths to spatial osm data
osm <- merge(osm, osm.dt[, .(osm_id, min.width, max.width, min.r.buf, max.r.buf)], by = "osm_id", all.x = F)
rm(osm.dt) # remove unused obj

# remove tunnel and oneway as no longer needed
osm$tunnel <- NULL
osm$oneway <- NULL

# transform osm crs to EPSG:2223
osm <- spTransform(osm, crs(my.crs))

# clip osm data to my buffer for quicker computing
osm.sf <- st_as_sf(osm)
osm <- st_intersection(osm.sf, my.buffer$geometry)
osm <- as(osm, "Spatial")
rm(osm.sf)

# store the number of cores
my.cores <- parallel::detectCores() - 1 # n-1 for headspace

# create empty list for foreach computations
my.list <- list() 

# create list of unique min/max buffer widths based on oneway and estiamted roadway width
w.min <- unique(osm$min.r.buf) 
w.max <- unique(osm$max.r.buf)  

cat(paste0("Time: ", Sys.time(),  # script run time update
           ".\nDuration: ", round(difftime(Sys.time(), script.start, units = "mins")), " mins."))
gc() # initiate cluster after only all the necessary objects present in R environment
while(exists("cl") == F){ # ensures the cluster is made even if fails on first try   
  cl <- makeCluster(my.cores, outfile = "")} # so just while loop until it is sucsessful 
registerDoParallel(cl) # register parallel backend
invisible(clusterCall(cl, function(x) .libPaths(x), .libPaths())) # supress printing

# parellel buffer roadways
osm.buf.min <- foreach(i = 1:length(w.min), .packages = c("sp","rgeos")) %dopar% {
  my.list[[i]] <- gBuffer(osm[osm$min.r.buf == w.min[i], ], byid = T, width = w.min[i], capStyle = "ROUND") # round b/c end of roads are usually cul-de-sac
}
osm.buf.max <- foreach(i = 1:length(w.max), .packages = c("sp","rgeos")) %dopar% {
  my.list[[i]] <- gBuffer(osm[osm$max.r.buf == w.max[i], ], byid = T, width = w.max[i], capStyle = "ROUND") # round b/c end of roads are usually cul-de-sac
} 

# stop and remove cluster
stopCluster(cl)
rm(cl, osm)

# bind the buffered osm data output
osm.buf.mrg.min <- do.call(raster::bind, osm.buf.min) # bind list of spatial objects into single spatial obj
osm.buf.mrg.max <- do.call(raster::bind, osm.buf.max) # bind list of spatial objects into single spatial obj
rm(osm.buf.min, osm.buf.max) # remove unused objects

# subract overlapping areas, removing lower tier road class 
# ******NOT YET IMPLEMENTED********

# estiamte spatial extent of parking by buffering each point such that area of circle = parking area
# therefore r = sqrt(parking area / pi)
# because this is an approximation it is off by a slight amount, so we estimate the factor it is off by (1.016641x)
parking.pts$min.r <- sqrt(parking.pts$min.area * 1.016641 / pi)
parking.pts$max.r <- sqrt(parking.pts$max.area * 1.016641 / pi)

# split total point features in parking data into parts for parellel splitting
parts.p <- split(1:nrow(parking.pts[,]), cut(1:nrow(parking.pts[,]), my.cores))

# buffer each parking point to create min and max parking area circles to clip by raster area
# this ensures that large parking lots are not concentrated into a single cell
# then clip all parts by raster and convert clipped parts to individual centriods

# BUFFER PARKING POINT DATA
cat(paste0("Time: ", Sys.time(),  # script run time update
           ".\nDuration: ", round(difftime(Sys.time(), script.start, units = "mins")), " mins."))
gc() # initiate cluster after only all the necessary objects present in R environment
while(exists("cl") == F){ # for some reason makeCluster has been failing on the first try   
  cl <- makeCluster(my.cores, outfile = "") # so just while loop until it is sucsessful 
} 
registerDoParallel(cl) # register parallel backend
invisible(clusterCall(cl, function(x) .libPaths(x), .libPaths())) # supress printing 

park.buf.min.p <- foreach(i = 1:my.cores, .packages = c("sp","rgeos")) %dopar% {
  my.list[[i]] <- gBuffer(parking.pts[parts.p[[i]],], byid = T, width = parking.pts$min.r[parts.p[[i]]], capStyle = "ROUND")
}
park.buf.max.p <- foreach(i = 1:my.cores, .packages = c("sp","rgeos")) %dopar% {
  my.list[[i]] <- gBuffer(parking.pts[parts.p[[i]],], byid = T, width = parking.pts$max.r[parts.p[[i]]], capStyle = "ROUND")
}

# stop cluster
stopCluster(cl)
rm(cl, parking.pts, parts.p)

# bind the buffered parking data output
park.buf.min <- do.call(raster::bind, park.buf.min.p) # bind list of spatial objects into single spatial obj
park.buf.max <- do.call(raster::bind, park.buf.max.p) # bind list of spatial objects into single spatial obj
rm(park.buf.min.p, park.buf.max.p)


# actual areas (use to check accuracy)
#park.buf.min$min.area.act <- gArea(park.buf.min, byid = T)
#park.buf.max$max.area.act <- gArea(park.buf.max, byid = T)

# convert to sf objects for quicker computing (sf::st_intersection is fastest for zonal intersections)
park.min.s <- st_as_sf(park.buf.min)
park.max.s <- st_as_sf(park.buf.max)

# remove unused obj
rm(park.buf.min, park.buf.max)

# split total buffered parking data into parts for parellel splitting for clipping to my extent
parts.p.min <- split(1:nrow(park.min.s[,]), cut(1:nrow(park.min.s[,]), my.cores))
parts.p.max <- split(1:nrow(park.max.s[,]), cut(1:nrow(park.max.s[,]), my.cores))

# CLIP PARKING AREA DATA TO DESIRED (NON-BUFFERED) EXTENT
cat(paste0("Time: ", Sys.time(),  # script run time update
           ".\nDuration: ", round(difftime(Sys.time(), script.start, units = "mins")), " mins."))
gc() # initiate cluster after only all the necessary objects present in R environment
while(exists("cl") == F){ # ensures the cluster is made even if fails on first try   
  cl <- makeCluster(my.cores, outfile = "")} # so just while loop until it is sucsessful 
registerDoParallel(cl) # register parallel backend
invisible(clusterCall(cl, function(x) .libPaths(x), .libPaths())) # supress printing

# clip parking to actual extent to remove parking outside edges
park.min.i.u.p <- foreach(i = 1:my.cores, .packages = c("sf")) %dopar% {
  my.list[[i]] <- st_intersection(park.min.s[parts.p.min[[i]],], my.extent$geometry)
}
park.max.i.u.p <- foreach(i = 1:my.cores, .packages = c("sp")) %dopar% {
  my.list[[i]] <- st_intersection(park.max.s[parts.p.max[[i]],], my.extent$geometry)
}

# stop cluster
stopCluster(cl)
rm(cl, park.max.s, park.min.s, parts.p.min, parts.p.max, my.extent)

# bind the clipped-to-extent parking data output (in sf still)
park.min.i.u <- do.call(rbind, park.min.i.u.p) # bind list of spatial objects into single spatial obj
park.max.i.u <- do.call(rbind, park.max.i.u.p) # bind list of spatial objects into single spatial obj
rm(park.min.i.u.p, park.max.i.u.p)

# RASTERIZE DATA

# polygonize raster to clip other spatial data by to eventually rasterize 
r.p <- rasterToPolygons(r)
r.t <- st_as_sf(r.p) # sf object for quicker intersections

# load vehicle travel network and data and convert to sf
traffic.net <- readRDS(here("data/outputs/network/icarus-network.rds")) # cleaned/simplified ICARUS traffic network to pair with traffic data
#traffic.net <- shapefile(here("data/outputs/temp/icarus-test.shp")) # TEST ICARUS NETWORK
#traffic <- fread() simulated traffic data for region from ICARUS model
veh <- st_as_sf(traffic.net)
rm(traffic.net)

# ICARUS traffic data has 101 cols where col 1 is link id and the rest are from 12am to 12am divided in 100 chunks
# therefore every column from V2:V101 represents a 0.24 hour period
# morning rush would be from: hour = 7.92 am to 8.88 am (6:54:12 am to 8:52:48 am); V34:V37
# evening rush would be from: hour = 5.04 pm  to 6 pm (5:02:24 pm to 6:00:00 pm); V72:V75

# merge summarized icarus flow data by link id
iflow <- fread(here("data/icarus/full_flow.csv"))
setnames(iflow, "V1", "id")
veh.m <- merge(veh, iflow[, .(day.flow = sum(V2:V101),
                                fl.8.9am = sum(V34:V37),
                                fl.5.6pm = sum(V72:V75)),
                            by = id], by = "id")
rm(iflow, veh)

# sf objects for quicker intersections
osm.min.s <- st_as_sf(osm.buf.mrg.min)
osm.max.s <- st_as_sf(osm.buf.mrg.max)
rm(osm.buf.mrg.min, osm.buf.mrg.max)

# split total osm, parking, and veh data into parts for parellel splitting
parts.r.min <- split(1:nrow(osm.min.s[,]), cut(1:nrow(osm.min.s[,]), my.cores))
parts.r.max <- split(1:nrow(osm.max.s[,]), cut(1:nrow(osm.max.s[,]), my.cores))

parts.p.min <- split(1:nrow(park.min.i.u[,]), cut(1:nrow(park.min.i.u[,]), my.cores))
parts.p.max <- split(1:nrow(park.max.i.u[,]), cut(1:nrow(park.max.i.u[,]), my.cores))

parts.c <- split(1:nrow(veh.m[,]), cut(1:nrow(veh.m[,]), my.cores))


# INTERSECT POLYGONIZED RASTER w/ PARKING + ROAD DATA
cat(paste0("Time: ", Sys.time(),  # script run time update
           ".\nDuration: ", round(difftime(Sys.time(), script.start, units = "mins")), " mins."))
gc() # initiate cluster after only all the necessary objects present in R environment
while(exists("cl") == F){ # ensures the cluster is made even if fails on first try   
  cl <- makeCluster(my.cores, outfile = "")} # so just while loop until it is sucsessful 
registerDoParallel(cl) # register parallel backend
invisible(clusterCall(cl, function(x) .libPaths(x), .libPaths())) # supress printing

# park area intersect with polygonized raster
park.min.i.p <- foreach(i = 1:my.cores, .packages = c("sf")) %dopar% {
  my.list[[i]] <- st_intersection(park.min.i.u[parts.p.min[[i]],], r.t)
}
park.max.i.p <- foreach(i = 1:my.cores, .packages = c("sf")) %dopar% {
  my.list[[i]] <- st_intersection(park.max.i.u[parts.p.max[[i]],], r.t)
}

# pave area intersect with polygonized raster
osm.min.i.p <- foreach(i = 1:my.cores, .packages = c("sf")) %dopar% {
  my.list[[i]] <- st_intersection(osm.min.s[parts.r.min[[i]],], r.t)
}
osm.max.i.p <- foreach(i = 1:my.cores, .packages = c("sf")) %dopar% {
  my.list[[i]] <- st_intersection(osm.max.s[parts.r.max[[i]],], r.t)
}

# vehicle travel links intersect with polygonized raster
veh.i.p <- foreach(i = 1:my.cores, .packages = c("sf")) %dopar% {
  my.list[[i]] <- st_intersection(veh.m[parts.c[[i]],], r.t)
}

# stop cluster
stopCluster(cl)
rm(cl, parts.r.min, parts.r.max, parts.p.min, parts.p.max, parts.c)

# bind the buffered parking data output
osm.min.i <- do.call(rbind, osm.min.i.p) # bind list of spatial objects into single spatial obj
osm.max.i <- do.call(rbind, osm.max.i.p) # bind list of spatial objects into single spatial obj

park.min.i <- do.call(rbind, park.min.i.p) # bind list of spatial objects into single spatial obj
park.max.i <- do.call(rbind, park.max.i.p) # bind list of spatial objects into single spatial obj

veh.i <- do.call(rbind, veh.i.p) # bind list of spatial objects into single spatial obj

# remove unused objects
rm(osm.min.i.p, osm.max.i.p, park.min.i.p, park.max.i.p, veh.i.p)

# calc adjusted fractional area of parking/pavement in raster cell size
osm.min.i$area <- st_area(osm.min.i)
osm.min.i$frac <- osm.min.i$area / (res * res) # divide by raster cell area
osm.max.i$area <- st_area(osm.max.i)
osm.max.i$frac <- osm.max.i$area / (res * res) # divide by raster cell area

park.min.i$area <- st_area(park.min.i)
park.min.i$frac <- park.min.i$area / (res * res) # divide by raster cell area
park.max.i$area <- st_area(park.max.i)
park.max.i$frac <- park.max.i$area / (res * res) # divide by raster cell area

# for vehicle travel, calc VMT by trimmed link length * vehicles traversed
veh.i$day.vmt <- st_length(veh.i)  * veh.i$day.flow
veh.i$am.vmt <- st_length(veh.i)  * veh.i$fl.8.9am
veh.i$pm.vmt <- st_length(veh.i)  * veh.i$fl.5.6am

# import SVF data and convert to points from lat lon and transform to EPSG:2223
svf <- readRDS(here("data/phoenix_SVF.rds")) # phoenix sky view factor data (via A. Middel)
svf.pts <- SpatialPointsDataFrame(coords = svf[,.(lon,lat)], data = svf[, .(SVF)], 
                                  proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
svf.pts <- spTransform(svf.pts, crs(my.crs))
svf.pts <- intersect(svf.pts, extent(my.buffer))

# convert clipped spatial parking area to centroids and data 
osm.min.c <- st_centroid(osm.min.i)
osm.max.c <- st_centroid(osm.max.i)

park.min.c <- st_centroid(park.min.i)
park.max.c <- st_centroid(park.max.i)

veh.c <- st_centroid(veh.i)

# convert to sp obj
park.min.p <- as(park.min.c, "Spatial")
park.max.p <- as(park.max.c, "Spatial")

osm.min.p <- as(osm.min.c, "Spatial")
osm.max.p <- as(osm.max.c, "Spatial")

veh.p <- as(veh.c, "Spatial")

# removed unused objects
rm(osm.min.c, osm.max.c, osm.min.i, osm.max.i, 
   park.min.i, park.max.i, park.min.c, park.max.c, 
   veh.i, veh.c, svf)

# split total point features in pavement/parking data into parts for parellel splitting
parts.min.r <- split(1:nrow(osm.min.p[,]), cut(1:nrow(osm.min.p[,]), my.cores))
parts.max.r <- split(1:nrow(osm.max.p[,]), cut(1:nrow(osm.max.p[,]), my.cores))

parts.min.p <- split(1:nrow(park.min.p[,]), cut(1:nrow(park.min.p[,]), my.cores))
parts.max.p <- split(1:nrow(park.max.p[,]), cut(1:nrow(park.max.p[,]), my.cores))

parts.c <- split(1:nrow(veh.p[,]), cut(1:nrow(veh.p[,]), my.cores))

parts.svf <- split(1:nrow(svf.pts[,]), cut(1:nrow(svf.pts[,]), my.cores))

# create temporary output directory
dir.create(here("data/outputs/temp/rasters"), showWarnings = F)

# CREATE FINAL RASTERS OF ROAD/PARK FRACTIONAL AREA AND DAILY VMT
cat(paste0("Time: ", Sys.time(),  # script run time update
           ".\nDuration: ", round(difftime(Sys.time(), script.start, units = "mins")), " mins."))
gc() # initiate cluster after only all the necessary objects present in R environment
while(exists("cl") == F){ # ensures the cluster is made even if fails on first try   
  cl <- makeCluster(my.cores, outfile = "")} # so just while loop until it is sucsessful 
registerDoParallel(cl) # register parallel backend
invisible(clusterCall(cl, function(x) .libPaths(x), .libPaths())) # supress printing

# rasterize in parellel min/max fractional road network area by fclass and save
# based on roadway point summary data clipped by raster cells to ensure no loss of data from getCover
invisible(foreach(i = 1:my.cores, .packages = c("raster", "here")) %dopar% {
  rasterize(osm.min.p[parts.min.r[[i]],], r, field = "frac", fun = sum, background = 0,
            filename = here(paste0("data/outputs/temp/rasters/road-min-part-", i, ".tif")), 
            overwrite = T)
})
invisible(foreach(i = 1:my.cores, .packages = c("raster", "here")) %dopar% {
  rasterize(osm.max.p[parts.max.r[[i]],], r, field = "frac", fun = sum, background = 0,
            filename = here(paste0("data/outputs/temp/rasters/road-max-part-", i, ".tif")), 
            overwrite = T)
})

# PARKING 
# rasterize min/max parking area based on buffered, raster clipped, and centrioded parking area data (adjusted)
invisible(foreach(i = 1:my.cores, .packages = c("raster", "here")) %dopar% {
  rasterize(park.min.p[parts.min.p[[i]],], r, field = "frac", fun = sum, background = 0,
            filename = here(paste0("data/outputs/temp/rasters/park-min-part-", i, ".tif")), 
            overwrite = T)
})
invisible(foreach(i = 1:my.cores, .packages = c("raster", "here")) %dopar% {
  rasterize(park.max.p[parts.max.p[[i]],], r, field = "frac", fun = sum, background = 0,
            filename = here(paste0("data/outputs/temp/rasters/park-max-part-", i, ".tif")), 
            overwrite = T)
})


# VEHICLES
# rasterize VMT based on cleaned icarus clipped links with vehicle travel centrioded
invisible(foreach(i = 1:my.cores, .packages = c("raster", "here")) %dopar% {
  rasterize(veh.p[parts.c[[i]],], r, field = "day.vmt", fun = sum, background = 0,
            filename = here(paste0("data/outputs/temp/rasters/veh-part-", i, ".tif")), 
            overwrite = T)
})

# SVF
# rasterize SVF point data into mean by pixel
invisible(foreach(i = 1:my.cores, .packages = c("raster", "here")) %dopar% {
  rasterize(svf.pts[parts.svf[[i]],], r, field = "SVF", fun = mean, background = NA,
            filename = here(paste0("data/outputs/temp/rasters/svf-part-", i, ".tif")), 
            overwrite = T)
})

# stop cluster
stopCluster(cl)
rm(cl, osm.min.p, osm.max.p, parts.min.r, parts.max.r, 
   park.min.p, park.max.p, parts.min.p, parts.max.p, veh.p, parts.c)

# SUMMARIZE RASTER DATA and CALCULATE HEAT FLUXES
# load pavement and mpg summary data
#fclass.meta <- readRDS(here("data/outputs/pavement-class-summary.rds"))
pave.veh.meta <- readRDS(here("data/outputs/pavement-vehicle-heat-metadata.rds"))

# create list of pavement/parking raster parts from file
r.road.min.p <- lapply(1:my.cores, function (i) raster(here(paste0("data/outputs/temp/rasters/road-min-part-", i, ".tif"))))
r.road.max.p <- lapply(1:my.cores, function (i) raster(here(paste0("data/outputs/temp/rasters/road-max-part-", i, ".tif"))))

r.park.min.p <- lapply(1:my.cores, function (i) raster(here(paste0("data/outputs/temp/rasters/park-min-part-", i, ".tif"))))
r.park.max.p <- lapply(1:my.cores, function (i) raster(here(paste0("data/outputs/temp/rasters/park-max-part-", i, ".tif"))))

r.veh.p <- lapply(1:my.cores, function (i) raster(here(paste0("data/outputs/temp/rasters/veh-part-", i, ".tif"))))

r.svf.p <- lapply(1:my.cores, function (i) raster(here(paste0("data/outputs/temp/rasters/svf-part-", i, ".tif"))))

# merge all raster parts using sum function (function shouldn't really matter here w/ no overlaps)
r.road.min.p$fun <- sum
r.road.max.p$fun <- sum
r.road.min <- do.call(mosaic, r.road.min.p)
r.road.max <- do.call(mosaic, r.road.max.p)

r.park.min.p$fun <- sum
r.park.max.p$fun <- sum
r.park.min <- do.call(mosaic, r.park.min.p)
r.park.max <- do.call(mosaic, r.park.max.p)

r.veh.p$fun <- sum
r.veh <- do.call(mosaic, r.veh.p)

r.svf.p$fun <- sum
r.svf <- do.call(mosaic, r.svf.p)

# make sure that if there are still overlapping of roads, abnormally high parking fractions, or high SVF set max cell value to 1.0
values(r.road.min) <- ifelse(values(r.road.min) > 1.0, 1.0, values(r.road.min))
values(r.road.max) <- ifelse(values(r.road.max) > 1.0, 1.0, values(r.road.max))
values(r.park.min) <- ifelse(values(r.park.min) > 1.0, 1.0, values(r.park.min))
values(r.park.max) <- ifelse(values(r.park.max) > 1.0, 1.0, values(r.park.max))
values(r.svf) <- ifelse(values(r.svf) > 1.0, 1.0, values(r.svf)) # SVF can't be greater than 1.0

# create mean rasters
r.road.avg <- stackApply(stack(r.road.min, r.road.max), indices = c(1), fun = mean)
r.park.avg <- stackApply(stack(r.park.min, r.park.max), indices = c(1), fun = mean)

# parking + roads
r.pave.min <- stackApply(stack(r.road.min, r.park.min), indices = c(1), fun = sum)
r.pave.avg <- stackApply(stack(r.road.avg, r.park.avg), indices = c(1), fun = sum)
r.pave.max <- stackApply(stack(r.road.max, r.park.max), indices = c(1), fun = sum)

# make sure no sums greater than 1.0 coverage for parking + roads
values(r.pave.min) <- ifelse(values(r.pave.min) > 1.0, 1.0, values(r.pave.min)) 
values(r.pave.avg) <- ifelse(values(r.pave.avg) > 1.0, 1.0, values(r.pave.avg))
values(r.pave.max) <- ifelse(values(r.pave.max) > 1.0, 1.0, values(r.pave.max)) 

# make correct units of vmt by dividing result by 5280 (ft per mile), and add log scale vmt
values(r.veh) <- values(r.veh) / 5280 # * 1.60934 # for VKT (vehicle kilometers traveled)
r.veh.log <- r.veh
values(r.veh.log) <- log10(values(r.veh.log) + 1) # log base 10

# plots to check
plot(r.road.avg) # , rev(heat.colors(255))
plot(r.park.avg)
plot(r.pave.avg) 
#plot(r.veh)
plot(r.veh.log)
plot(r.svf)

# create output directory if doesn't exist
dir.create(here("data/outputs/rasters"), showWarnings = F) 

# write out final road sum rasters
writeRaster(r.road.min, here(paste0("data/outputs/rasters/road-min-", run.name, "-", res / 3.28084, "m.tif")), overwrite = T)
writeRaster(r.road.avg, here(paste0("data/outputs/rasters/road-avg-", run.name, "-", res / 3.28084, "m.tif")), overwrite = T)
writeRaster(r.road.max, here(paste0("data/outputs/rasters/road-max-", run.name, "-", res / 3.28084, "m.tif")), overwrite = T)

# write out final park sum rasters
writeRaster(r.park.min, here(paste0("data/outputs/rasters/park-min-", run.name, "-", res / 3.28084, "m.tif")), overwrite = T)
writeRaster(r.park.avg, here(paste0("data/outputs/rasters/park-avg-", run.name, "-", res / 3.28084, "m.tif")), overwrite = T)
writeRaster(r.park.max, here(paste0("data/outputs/rasters/park-max-", run.name, "-", res / 3.28084, "m.tif")), overwrite = T)

# write out final total pave sum rasters
writeRaster(r.pave.min, here(paste0("data/outputs/rasters/pave-min-", run.name, "-", res / 3.28084, "m.tif")), overwrite = T)
writeRaster(r.pave.avg, here(paste0("data/outputs/rasters/pave-avg-", run.name, "-", res / 3.28084, "m.tif")), overwrite = T)
writeRaster(r.pave.max, here(paste0("data/outputs/rasters/pave-max-", run.name, "-", res / 3.28084, "m.tif")), overwrite = T)

# write out final total car vmt raster
writeRaster(r.veh, here(paste0("data/outputs/rasters/vmt-", run.name, "-", res / 3.28084, "m.tif")), overwrite = T)
writeRaster(r.veh.log, here(paste0("data/outputs/rasters/log-vmt-", run.name, "-", res / 3.28084, "m.tif")), overwrite = T)

# paste final runtime
paste0("R model run complete on ", Sys.info()[4]," at ", Sys.time(),
       ". Model run length: ", round(difftime(Sys.time(), script.start, units = "mins"),2)," mins.")

