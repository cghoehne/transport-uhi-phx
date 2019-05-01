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
#run.name <- "metro-phx"
#run.name <- "phx-dwntwn"
run.name <- "north-tempe"

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
res <- 820.21  # ~250m x 250m
#res <- 1640.42 # ~500m x 500m
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

# assign aggregated class names for assigning pavement heat model data
fclass.meta <- readRDS(here("data/outputs/pavement-class-summary.rds"))
for(f in 1:4){
  osm.min.p$class[osm.min.p$fclass %in% fclass.meta[[f]]$class] <- names(fclass.meta)[f]
  osm.max.p$class[osm.max.p$fclass %in% fclass.meta[[f]]$class] <- names(fclass.meta)[f]
}

# drop NA agg roadway classess (non-roadway)
osm.min.p <- osm.min.p[!is.na(osm.min.p$class),]
osm.max.p <- osm.max.p[!is.na(osm.max.p$class),]

# split up so we can aggregate to raster layer for each sub class seperately 
osm.min.p.local <- osm.min.p[osm.min.p$class == unique(osm.min.p$class)[1],]
osm.min.p.minor <- osm.min.p[osm.min.p$class == unique(osm.min.p$class)[2],]
osm.min.p.major <- osm.min.p[osm.min.p$class == unique(osm.min.p$class)[3],]
osm.min.p.hiway <- osm.min.p[osm.min.p$class == unique(osm.min.p$class)[4],]

osm.max.p.local <- osm.max.p[osm.max.p$class == unique(osm.max.p$class)[1],]
osm.max.p.minor <- osm.max.p[osm.max.p$class == unique(osm.max.p$class)[2],]
osm.max.p.major <- osm.max.p[osm.max.p$class == unique(osm.max.p$class)[3],]
osm.max.p.hiway <- osm.max.p[osm.max.p$class == unique(osm.max.p$class)[4],]

park.min.p.asph <- park.min.p[park.min.p$type == unique(park.min.p$type)[1],]
park.min.p.conc <- park.min.p[park.min.p$type == unique(park.min.p$type)[2],]

park.max.p.asph <- park.max.p[park.max.p$type == unique(park.max.p$type)[1],]
park.max.p.conc <- park.max.p[park.max.p$type == unique(park.max.p$type)[2],]

# split total point features in pavement/parking data into parts for parellel splitting
parts.min.r.local <- split(1:nrow(osm.min.p.local[,]), cut(1:nrow(osm.min.p.local[,]), my.cores))
parts.min.r.minor <- split(1:nrow(osm.min.p.minor[,]), cut(1:nrow(osm.min.p.minor[,]), my.cores))
parts.min.r.major <- split(1:nrow(osm.min.p.major[,]), cut(1:nrow(osm.min.p.major[,]), my.cores))
parts.min.r.hiway <- split(1:nrow(osm.min.p.hiway[,]), cut(1:nrow(osm.min.p.hiway[,]), my.cores))

parts.max.r.local <- split(1:nrow(osm.max.p.local[,]), cut(1:nrow(osm.max.p.local[,]), my.cores))
parts.max.r.minor <- split(1:nrow(osm.max.p.minor[,]), cut(1:nrow(osm.max.p.minor[,]), my.cores))
parts.max.r.major <- split(1:nrow(osm.max.p.major[,]), cut(1:nrow(osm.max.p.major[,]), my.cores))
parts.max.r.hiway <- split(1:nrow(osm.max.p.hiway[,]), cut(1:nrow(osm.max.p.hiway[,]), my.cores))

parts.min.p.asph <- split(1:nrow(park.min.p.asph[,]), cut(1:nrow(park.min.p.asph[,]), my.cores))
parts.min.p.conc <- split(1:nrow(park.min.p.conc[,]), cut(1:nrow(park.min.p.conc[,]), my.cores))

parts.max.p.asph <- split(1:nrow(park.max.p.asph[,]), cut(1:nrow(park.max.p.asph[,]), my.cores))
parts.max.p.conc <- split(1:nrow(park.max.p.conc[,]), cut(1:nrow(park.max.p.conc[,]), my.cores))

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

# min osm by fclass
invisible(foreach(i = 1:my.cores, .packages = c("raster", "here")) %dopar% {
  rasterize(osm.min.p.local[parts.min.r.local[[i]],], r, field = "frac", fun = sum, background = 0,
            filename = here(paste0("data/outputs/temp/rasters/road-min-part-local-", i, ".tif")), 
            overwrite = T)
})
invisible(foreach(i = 1:my.cores, .packages = c("raster", "here")) %dopar% {
  rasterize(osm.min.p.minor[parts.min.r.minor[[i]],], r, field = "frac", fun = sum, background = 0,
            filename = here(paste0("data/outputs/temp/rasters/road-min-part-minor-", i, ".tif")), 
            overwrite = T)
})
invisible(foreach(i = 1:my.cores, .packages = c("raster", "here")) %dopar% {
  rasterize(osm.min.p.major[parts.min.r.major[[i]],], r, field = "frac", fun = sum, background = 0,
            filename = here(paste0("data/outputs/temp/rasters/road-min-part-major-", i, ".tif")), 
            overwrite = T)
})
invisible(foreach(i = 1:my.cores, .packages = c("raster", "here")) %dopar% {
  rasterize(osm.min.p.hiway[parts.min.r.hiway[[i]],], r, field = "frac", fun = sum, background = 0,
            filename = here(paste0("data/outputs/temp/rasters/road-min-part-hiway-", i, ".tif")), 
            overwrite = T)
})

# max osm by fclass
invisible(foreach(i = 1:my.cores, .packages = c("raster", "here")) %dopar% {
  rasterize(osm.max.p.local[parts.max.r.local[[i]],], r, field = "frac", fun = sum, background = 0,
            filename = here(paste0("data/outputs/temp/rasters/road-max-part-local-", i, ".tif")), 
            overwrite = T)
})
invisible(foreach(i = 1:my.cores, .packages = c("raster", "here")) %dopar% {
  rasterize(osm.max.p.minor[parts.max.r.minor[[i]],], r, field = "frac", fun = sum, background = 0,
            filename = here(paste0("data/outputs/temp/rasters/road-max-part-minor-", i, ".tif")), 
            overwrite = T)
})
invisible(foreach(i = 1:my.cores, .packages = c("raster", "here")) %dopar% {
  rasterize(osm.max.p.major[parts.max.r.major[[i]],], r, field = "frac", fun = sum, background = 0,
            filename = here(paste0("data/outputs/temp/rasters/road-max-part-major-", i, ".tif")), 
            overwrite = T)
})
invisible(foreach(i = 1:my.cores, .packages = c("raster", "here")) %dopar% {
  rasterize(osm.max.p.hiway[parts.max.r.hiway[[i]],], r, field = "frac", fun = sum, background = 0,
            filename = here(paste0("data/outputs/temp/rasters/road-max-part-hiway-", i, ".tif")), 
            overwrite = T)
})


# PARKING 
# rasterize min/max parking area based on buffered, raster clipped, and centrioded parking area data (adjusted)

# min parking by type
invisible(foreach(i = 1:my.cores, .packages = c("raster", "here")) %dopar% {
  rasterize(park.min.p.asph[parts.min.p.asph[[i]],], r, field = "frac", fun = sum, background = 0,
            filename = here(paste0("data/outputs/temp/rasters/park-min-part-asph-", i, ".tif")), 
            overwrite = T)
})
invisible(foreach(i = 1:my.cores, .packages = c("raster", "here")) %dopar% {
  rasterize(park.min.p.conc[parts.min.p.conc[[i]],], r, field = "frac", fun = sum, background = 0,
            filename = here(paste0("data/outputs/temp/rasters/park-min-part-conc-", i, ".tif")), 
            overwrite = T)
})

# max parking by type
invisible(foreach(i = 1:my.cores, .packages = c("raster", "here")) %dopar% {
  rasterize(park.max.p.asph[parts.max.p.asph[[i]],], r, field = "frac", fun = sum, background = 0,
            filename = here(paste0("data/outputs/temp/rasters/park-max-part-asph-", i, ".tif")), 
            overwrite = T)
})
invisible(foreach(i = 1:my.cores, .packages = c("raster", "here")) %dopar% {
  rasterize(park.max.p.conc[parts.max.p.conc[[i]],], r, field = "frac", fun = sum, background = 0,
            filename = here(paste0("data/outputs/temp/rasters/park-max-part-conc-", i, ".tif")), 
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
            na.rm = T, overwrite = T)
})

# stop cluster
stopCluster(cl)
rm(cl, veh.m, osm.min.p, osm.max.p, parts.min.r, parts.max.r, 
   park.min.p.asph, park.min.p.conc, park.max.p.asph, park.max.p.conc, 
   parts.min.r.hiway, parts.min.r.local, parts.min.r.major, parts.min.r.minor, 
   parts.max.r.hiway, parts.max.r.local, parts.max.r.major, parts.max.r.minor,
   veh.p, parts.c)
gc()

# SUMMARIZE RASTER DATA

# create list of pavement/parking raster parts from file
r.road.min.p.local <- lapply(1:my.cores, function (i) raster(here(paste0("data/outputs/temp/rasters/road-min-part-local-", i, ".tif"))))
r.road.min.p.minor <- lapply(1:my.cores, function (i) raster(here(paste0("data/outputs/temp/rasters/road-min-part-minor-", i, ".tif"))))
r.road.min.p.major <- lapply(1:my.cores, function (i) raster(here(paste0("data/outputs/temp/rasters/road-min-part-major-", i, ".tif"))))
r.road.min.p.hiway <- lapply(1:my.cores, function (i) raster(here(paste0("data/outputs/temp/rasters/road-min-part-hiway-", i, ".tif"))))

r.road.max.p.local <- lapply(1:my.cores, function (i) raster(here(paste0("data/outputs/temp/rasters/road-max-part-local-", i, ".tif"))))
r.road.max.p.minor <- lapply(1:my.cores, function (i) raster(here(paste0("data/outputs/temp/rasters/road-max-part-minor-", i, ".tif"))))
r.road.max.p.major <- lapply(1:my.cores, function (i) raster(here(paste0("data/outputs/temp/rasters/road-max-part-major-", i, ".tif"))))
r.road.max.p.hiway <- lapply(1:my.cores, function (i) raster(here(paste0("data/outputs/temp/rasters/road-max-part-hiway-", i, ".tif"))))

r.park.min.p.asph <- lapply(1:my.cores, function (i) raster(here(paste0("data/outputs/temp/rasters/park-min-part-asph-", i, ".tif"))))
r.park.min.p.conc <- lapply(1:my.cores, function (i) raster(here(paste0("data/outputs/temp/rasters/park-min-part-conc-", i, ".tif"))))

r.park.max.p.asph <- lapply(1:my.cores, function (i) raster(here(paste0("data/outputs/temp/rasters/park-max-part-asph-", i, ".tif"))))
r.park.max.p.conc <- lapply(1:my.cores, function (i) raster(here(paste0("data/outputs/temp/rasters/park-max-part-conc-", i, ".tif"))))

r.veh.p <- lapply(1:my.cores, function (i) raster(here(paste0("data/outputs/temp/rasters/veh-part-", i, ".tif"))))

r.svf.p <- lapply(1:my.cores, function (i) raster(here(paste0("data/outputs/temp/rasters/svf-part-", i, ".tif"))))

# merge all raster parts using sum function (function shouldn't really matter here w/ no overlaps)
r.road.min.p.local$fun <- sum
r.road.min.p.minor$fun <- sum
r.road.min.p.major$fun <- sum
r.road.min.p.hiway$fun <- sum
r.road.min.local <- do.call(mosaic, r.road.min.p.local)
r.road.min.minor <- do.call(mosaic, r.road.min.p.minor)
r.road.min.major <- do.call(mosaic, r.road.min.p.major)
r.road.min.hiway <- do.call(mosaic, r.road.min.p.hiway)

r.road.max.p.local$fun <- sum
r.road.max.p.minor$fun <- sum
r.road.max.p.major$fun <- sum
r.road.max.p.hiway$fun <- sum
r.road.max.local <- do.call(mosaic, r.road.max.p.local)
r.road.max.minor <- do.call(mosaic, r.road.max.p.minor)
r.road.max.major <- do.call(mosaic, r.road.max.p.major)
r.road.max.hiway <- do.call(mosaic, r.road.max.p.hiway)

r.park.min.p.asph$fun <- sum
r.park.min.p.conc$fun <- sum
r.park.min.asph <- do.call(mosaic, r.park.min.p.asph)
r.park.min.conc <- do.call(mosaic, r.park.min.p.conc)

r.park.max.p.asph$fun <- sum
r.park.max.p.conc$fun <- sum
r.park.max.asph <- do.call(mosaic, r.park.max.p.asph)
r.park.max.conc <- do.call(mosaic, r.park.max.p.conc)

r.veh.p$fun <- sum
r.veh <- do.call(mosaic, r.veh.p)

r.svf.p$fun <- mean
r.svf <- do.call(mosaic, r.svf.p)

# because mean of empty numeric gives NaN. We will assume the mean of the region to replace these areas
mean.svf <- mean(ifelse(is.nan(values(r.svf)) == T, NA, values(r.svf)), na.rm = T) # NaN to NA, then na.rm = T
values(r.svf) <- ifelse(is.na(values(r.svf)) == T, mean.svf, values(r.svf))

# stack rasters such that each layer is a class for roads and parking, add total roads/parking fractions
r.road.min <- stack(r.road.min.local, r.road.min.minor, r.road.min.major, r.road.min.hiway) # stack min roads fractions by class
r.road.min <- stack(r.road.min, stackApply(r.road.min, indices = c(1), fun = sum)) # create all roads sum for min scenario

r.road.max <- stack(r.road.max.local, r.road.max.minor, r.road.max.major, r.road.max.hiway) # stack max roads fractions by class
r.road.max <- stack(r.road.max, stackApply(r.road.max, indices = c(1), fun = sum)) # create all roads sum for max scenario

r.road <- stack(r.road.min, r.road.max) # combine min and max fractional areas into raster stack
r.road <- stack(r.road, stackApply(r.road, indices = c(1:5,1:5), fun = mean)) # add the mean by fclass
names(r.road) <- c("min.local", "min.minor", "min.major", "min.hiway", "min.roads", # names in order of bindings
                       "max.local", "max.minor", "max.major", "max.hiway", "max.roads",
                       "avg.local", "avg.minor", "avg.major", "avg.hiway", "avg.roads")

r.park.min <- stack(r.park.min.asph, r.park.min.conc) # stack min parking fractions by class
r.park.min <- stack(r.park.min, stackApply(r.park.min, indices = c(1), fun = sum)) # create all roads sum for min scenario

r.park.max <- stack(r.park.max.asph, r.park.max.conc) # stack max parking fractions by class
r.park.max <- stack(r.park.max, stackApply(r.park.max, indices = c(1), fun = sum)) # create all roads sum for max scenario

r.park <- stack(r.park.min, r.park.max) # combine min and max fractional areas into raster stack
r.park <- stack(r.park, stackApply(r.park, indices = c(1:3,1:3), fun = mean)) # add the mean by fclass
names(r.park) <- c("min.com.park", "min.res.park", "min.park",
                   "max.com.park", "max.res.park", "max.park",
                   "avg.res.park", "avg.com.park", "avg.park")

# pavement: parking + roads
r.pave <- stack(r.road, r.park, stackApply(stack(r.road[[c("min.roads","max.roads","avg.roads")]],
                                                 r.park[[c("min.park","max.park","avg.park")]]),
                                           indices = c(1:3,1:3), fun = sum))
names(r.pave)[25:27] <- c("min.pave", "max.pave", "avg.pave") # add names
values(r.pave$max.pave) <- ifelse(values(r.pave$max.pave) > 1.0, 1.0, values(r.pave$max.pave)) # make sure no > 1.0 for max pave

# make correct units of vmt by dividing result by 5280 (ft per mile), and add log scale vmt
values(r.veh) <- values(r.veh) / 5280  * 1.60934 # for VKT (vehicle kilometers traveled)
r.veh.log <- r.veh
values(r.veh.log) <- log10(values(r.veh.log) + 1) # log base 10

# add vehicle and svf to master raster
r.all <- stack(r.pave, r.veh, r.veh.log, r.svf)
names(r.all)[28:30] <- c("VKT", "log.VKT", "SVF")

# CALCULATE HEAT FLUXES
# load pavement and mpg summary data
pave.veh.meta <- readRDS(here("data/outputs/pavement-vehicle-heat-metadata.rds"))

# adjust SVF such that there is a floor SVF b/c heat model simplifies incoming radiation multiplying by SVF
# so a 0 or very low SVF is impractical to use with this idealized model
# therefore we assume that even under near 100% shading there is some diffuse and infared radiation that reaches the surface
r.all$adj.SVF <- r.all$SVF
values(r.all$adj.SVF) <- ifelse(values(r.all$adj.SVF) < 0.5, 0.5, values(r.all$adj.SVF))
r.all$adj.SVF <- (2 * r.all$adj.SVF) - 1 # adjustment for 100% SVF and 50% SVF instead of 100/0  

# fractional area * (heat flux for pavement class (W/m2) - unpaved scenario) = w/m2 from pave in raster cell
# min roads heat flux
r.all$min.day.flux.local <- (r.all$min.local 
                             * (((pave.veh.meta[pave.class == "local" & SVF == 1.0, mean.day.out.flux.lwr] 
                                  - pave.veh.meta[pave.class == "unpaved" & SVF == 1.0, mean.day.out.flux.lwr]) * r.all$adj.SVF)
                                + ((pave.veh.meta[pave.class == "local" & SVF == 0.5, mean.day.out.flux.lwr]
                                   - pave.veh.meta[pave.class == "unpaved" & SVF == 0.5, mean.day.out.flux.lwr]) * (1 - r.all$adj.SVF))))
r.all$min.day.flux.minor <- (r.all$min.minor 
                             * (((pave.veh.meta[pave.class == "minor" & SVF == 1.0, mean.day.out.flux.lwr]
                                 - pave.veh.meta[pave.class == "unpaved" & SVF == 1.0, mean.day.out.flux.lwr]) * r.all$adj.SVF)
                                + ((pave.veh.meta[pave.class == "minor" & SVF == 0.5, mean.day.out.flux.lwr]
                                   - pave.veh.meta[pave.class == "unpaved" & SVF == 0.5, mean.day.out.flux.lwr]) * (1 - r.all$adj.SVF))))
r.all$min.day.flux.major <- (r.all$min.major 
                             * (((pave.veh.meta[pave.class == "major" & SVF == 1.0, mean.day.out.flux.lwr]
                                 - pave.veh.meta[pave.class == "unpaved" & SVF == 1.0, mean.day.out.flux.lwr]) * r.all$adj.SVF)
                                + ((pave.veh.meta[pave.class == "major" & SVF == 0.5, mean.day.out.flux.lwr]
                                   - pave.veh.meta[pave.class == "unpaved" & SVF == 0.5, mean.day.out.flux.lwr]) * (1 - r.all$adj.SVF))))
r.all$min.day.flux.hiway <- (r.all$min.hiway 
                             * (((pave.veh.meta[pave.class == "highway" & SVF == 1.0, mean.day.out.flux.lwr]
                                 - pave.veh.meta[pave.class == "unpaved" & SVF == 1.0, mean.day.out.flux.lwr]) * r.all$adj.SVF)
                                + ((pave.veh.meta[pave.class == "highway" & SVF == 0.5, mean.day.out.flux.lwr]
                                   - pave.veh.meta[pave.class == "unpaved" & SVF == 0.5, mean.day.out.flux.lwr]) * (1 - r.all$adj.SVF))))

r.all <- stack(r.all, stackApply(r.all[[c("min.day.flux.local", "min.day.flux.minor", "min.day.flux.major", "min.day.flux.hiway")]], 
                                 indices = c(1), fun = sum))
names(r.all[[nlayers(r.all)]]) <- "min.day.flux.roads"

# max roads heat flux
r.all$max.day.flux.local <- (r.all$max.local 
                             * (((pave.veh.meta[pave.class == "local" & SVF == 1.0, mean.day.out.flux.upr]
                                 - pave.veh.meta[pave.class == "unpaved" & SVF == 1.0, mean.day.out.flux.upr]) * r.all$adj.SVF)
                                + ((pave.veh.meta[pave.class == "local" & SVF == 0.5, mean.day.out.flux.upr]
                                   - pave.veh.meta[pave.class == "unpaved" & SVF == 0.5, mean.day.out.flux.upr])* (1 - r.all$adj.SVF))))
r.all$max.day.flux.minor <- (r.all$max.minor 
                             * (((pave.veh.meta[pave.class == "minor" & SVF == 1.0, mean.day.out.flux.upr]
                                 - pave.veh.meta[pave.class == "unpaved" & SVF == 1.0, mean.day.out.flux.upr]) * r.all$adj.SVF)
                                + ((pave.veh.meta[pave.class == "minor" & SVF == 0.5, mean.day.out.flux.upr]
                                   - pave.veh.meta[pave.class == "unpaved" & SVF == 0.5, mean.day.out.flux.upr])* (1 - r.all$adj.SVF))))
r.all$max.day.flux.major <- (r.all$max.major 
                             * (((pave.veh.meta[pave.class == "major" & SVF == 1.0, mean.day.out.flux.upr]
                                 - pave.veh.meta[pave.class == "unpaved" & SVF == 1.0, mean.day.out.flux.upr]) * r.all$adj.SVF)
                                + ((pave.veh.meta[pave.class == "major" & SVF == 0.5, mean.day.out.flux.upr]
                                   - pave.veh.meta[pave.class == "unpaved" & SVF == 0.5, mean.day.out.flux.upr]) * (1 - r.all$adj.SVF))))
r.all$max.day.flux.hiway <- (r.all$max.hiway 
                             * (((pave.veh.meta[pave.class == "highway" & SVF == 1.0, mean.day.out.flux.upr]
                                 - pave.veh.meta[pave.class == "unpaved" & SVF == 1.0, mean.day.out.flux.upr]) * r.all$adj.SVF)
                                + ((pave.veh.meta[pave.class == "highway" & SVF == 0.5, mean.day.out.flux.upr]
                                   - pave.veh.meta[pave.class == "unpaved" & SVF == 0.5, mean.day.out.flux.upr])* (1 - r.all$adj.SVF))))

r.all <- stack(r.all, stackApply(r.all[[c("max.day.flux.local", "max.day.flux.minor", "max.day.flux.major", "max.day.flux.hiway")]], 
                                 indices = c(1), fun = sum))
names(r.all[[nlayers(r.all)]]) <- "max.day.flux.roads"

# avg flux roads
r.all <- stack(r.all, stackApply(r.all[[c("min.day.flux.roads", "max.day.flux.roads")]], indices = c(1), fun = mean))
names(r.all[[nlayers(r.all)]]) <- "avg.day.flux.roads"

# parking
# min roads heat flux
r.all$min.day.flux.com.park <- (r.all$min.com.park 
                             * (((pave.veh.meta[pave.class == "com.park" & SVF == 1.0, mean.day.out.flux.lwr] 
                                  - pave.veh.meta[pave.class == "unpaved" & SVF == 1.0, mean.day.out.flux.lwr]) * r.all$adj.SVF)
                                + ((pave.veh.meta[pave.class == "com.park" & SVF == 0.5, mean.day.out.flux.lwr]
                                    - pave.veh.meta[pave.class == "unpaved" & SVF == 0.5, mean.day.out.flux.lwr]) * (1 - r.all$adj.SVF))))
r.all$min.day.flux.res.park <- (r.all$min.res.park 
                             * (((pave.veh.meta[pave.class == "res.park" & SVF == 1.0, mean.day.out.flux.lwr]
                                  - pave.veh.meta[pave.class == "unpaved" & SVF == 1.0, mean.day.out.flux.lwr]) * r.all$adj.SVF)
                                + ((pave.veh.meta[pave.class == "res.park" & SVF == 0.5, mean.day.out.flux.lwr]
                                    - pave.veh.meta[pave.class == "unpaved" & SVF == 0.5, mean.day.out.flux.lwr]) * (1 - r.all$adj.SVF))))

r.all <- stack(r.all, stackApply(r.all[[c("min.day.flux.com.park", "min.day.flux.res.park")]], 
                                 indices = c(1), fun = sum))
names(r.all[[nlayers(r.all)]]) <- "min.day.flux.park"

# max roads heat flux
r.all$max.day.flux.com.park <- (r.all$max.com.park 
                             * (((pave.veh.meta[pave.class == "com.park" & SVF == 1.0, mean.day.out.flux.upr]
                                  - pave.veh.meta[pave.class == "unpaved" & SVF == 1.0, mean.day.out.flux.upr]) * r.all$adj.SVF)
                                + ((pave.veh.meta[pave.class == "com.park" & SVF == 0.5, mean.day.out.flux.upr]
                                    - pave.veh.meta[pave.class == "unpaved" & SVF == 0.5, mean.day.out.flux.upr])* (1 - r.all$adj.SVF))))
r.all$max.day.flux.res.park  <- (r.all$max.res.park 
                             * (((pave.veh.meta[pave.class == "res.park" & SVF == 1.0, mean.day.out.flux.upr]
                                  - pave.veh.meta[pave.class == "unpaved" & SVF == 1.0, mean.day.out.flux.upr]) * r.all$adj.SVF)
                                + ((pave.veh.meta[pave.class == "res.park" & SVF == 0.5, mean.day.out.flux.upr]
                                    - pave.veh.meta[pave.class == "unpaved" & SVF == 0.5, mean.day.out.flux.upr])* (1 - r.all$adj.SVF))))

r.all <- stack(r.all, stackApply(r.all[[c("max.day.flux.com.park", "max.day.flux.res.park")]], 
                                 indices = c(1), fun = sum))
names(r.all[[nlayers(r.all)]]) <- "max.day.flux.park"

# avg flux roads
r.all <- stack(r.all, stackApply(r.all[[c("min.day.flux.park", "max.day.flux.park")]], indices = c(1), fun = mean))
names(r.all[[nlayers(r.all)]]) <- "avg.day.flux.park"

#plot(r.all[[c("min.day.flux.park", "max.day.flux.park")]])

# vehicles
# min roads heat flux
# assume 9,500 Wh/liter
# 1 MPG = 0.425144 km/liter or 1 gal/mi = 2.35215 liter/km
# energy lost to waste heat assumed 0.30 to 0.80
pave.veh.meta[,energy.min := (1/MPGe.min) * 2.35215 * 9500 * 0.30] # Wh/km (Watt-hours per kilometer)
pave.veh.meta[,energy.max := (1/MPGe.max) * 2.35215 * 9500 * 0.80] # Wh/km (Watt-hours per kilometer)

# heat flux from vehicles in a day = VKT (km) * energy rate (Wh/km) / hours (hrs) / cell resolution (m2) = W/m2
# min veh flux/day
r.all$min.day.flux.local.veh <- (r.all$VKT  * pave.veh.meta[pave.class == "local" & SVF == 1.0, energy.min] / 24 / ((res / 3.28084)^2))
r.all$min.day.flux.minor.veh <- (r.all$VKT  * pave.veh.meta[pave.class == "minor" & SVF == 1.0, energy.min] / 24 / ((res / 3.28084)^2))
r.all$min.day.flux.major.veh <- (r.all$VKT  * pave.veh.meta[pave.class == "major" & SVF == 1.0, energy.min] / 24 / ((res / 3.28084)^2))
r.all$min.day.flux.hiway.veh <- (r.all$VKT  * pave.veh.meta[pave.class == "highway" & SVF == 1.0, energy.min] / 24 / ((res / 3.28084)^2))

r.all <- stack(r.all, stackApply(r.all[[c("min.day.flux.local.veh", "min.day.flux.minor.veh", "min.day.flux.major.veh", "min.day.flux.hiway.veh")]], 
                                 indices = c(1), fun = sum))
names(r.all[[nlayers(r.all)]]) <- "min.day.flux.veh"

# max veh flux/day
r.all$max.day.flux.local.veh <- (r.all$VKT  * pave.veh.meta[pave.class == "local" & SVF == 1.0, energy.max] / 24 / ((res / 3.28084)^2))
r.all$max.day.flux.minor.veh <- (r.all$VKT  * pave.veh.meta[pave.class == "minor" & SVF == 1.0, energy.max] / 24 / ((res / 3.28084)^2))
r.all$max.day.flux.major.veh <- (r.all$VKT  * pave.veh.meta[pave.class == "major" & SVF == 1.0, energy.max] / 24 / ((res / 3.28084)^2))
r.all$max.day.flux.hiway.veh <- (r.all$VKT  * pave.veh.meta[pave.class == "highway" & SVF == 1.0, energy.max] / 24 / ((res / 3.28084)^2))

r.all <- stack(r.all, stackApply(r.all[[c("max.day.flux.local.veh", "max.day.flux.minor.veh", "max.day.flux.major.veh", "max.day.flux.hiway.veh")]], 
                                 indices = c(1), fun = sum))
names(r.all[[nlayers(r.all)]]) <- "max.day.flux.veh"

# avg flux vehicles
r.all <- stack(r.all, stackApply(r.all[[c("min.day.flux.veh", "max.day.flux.veh")]], indices = c(1), fun = mean))
names(r.all[[nlayers(r.all)]]) <- "avg.day.flux.veh"
#plot(r.all$avg.day.flux.veh)

# min/avg/max total flux
r.all <- stack(r.all, stackApply(r.all[[c("min.day.flux.roads","min.day.flux.park","min.day.flux.veh",
                                          "avg.day.flux.roads","avg.day.flux.park","avg.day.flux.veh",
                                          "max.day.flux.roads", "max.day.flux.park", "max.day.flux.veh")]], 
                                 indices = c(1,1,1,2,2,2,3,3,3), fun = sum))
names(r.all)[(nlayers(r.all)-2):nlayers(r.all)] <- c("total.min.day.flux", "total.avg.day.flux", "total.max.day.flux")

#plot(r.all[[c("total.min.day.flux", "total.avg.day.flux", "total.avg.day.flux")]])
plot(r.all$total.avg.day.flux)
plot(r.all[[c("avg.roads", "avg.park", "VKT", "total.avg.day.flux")]])
plot(r.all[[c("min.day.flux.veh", "min.day.flux.park", "min.day.flux.roads", "total.min.day.flux")]])
plot(r.all[[c("avg.day.flux.veh", "avg.day.flux.park", "avg.day.flux.roads", "total.avg.day.flux")]])
plot(r.all[[c("max.day.flux.veh", "max.day.flux.park", "max.day.flux.roads", "total.max.day.flux")]])

mean(values(r.all$total.avg.day.flux)) # mean W/m2 
options(scipen = 999)
quants <- lapply(1:length(names(r.all)), function(i) quantile(values(
  r.all[[i]]), c(0, 0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99, 1.0), na.rm = T))
names(quants) <- names(r.all)

quants

# create output directory if doesn't exist
dir.create(here("data/outputs/rasters"), showWarnings = F) 

# write out final road sum rasters
writeRaster(r.all, here(paste0("data/outputs/rasters/all-pave-veh-heat", run.name, "-", res / 3.28084, "m.tif")), overwrite = T)

# paste final runtime
paste0("R model run complete on ", Sys.info()[4]," at ", Sys.time(),
       ". Model run length: ", round(difftime(Sys.time(), script.start, units = "mins"),2)," mins.")

