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

# remove tunnel and oneway as no longer needed
osm$tunnel <- NULL
osm$oneway <- NULL

# transform osm crs to EPSG:2223
osm <- spTransform(osm, crs(my.crs))

# clip osm data to my buffer for quicker computing
osm.sf <- st_as_sf(osm)
osm <- st_intersection(osm.sf, my.buffer$geometry)
osm <- as(osm, "Spatial")

# store the number of cores
my.cores <- parallel::detectCores() - 1 # n-1 for headspace

# create empty list for foreach computations
my.list <- list() 

# create list of unique min/max buffer widths based on oneway and estiamted roadway width
w.min <- unique(osm$min.r.buf) 
w.max <- unique(osm$max.r.buf)  

save.image(here("data/outputs/temp/rasterize-1.RData")) # save progress in case error in parellelization
rm(osm.sf, osm.dt, fclass.info, parking, min.shade.ratio, max.shade.ratio)
gc() # initiate cluster after only all the necessary objects present in R environment
cat(paste0("Time: ", Sys.time(),  # script run time update
           ".\nDuration: ", round(difftime(Sys.time(), script.start, units = "mins")), " mins."))
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
gc()

# bind the buffered osm data output
osm.buf.mrg.min <- do.call(raster::bind, osm.buf.min) # bind list of spatial objects into single spatial obj
osm.buf.mrg.max <- do.call(raster::bind, osm.buf.max) # bind list of spatial objects into single spatial obj

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
save.image(here("data/outputs/temp/rasterize-2.RData")) # save progress in case error in parellelization
rm(osm.buf.min, osm.buf.max) # remove unused objects
gc() # initiate cluster after only all the necessary objects present in R environment
cat(paste0("Time: ", Sys.time(),  # script run time update
           ".\nDuration: ", round(difftime(Sys.time(), script.start, units = "mins")), " mins."))
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
gc()

# bind the buffered parking data output
park.buf.min <- do.call(raster::bind, park.buf.min.p) # bind list of spatial objects into single spatial obj
park.buf.max <- do.call(raster::bind, park.buf.max.p) # bind list of spatial objects into single spatial obj

# actual areas (use to check accuracy)
#park.buf.min$min.area.act <- gArea(park.buf.min, byid = T)
#park.buf.max$max.area.act <- gArea(park.buf.max, byid = T)

# convert to sf objects for quicker computing (sf::st_intersection is fastest for zonal intersections)
park.min.s <- st_as_sf(park.buf.min)
park.max.s <- st_as_sf(park.buf.max)

# split total buffered parking data into parts for parellel splitting for clipping to my extent
parts.p.min <- split(1:nrow(park.min.s[,]), cut(1:nrow(park.min.s[,]), my.cores))
parts.p.max <- split(1:nrow(park.max.s[,]), cut(1:nrow(park.max.s[,]), my.cores))

# CLIP PARKING AREA DATA TO DESIRED (NON-BUFFERED) EXTENT
save.image(here("data/outputs/temp/rasterize-3.RData")) # save progress in case error in parellelization
rm(park.buf.min, park.buf.max, park.buf.min.p, park.buf.max.p) # remove unused obj
gc() # initiate cluster after only all the necessary objects present in R environment
cat(paste0("Time: ", Sys.time(),  # script run time update
           ".\nDuration: ", round(difftime(Sys.time(), script.start, units = "mins")), " mins."))
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
gc()

# bind the clipped-to-extent parking data output (in sf still)
park.min.i.u <- do.call(rbind, park.min.i.u.p) # bind list of spatial objects into single spatial obj
park.max.i.u <- do.call(rbind, park.max.i.u.p) # bind list of spatial objects into single spatial obj


# RASTERIZE DATA

# polygonize raster to clip other spatial data by to eventually rasterize 
r.p <- rasterToPolygons(r)
r.t <- st_as_sf(r.p) # sf object for quicker intersections

# load vehicle travel network and data and convert to sf
traffic.net <- readRDS(here("data/outputs/network/icarus-network.rds")) # cleaned/simplified ICARUS traffic network to pair with traffic data
veh <- st_as_sf(traffic.net)

# calculate acutal link length and length reduction ratio to ensure accurate VKT estaimtes
veh$adj.length <- st_length(veh)
veh$adj.l.ratio <- veh$length / veh$adj.length 
#quantile(veh$adj.l.ratio, c(0, 0.50, 0.75, 0.90, 0.95, 0.99, 0.999, 0.9999, 1.0))

# ICARUS traffic data has 101 cols where col 1 is link id and the rest are from 12am to 12am divided in 100 chunks
# therefore every column from V2:V101 represents a 0.24 hour period
# morning rush would be from: hour = 7.92 am to 8.88 am (6:54:12 am to 8:52:48 am); V34:V37
# evening rush would be from: hour = 5.04 pm  to 6 pm (5:02:24 pm to 6:00:00 pm); V72:V75

# merge icarus flow data by link id
iflow <- fread(here("data/icarus/full_flow.csv"))
setnames(iflow, "V1", "id")
veh.m <- merge(veh, iflow, by = "id")

# sf objects for quicker intersections
osm.min.s <- st_as_sf(osm.buf.mrg.min)
osm.max.s <- st_as_sf(osm.buf.mrg.max)

# split total osm, parking, and veh data into parts for parellel splitting
parts.r.min <- split(1:nrow(osm.min.s[,]), cut(1:nrow(osm.min.s[,]), my.cores))
parts.r.max <- split(1:nrow(osm.max.s[,]), cut(1:nrow(osm.max.s[,]), my.cores))

parts.p.min <- split(1:nrow(park.min.i.u[,]), cut(1:nrow(park.min.i.u[,]), my.cores))
parts.p.max <- split(1:nrow(park.max.i.u[,]), cut(1:nrow(park.max.i.u[,]), my.cores))

parts.c <- split(1:nrow(veh.m[,]), cut(1:nrow(veh.m[,]), my.cores))


# INTERSECT POLYGONIZED RASTER w/ PARKING + ROAD DATA
save.image(here("data/outputs/temp/rasterize-4.RData")) # save progress in case error in parellelization
rm(traffic.net, park.min.i.u.p, park.max.i.u.p, veh, osm.buf.mrg.min, osm.buf.mrg.max)
gc() # initiate cluster after only all the necessary objects present in R environment
cat(paste0("Time: ", Sys.time(),  # script run time update
           ".\nDuration: ", round(difftime(Sys.time(), script.start, units = "mins")), " mins."))
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
rm(cl, parts.r.min, parts.r.max, parts.p.min, parts.p.max, parts.c, 
   park.min.i.u, park.max.i.u, osm.min.s, osm.max.s)
gc()

# bind the buffered parking data output
osm.min.i <- do.call(rbind, osm.min.i.p) # bind list of spatial objects into single spatial obj
osm.max.i <- do.call(rbind, osm.max.i.p) # bind list of spatial objects into single spatial obj

park.min.i <- do.call(rbind, park.min.i.p) # bind list of spatial objects into single spatial obj
park.max.i <- do.call(rbind, park.max.i.p) # bind list of spatial objects into single spatial obj

veh.i <- do.call(rbind, veh.i.p) # bind list of spatial objects into single spatial obj

# calc adjusted fractional area of parking/pavement in raster cell size
osm.min.i$area <- st_area(osm.min.i)
osm.min.i$frac <- osm.min.i$area / (res * res) # divide by raster cell area
osm.max.i$area <- st_area(osm.max.i)
osm.max.i$frac <- osm.max.i$area / (res * res) # divide by raster cell area

park.min.i$area <- st_area(park.min.i)
park.min.i$frac <- park.min.i$area / (res * res) # divide by raster cell area
park.max.i$area <- st_area(park.max.i)
park.max.i$frac <- park.max.i$area / (res * res) # divide by raster cell area

# for vehicle travel, calc veh-ft traveled by trimmed link length * vehicles traversed
# we use the trimmed length of the adjusted link times the adjustment ratio to 
# approx the actual distance traveled on the trimmed link
for(i in 7:(length(iflow)+5)){
  veh.i[[i]] <- veh.i[[i]] * st_length(veh.i) * veh.i$adj.l.ratio
}

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

# assign aggregated class names for assigning pavement heat model data
fclass.meta <- readRDS(here("data/outputs/pavement-class-summary.rds"))
for(f in 1:4){
  osm.min.p$class[osm.min.p$fclass %in% fclass.meta[[f]]$class] <- names(fclass.meta)[f]
  osm.max.p$class[osm.max.p$fclass %in% fclass.meta[[f]]$class] <- names(fclass.meta)[f]
}

# same for vehicles based on capcity
veh.p.local <- veh.p[veh.p$capacity < 1000,]
veh.p.minor <- veh.p[veh.p$capacity %in% c(1000:2000),]
veh.p.major <- veh.p[veh.p$capacity %in% c(2001:7000),]
veh.p.hiway <- veh.p[veh.p$capacity > 7000,]

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

parts.c.local <- split(1:nrow(veh.p.local[,]), cut(1:nrow(veh.p.local[,]), my.cores))
parts.c.minor <- split(1:nrow(veh.p.minor[,]), cut(1:nrow(veh.p.minor[,]), my.cores))
parts.c.major <- split(1:nrow(veh.p.major[,]), cut(1:nrow(veh.p.major[,]), my.cores))
parts.c.hiway <- split(1:nrow(veh.p.hiway[,]), cut(1:nrow(veh.p.hiway[,]), my.cores))

parts.svf <- split(1:nrow(svf.pts[,]), cut(1:nrow(svf.pts[,]), my.cores))

# create temporary output directory
dir.create(here("data/outputs/temp/rasters"), showWarnings = F)

# CREATE FINAL RASTERS OF ROAD/PARK FRACTIONAL AREA AND DAILY VMT
save.image(here("data/outputs/temp/rasterize-5.RData")) # save progress in case error in parellelization
rm(osm.min.i.p, osm.max.i.p, park.min.i.p, park.max.i.p, veh.i.p,
   osm.min.c, osm.max.c, osm.min.i, osm.max.i, 
   park.min.i, park.max.i, park.min.c, park.max.c, 
   veh.c, svf, park.min.p, park.max.p) # remove unused objects
gc() # initiate cluster after only all the necessary objects present in R environment
cat(paste0("Time: ", Sys.time(),  # script run time update
           ".\nDuration: ", round(difftime(Sys.time(), script.start, units = "mins")), " mins."))
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
# rasterize veh-ft-traveled based on cleaned icarus clipped links with vehicle travel centrioded
# loop through each timeslice during day and compute rasterization of flow for timeperiod
# this is done for each of the four major classes of road types
for(j in 7:(length(iflow)+5)){ # length of unique timeperiods 
  invisible(foreach(i = 1:my.cores, .packages = c("raster", "here")) %dopar% {
    rasterize(veh.p.local[parts.c.local[[i]],], r, field = j, fun = sum, background = 0, 
              filename = here(paste0("data/outputs/temp/rasters/vkt-local-time-", j-6, "-part-", i, ".tif")), 
              overwrite = T)
  })
  invisible(foreach(i = 1:my.cores, .packages = c("raster", "here")) %dopar% {
    rasterize(veh.p.minor[parts.c.minor[[i]],], r, field = j, fun = sum, background = 0, 
              filename = here(paste0("data/outputs/temp/rasters/vkt-minor-time-", j-6, "-part-", i, ".tif")), 
              overwrite = T)
  })
  invisible(foreach(i = 1:my.cores, .packages = c("raster", "here")) %dopar% {
    rasterize(veh.p.major[parts.c.major[[i]],], r, field = j, fun = sum, background = 0,
              filename = here(paste0("data/outputs/temp/rasters/vkt-major-time-", j-6, "-part-", i, ".tif")), 
              overwrite = T)
  })
  invisible(foreach(i = 1:my.cores, .packages = c("raster", "here")) %dopar% {
    rasterize(veh.p.hiway[parts.c.hiway[[i]],], r, field = j, fun = sum, background = 0, 
              filename = here(paste0("data/outputs/temp/rasters/vkt-hiway-time-", j-6, "-part-", i, ".tif")), 
              overwrite = T)
  })
}


# SVF
# rasterize SVF point data into mean by pixel
invisible(foreach(i = 1:my.cores, .packages = c("raster", "here")) %dopar% {
  rasterize(svf.pts[parts.svf[[i]],], r, field = "SVF", fun = mean, background = NA,
            filename = here(paste0("data/outputs/temp/rasters/svf-part-", i, ".tif")), 
            na.rm = T, overwrite = T)
})

# stop cluster
stopCluster(cl)
rm(osm.min.p, osm.max.p, park.min.p.asph, park.min.p.conc, park.max.p.asph, park.max.p.conc, 
   parts.min.r.local, parts.min.r.minor, parts.min.r.major, parts.min.r.hiway,
   parts.max.r.local, parts.max.r.minor, parts.max.r.major, parts.max.r.hiway,
   veh.p.local, veh.p.minor, veh.p.major, veh.p.hiway, cl, veh.m)
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

#r.veh.p <- lapply(1:my.cores, function (i) raster(here(paste0("data/outputs/temp/rasters/veh-part-", i, ".tif"))))

# create raster brick where each layer is time slice for the four major road classes 
for(j in 7:(length(iflow)+5)){
  r.veh.p.local <- lapply(1:my.cores, function (i) raster(here(paste0("data/outputs/temp/rasters/vkt-local-time-", j-6, "-part-", i, ".tif"))))
  r.veh.p.local$fun <- sum
  r.veh.local <- do.call(mosaic, r.veh.p.local)
  assign(paste0("r.veh.local.", j-6), r.veh.local) 
}
r.veh.local.list <- paste0("r.veh.local.", 1:(length(iflow)-1)) # list of all raster objects for each time slice
r.veh.local <- stack(lapply(r.veh.local.list, function(x) (get(x)))) # get and stack list of raster objects
rm(list = r.veh.local.list)
names(r.veh.local) <- paste0("vkt.local.", 1:100) # name flows

for(j in 7:(length(iflow)+5)){
  r.veh.p.minor <- lapply(1:my.cores, function (i) raster(here(paste0("data/outputs/temp/rasters/vkt-minor-time-", j-6, "-part-", i, ".tif"))))
  r.veh.p.minor$fun <- sum
  r.veh.minor <- do.call(mosaic, r.veh.p.minor)
  assign(paste0("r.veh.minor.", j-6), r.veh.minor) 
}
r.veh.minor.list <- paste0("r.veh.minor.", 1:(length(iflow)-1)) # list of all raster objects for each time slice
r.veh.minor <- stack(lapply(r.veh.minor.list, function(x) (get(x)))) # get and stack list of raster objects
rm(list = r.veh.minor.list)
names(r.veh.minor) <- paste0("vkt.minor.", 1:100) # name flows

for(j in 7:(length(iflow)+5)){
  r.veh.p.major <- lapply(1:my.cores, function (i) raster(here(paste0("data/outputs/temp/rasters/vkt-major-time-", j-6, "-part-", i, ".tif"))))
  r.veh.p.major$fun <- sum
  r.veh.major <- do.call(mosaic, r.veh.p.major)
  assign(paste0("r.veh.major.", j-6), r.veh.major) 
}
r.veh.major.list <- paste0("r.veh.major.", 1:(length(iflow)-1)) # list of all raster objects for each time slice
r.veh.major <- stack(lapply(r.veh.major.list, function(x) (get(x)))) # get and stack list of raster objects
rm(list = r.veh.major.list)
names(r.veh.major) <- paste0("vkt.major.", 1:100) # name flows

for(j in 7:(length(iflow)+5)){
  r.veh.p.hiway <- lapply(1:my.cores, function (i) raster(here(paste0("data/outputs/temp/rasters/vkt-hiway-time-", j-6, "-part-", i, ".tif"))))
  r.veh.p.hiway$fun <- sum
  r.veh.hiway <- do.call(mosaic, r.veh.p.hiway)
  assign(paste0("r.veh.hiway.", j-6), r.veh.hiway) 
}
r.veh.hiway.list <- paste0("r.veh.hiway.", 1:(length(iflow)-1)) # list of all raster objects for each time slice
r.veh.hiway <- stack(lapply(r.veh.hiway.list, function(x) (get(x)))) # get and stack list of raster objects
rm(list = r.veh.hiway.list)
names(r.veh.hiway) <- paste0("vkt.hiway.", 1:100) # name flows
gc()

# get sky veiw factor rasterized data
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

r.svf.p$fun <- mean
r.svf <- do.call(mosaic, r.svf.p)

# combine and summarize all vehicle flow data into one stack
r.veh <- stack(r.veh.local, r.veh.minor, r.veh.major, r.veh.hiway)
r.veh <- stack(r.veh, stackApply(r.veh, indices = rep(1:4, each = 100), fun = sum)) # add the sum by fclass
r.veh <- stack(r.veh, stackApply(r.veh[[401:404]], indices = c(1), fun = sum))
names(r.veh)[401:405] <- c("daily.vkt.local", "daily.vkt.minor", "daily.vkt.major", "daily.vkt.hiway", "daily.vkt")

# make correct units of vkt by dividing result by 5280 (ft per mile) mult by 1.60934 km per mi
for(i in 1:length(names(r.veh))){
  #values(r.veh[[i]]) <- values(r.veh[[i]]) / 5280  * 1.60934 # for VKT (vehicle kilometers traveled)
#  #values(r.veh.log) <- log10(values(r.veh.log) + 1) # log base 10 values
}

#r.veh.log <- r.veh # log scale vkt
#for(i in 1:length(names(r.veh))){
#  values(r.veh.log) <- log10(values(r.veh.log) + 1) # log base 10 values
#}

# also rush hour summaries
# morning rush would be from: hour = 7.92 am to 8.88 am (6:54:12 am to 8:52:48 am); V34:V37
# evening rush would be from: hour = 5.04 pm  to 6 pm (5:02:24 pm to 6:00:00 pm); V72:V75
rush.hrs <- stackApply(stack(r.veh[[34:37]], r.veh[[134:137]], r.veh[[234:237]], r.veh[[334:337]],
                             r.veh[[72:75]], r.veh[[172:175]], r.veh[[272:275]], r.veh[[372:375]]), 
                       indices = rep(1:8, each = 4), fun = sum)
names(rush.hrs) <- c("vkt.local.8am", "vkt.minor.8am", "vkt.major.8am", "vkt.hiway.8am",
                     "vkt.local.5pm", "vkt.minor.5pm", "vkt.major.5pm", "vkt.hiway.5pm")

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
names(r.road) <- c("min.local.road", "min.minor.road", "min.major.road", "min.hiway.road", "min.all.roads", # names in order of bindings
                       "max.local.road", "max.minor.road", "max.major.road", "max.hiway.road", "max.all.roads",
                       "avg.local.road", "avg.minor.road", "avg.major.road", "avg.hiway.road", "avg.all.roads")

r.park.min <- stack(r.park.min.asph, r.park.min.conc) # stack min parking fractions by class
r.park.min <- stack(r.park.min, stackApply(r.park.min, indices = c(1), fun = sum)) # create all roads sum for min scenario

r.park.max <- stack(r.park.max.asph, r.park.max.conc) # stack max parking fractions by class
r.park.max <- stack(r.park.max, stackApply(r.park.max, indices = c(1), fun = sum)) # create all roads sum for max scenario

r.park <- stack(r.park.min, r.park.max) # combine min and max fractional areas into raster stack
r.park <- stack(r.park, stackApply(r.park, indices = c(1:3,1:3), fun = mean)) # add the mean by fclass
names(r.park) <- c("min.com.park", "min.res.park", "min.all.park",
                   "max.com.park", "max.res.park", "max.all.park",
                   "avg.res.park", "avg.com.park", "avg.all.park")

# pavement: parking + roads
r.pave <- stack(r.road, r.park, stackApply(stack(r.road[[c("min.all.roads","max.all.roads","avg.all.roads")]],
                                                 r.park[[c("min.all.park","max.all.park","avg.all.park")]]),
                                           indices = c(1:3,1:3), fun = sum))
names(r.pave)[25:27] <- c("min.pave", "max.pave", "avg.pave") # add names
values(r.pave$min.pave) <- ifelse(values(r.pave$min.pave) > 1.0, 1.0, values(r.pave$min.pave)) # make sure no > 1.0 for max pave
values(r.pave$avg.pave) <- ifelse(values(r.pave$avg.pave) > 1.0, 1.0, values(r.pave$avg.pave)) # make sure no > 1.0 for max pave
values(r.pave$max.pave) <- ifelse(values(r.pave$max.pave) > 1.0, 1.0, values(r.pave$max.pave)) # make sure no > 1.0 for max pave

# add vehicle summary and svf to master raster
r.all <- stack(r.pave, 
               r.veh[[c("daily.vkt.local", "daily.vkt.minor", "daily.vkt.major", "daily.vkt.hiway", "daily.vkt")]], 
               rush.hrs,
               r.svf)
names(r.all)[41] <- c("SVF")

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
r.all$min.day.flux.local <- (r.all$min.local.road
                             * (((pave.veh.meta[pave.class == "local" & SVF == 1.0, mean.day.out.flux.lwr] 
                                  - pave.veh.meta[pave.class == "unpaved" & SVF == 1.0, mean.day.out.flux.lwr]) * r.all$adj.SVF)
                                + ((pave.veh.meta[pave.class == "local" & SVF == 0.5, mean.day.out.flux.lwr]
                                   - pave.veh.meta[pave.class == "unpaved" & SVF == 0.5, mean.day.out.flux.lwr]) * (1 - r.all$adj.SVF))))
r.all$min.day.flux.minor <- (r.all$min.minor.road
                             * (((pave.veh.meta[pave.class == "minor" & SVF == 1.0, mean.day.out.flux.lwr]
                                 - pave.veh.meta[pave.class == "unpaved" & SVF == 1.0, mean.day.out.flux.lwr]) * r.all$adj.SVF)
                                + ((pave.veh.meta[pave.class == "minor" & SVF == 0.5, mean.day.out.flux.lwr]
                                   - pave.veh.meta[pave.class == "unpaved" & SVF == 0.5, mean.day.out.flux.lwr]) * (1 - r.all$adj.SVF))))
r.all$min.day.flux.major <- (r.all$min.major.road
                             * (((pave.veh.meta[pave.class == "major" & SVF == 1.0, mean.day.out.flux.lwr]
                                 - pave.veh.meta[pave.class == "unpaved" & SVF == 1.0, mean.day.out.flux.lwr]) * r.all$adj.SVF)
                                + ((pave.veh.meta[pave.class == "major" & SVF == 0.5, mean.day.out.flux.lwr]
                                   - pave.veh.meta[pave.class == "unpaved" & SVF == 0.5, mean.day.out.flux.lwr]) * (1 - r.all$adj.SVF))))
r.all$min.day.flux.hiway <- (r.all$min.hiway.road
                             * (((pave.veh.meta[pave.class == "highway" & SVF == 1.0, mean.day.out.flux.lwr]
                                 - pave.veh.meta[pave.class == "unpaved" & SVF == 1.0, mean.day.out.flux.lwr]) * r.all$adj.SVF)
                                + ((pave.veh.meta[pave.class == "highway" & SVF == 0.5, mean.day.out.flux.lwr]
                                   - pave.veh.meta[pave.class == "unpaved" & SVF == 0.5, mean.day.out.flux.lwr]) * (1 - r.all$adj.SVF))))

r.all <- stack(r.all, stackApply(r.all[[c("min.day.flux.local", "min.day.flux.minor", "min.day.flux.major", "min.day.flux.hiway")]], 
                                 indices = c(1), fun = sum))
names(r.all[[nlayers(r.all)]]) <- "min.day.flux.roads"

# max roads heat flux
r.all$max.day.flux.local <- (r.all$max.local.road
                             * (((pave.veh.meta[pave.class == "local" & SVF == 1.0, mean.day.out.flux.upr]
                                 - pave.veh.meta[pave.class == "unpaved" & SVF == 1.0, mean.day.out.flux.upr]) * r.all$adj.SVF)
                                + ((pave.veh.meta[pave.class == "local" & SVF == 0.5, mean.day.out.flux.upr]
                                   - pave.veh.meta[pave.class == "unpaved" & SVF == 0.5, mean.day.out.flux.upr])* (1 - r.all$adj.SVF))))
r.all$max.day.flux.minor <- (r.all$max.minor.road
                             * (((pave.veh.meta[pave.class == "minor" & SVF == 1.0, mean.day.out.flux.upr]
                                 - pave.veh.meta[pave.class == "unpaved" & SVF == 1.0, mean.day.out.flux.upr]) * r.all$adj.SVF)
                                + ((pave.veh.meta[pave.class == "minor" & SVF == 0.5, mean.day.out.flux.upr]
                                   - pave.veh.meta[pave.class == "unpaved" & SVF == 0.5, mean.day.out.flux.upr])* (1 - r.all$adj.SVF))))
r.all$max.day.flux.major <- (r.all$max.major.road
                             * (((pave.veh.meta[pave.class == "major" & SVF == 1.0, mean.day.out.flux.upr]
                                 - pave.veh.meta[pave.class == "unpaved" & SVF == 1.0, mean.day.out.flux.upr]) * r.all$adj.SVF)
                                + ((pave.veh.meta[pave.class == "major" & SVF == 0.5, mean.day.out.flux.upr]
                                   - pave.veh.meta[pave.class == "unpaved" & SVF == 0.5, mean.day.out.flux.upr]) * (1 - r.all$adj.SVF))))
r.all$max.day.flux.hiway <- (r.all$max.hiway.road
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
# min parking heat flux
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

# max parking heat flux
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

# avg heat flux parking
r.all <- stack(r.all, stackApply(r.all[[c("min.day.flux.park", "max.day.flux.park")]], indices = c(1), fun = mean))
names(r.all[[nlayers(r.all)]]) <- "avg.day.flux.park"

#plot(r.all[[c("min.day.flux.park", "max.day.flux.park")]])

# vehicles
# min veh heat flux
# assume 8800  Wh/liter (33.3 kWh/gal)
# 1 MPG = 0.425144 km/liter or 1 gal/mi = 2.35215 liter/km
# energy lost to waste heat assumed = 0.65 (65%)
pave.veh.meta[, energy.min := (1 / MPGe.min) * 2.35215 * 8800 * 0.65] # Wh/km (Watt-hours per kilometer)
pave.veh.meta[, energy.max := (1 / MPGe.max) * 2.35215 * 8800 * 0.65] # Wh/km (Watt-hours per kilometer)

# heat flux from vehicles in a day = VKT (km) * energy rate (Wh/km) / hours (hrs) / cell resolution (m2) = W/m2
# min veh flux/day
r.all$min.day.flux.local.veh <- (r.all$daily.vkt.local  * pave.veh.meta[pave.class == "local" & SVF == 1.0, energy.min] / 24 / ((res / 3.28084)^2))
r.all$min.day.flux.minor.veh <- (r.all$daily.vkt.minor  * pave.veh.meta[pave.class == "minor" & SVF == 1.0, energy.min] / 24 / ((res / 3.28084)^2))
r.all$min.day.flux.major.veh <- (r.all$daily.vkt.major  * pave.veh.meta[pave.class == "major" & SVF == 1.0, energy.min] / 24 / ((res / 3.28084)^2))
r.all$min.day.flux.hiway.veh <- (r.all$daily.vkt.hiway  * pave.veh.meta[pave.class == "highway" & SVF == 1.0, energy.min] / 24 / ((res / 3.28084)^2))

r.all <- stack(r.all, stackApply(r.all[[c("min.day.flux.local.veh", "min.day.flux.minor.veh", "min.day.flux.major.veh", "min.day.flux.hiway.veh")]], 
                                 indices = c(1), fun = sum))
names(r.all[[nlayers(r.all)]]) <- "min.day.flux.veh"

# max veh flux/day
r.all$max.day.flux.local.veh <- (r.all$daily.vkt.local  * pave.veh.meta[pave.class == "local" & SVF == 1.0, energy.max] / 24 / ((res / 3.28084)^2))
r.all$max.day.flux.minor.veh <- (r.all$daily.vkt.minor  * pave.veh.meta[pave.class == "minor" & SVF == 1.0, energy.max] / 24 / ((res / 3.28084)^2))
r.all$max.day.flux.major.veh <- (r.all$daily.vkt.major  * pave.veh.meta[pave.class == "major" & SVF == 1.0, energy.max] / 24 / ((res / 3.28084)^2))
r.all$max.day.flux.hiway.veh <- (r.all$daily.vkt.hiway  * pave.veh.meta[pave.class == "highway" & SVF == 1.0, energy.max] / 24 / ((res / 3.28084)^2))

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
# plots to check
plot(r.all$avg.all.roads) # , rev(heat.colors(255))
plot(r.all$avg.all.park)
plot(r.all$avg.pave) 
#plot(r.all$log.VKT)
plot(r.all$SVF, col = rev(rainbow(255, start = 4/6, end = 6/6))) #
plot(r.all$total.avg.day.flux)
plot(r.all[[c("avg.all.roads", "avg.all.park", "daily.vkt", "total.avg.day.flux")]])
#plot(r.all[[c("min.day.flux.veh", "min.day.flux.park", "min.day.flux.roads", "total.min.day.flux")]])
plot(r.all[[c("avg.day.flux.veh", "avg.day.flux.park", "avg.day.flux.roads", "total.avg.day.flux")]], 
     main = c("Mean Daily Excess Heat Flux from Vehicles","Mean Daily Excess Heat Flux from Parking Pavement", 
              "Mean Daily Excess Heat Flux from Roadway Pavement","Mean Daily Excess Heat Flux from Vehicles & Pavements"))
     #xlab = rep("Westing Coordinate (ft)", 2), xlab = rep("Northing Coordinate (ft)", 2))
#plot(r.all[[c("max.day.flux.veh", "max.day.flux.park", "max.day.flux.roads", "total.max.day.flux")]])

mean(values(r.all$total.avg.day.flux)) # total study region mean W/m2 
mean(values(r.all$avg.day.flux.park)) # total study region mean W/m2 
mean(values(r.all$avg.day.flux.roads)) # total study region mean W/m2 
mean(values(r.all$avg.day.flux.veh)) # total study region mean W/m2 


options(scipen = 999)
quants <- lapply(1:length(names(r.all)), function(i) quantile(values(
  r.all[[i]]), c(0, 0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99, 1.0), na.rm = T))
names(quants) <- names(r.all)

#quants

# create output directory if doesn't exist
dir.create(here("data/outputs/rasters"), showWarnings = F) 

# write out final rasters
writeRaster(r.all, here(paste0("data/outputs/rasters/master-pave-veh-heat-", run.name, "-", res / 3.28084, "m.tif")), overwrite = T)
saveRDS(r.all, here(paste0("data/outputs/rasters/master-pave-veh-heat-", run.name, "-", res / 3.28084, "m.rds")))
saveRDS(r.veh, here(paste0("data/outputs/rasters/master-veh-time-heat-", run.name, "-", res / 3.28084, "m.rds")))
#shapefile(veh.p[,1:4], here("data/outputs/shapefiles/icarus-data-network-clipped"))

# paste final runtime
paste0("R model run complete on ", Sys.info()[4]," at ", Sys.time(),
       ". Model run length: ", round(difftime(Sys.time(), script.start, units = "mins"),2)," mins.")

