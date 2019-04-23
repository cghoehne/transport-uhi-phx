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
#osm <- shapefile(here("data/osm/maricopa_county_osm_roads.shp")) # import OSM data (maricopa county clipped raw road network data)
osm <- shapefile(here("data/outputs/temp/osm-test.shp")) # SMALL TEST NETWORK (NORTH TEMPE)
parking <- readRDS(here("data/parking/phx-parking.rds")) # phoenix off-street parking space data by parcel centriod xy coords in EPSG:2223
fclass.info <- fread(here("data/osm_fclass_info.csv")) # additional OSM info by roadway functional class (fclass)
uza.buffer <- readRDS(here("data/outputs/temp/uza-buffer.rds")) # Maricopa UZA buffered ~1mi

# PARKING DATA FORMAT
# create a SPDF from parking coords and data
parking.pts <- SpatialPointsDataFrame(parking[,.(X,Y)], # coords from EPSG:2223
                                      proj4string = crs(uza.buffer), # CRS EPSG:2223
                                      data = parking[, .(APN, spaces, type)]) # other data

# clip parking data to desired extent
parking.pts <- intersect(parking.pts, extent(osm)) # FOR TEST NETWORK
#parking.pts <- intersect(parking.pts, uza.buffer)

# calculate min and max parking area by property type and other assumptions (proj is in ft)

# for max parking area in both commerical and residentail areas, 
# assume both have 330 sq ft per space dedicated 
# this is based on Shoup and others estiamtes - Phx parking assumed this too
parking.pts$max.area <- parking.pts$spaces * 330

# for minimum commerical parking: assume a max of 20% of commerical parking is in someway shaded
parking.pts$min.area <- parking.pts$spaces * 330 * 0.80


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

# transform osm crs to EPSG:2223
osm <- spTransform(osm, crs(uza.buffer))

# clip osm data to buffer uza for quicker computing
osm <- intersect(osm, uza.buffer) # better than gIntersection b/c it keeps attributes

# store osm data for quick calcs of new variables and rebind later
osm.dt <- as.data.table(osm@data)

# merge fclass.info with osm data
osm.dt <- merge(osm.dt, fclass.info, by = "fclass", all.x = T)

# drop tunnels, and non pavement fclass
osm.dt <- osm.dt[tunnel == "F" & min.2w.width.m > 0 
                 & !(fclass %in% c("bridleway", "path", "track_grade3",
                                   "track_grade4", "track_grade5", "unknown")),]

# because the fclass of "service" roads is inconsistent but does often trace parking lots,
# we ignore this in place of parking estimates which will be more consistent.
# NOTE: it also traces alleyways consistently but there is no easy way to disentangle
osm.dt <- osm.dt[fclass != "service",]

# calc min/max road width based on 2way widths and 1way/2way ("B" == both, "T"/"F" True/False for driving in the opposite dir of linestring)
osm.dt[oneway == "B", max.width := as.numeric(max.2w.width.m)]
osm.dt[oneway == "B", min.width := as.numeric(min.2w.width.m)]
osm.dt[oneway == "T" | oneway == "F", max.width := as.numeric(max.2w.width.m / 2)]
osm.dt[oneway == "T" | oneway == "F", min.width := as.numeric(min.2w.width.m / 2)]

# calc min/max buffer radius (in ft from m) of each road
osm.dt[, min.r.buf := min.width * 3.28084 * 0.5] # raduis = 0.5 * diameter
osm.dt[, max.r.buf := max.width * 3.28084 * 0.5] # 3.28084 ft per meter

# merge filtered data with min/max roadway widths to spatial osm data
osm <- merge(osm, osm.dt[, .(osm_id, min.width, max.width, min.r.buf, max.r.buf)], by = "osm_id", all.x = F)

# remove tunnel and oneway as no longer needed
osm$tunnel <- NULL
osm$oneway <- NULL

# clean up space
save.image(here("data/outputs/temp/rasterize-data-1.RData")) # first save for data backup
rm(list=setdiff(ls(), c("osm", "script.start", "parking.pts")))
gc()

# calculate the number of cores
my.cores <- parallel::detectCores() - 1 # store computers cores n-1 for headspace

# initiate cluster after only all the necessary objects present in R environment
cl <- makeCluster(my.cores)
registerDoParallel(cl) # register parallel backend
clusterCall(cl, function(x) .libPaths(x), .libPaths())

w.min <- unique(osm$min.r.buf)  # list of unique min buffer widths based on oneway and estiamted roadway width
b.min <- list() # create empty list for foreach
osm.buf.min <- foreach(i = 1:length(w.min), .packages = c("sp","rgeos")) %dopar% {
  #b.min[[i]] <- buffer(osm[osm$min.r.buf == w.min[i], ], width = w.min[i])
  b.min[[i]] <- gBuffer(osm[osm$min.r.buf == w.min[i], ], byid = T, width = w.min[i], capStyle = "ROUND") # round b/c end of roads are usually cul-de-sac
}

w.max <- unique(osm$max.r.buf)  # list of unique min buffer widths based on oneway and estiamted roadway width
b.max <- list() # create empty list for foreach
osm.buf.max <- foreach(i = 1:length(w.max), .packages = c("sp","rgeos")) %dopar% {
  #b.max[[i]] <- buffer(osm[osm$max.r.buf == w.max[i], ], width = w.max[i])
  b.max[[i]] <- gBuffer(osm[osm$max.r.buf == w.max[i], ], byid = T, width = w.max[i], capStyle = "ROUND") # round b/c end of roads are usually cul-de-sac
} 

# stop cluster
stopCluster(cl)

# bind the buffered osm data output
osm.buf.mrg.min <- do.call(raster::bind, osm.buf.min) # bind list of spatial objects into single spatial obj
osm.buf.mrg.max <- do.call(raster::bind, osm.buf.max) # bind list of spatial objects into single spatial obj

# dissolve the roadway buffers to polygons by fclass (roadway class) to eliminate overlaps by fclass
#osm.min <- unionSpatialPolygons(osm.buf.mrg.min, osm.buf.mrg.min$fclass)
#osm.max <- unionSpatialPolygons(osm.buf.mrg.max, osm.buf.mrg.max$fclass)

# get list of fclass for min/max roads
#fclass.min <- unique(osm.buf.mrg.min$fclass)
#fclass.max <- unique(osm.buf.mrg.max$fclass)

# rebind the fclass and create a SPDF
#osm.min.sp <- SpatialPolygonsDataFrame(Sr = osm.min, data = data.frame(row.names = fclass.min, fclass.min))
#osm.max.sp <- SpatialPolygonsDataFrame(Sr = osm.max, data = data.frame(row.names = fclass.max, fclass.max))

# subract overlapping areas, removing lower tier road class 
# NOT YET IMPLEMENTED

# RASTERIZE DATA
# create empty raster at desired extent, use osm data extent
#res <- 164.042 # ~50 x 50 m   IDEAL
res <- 328.084 # ~100 x 100 m
#res <- 820.21  # ~250m x 250m
#res <- 1640.42  # ~500m x 500m
#res <- 3280.84 # ~1000 x 1000 m; full script run at this res is 202.68 min using 3 cores
r <- raster(ext = extent(osm), crs = crs(osm), res = res) # create raster = ~10 x 10 m

# polygonize raster, clip roadway area by this polygon, calc road area and fractional road area for each feature
r.p <- rasterToPolygons(r)
#osm.min.i <- raster::intersect(osm.min.s, r.p)
#osm.min.i$area <- gArea(osm.min.i, byid = T)
#osm.min.i$frac <- osm.min.i$area / (res * res) # divide by raster cell area
#osm.max.i <- raster::intersect(osm.max.s, r.p)
#osm.max.i$area <- gArea(osm.max.i, byid = T)
#osm.max.i$frac <- osm.max.i$area / (res * res) # divide by raster cell area

# also convert total area of parking into fractional area of raster cell size
#parking.pts$min.frac <- parking.pts$min.area / (res * res) 
#parking.pts$max.frac <- parking.pts$max.area / (res * res) 

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
cl <- makeCluster(my.cores) # initiate cluster
registerDoParallel(cl) # register parallel backend
clusterCall(cl, function(x) .libPaths(x), .libPaths()) 

# blank lists
p.min <- list()
p.max <- list()

park.buf.min.p <- foreach(i = 1:my.cores, .packages = c("sp","rgeos")) %dopar% {
  p.min[[i]] <- gBuffer(parking.pts[parts.p[[i]],], byid = T, width = parking.pts$min.r[parts.p[[i]]], capStyle = "ROUND")
}
park.buf.max.p <- foreach(i = 1:my.cores, .packages = c("sp","rgeos")) %dopar% {
  p.max[[i]] <- gBuffer(parking.pts[parts.p[[i]],], byid = T, width = parking.pts$max.r[parts.p[[i]]], capStyle = "ROUND")
}

# stop cluster
stopCluster(cl)

# bind the buffered parking data output
park.buf.min <- do.call(raster::bind, park.buf.min.p) # bind list of spatial objects into single spatial obj
park.buf.max <- do.call(raster::bind, park.buf.max.p) # bind list of spatial objects into single spatial obj

# actual areas (use to check accuracy)
#park.buf.min$min.area.act <- gArea(park.buf.min, byid = T)
#park.buf.max$max.area.act <- gArea(park.buf.max, byid = T)

# convert to sf objects for quicker computing
# sf::st_intersection is fastest for zonal intersections
osm.min.s <- st_as_sf(osm.buf.mrg.min)
osm.max.s <- st_as_sf(osm.buf.mrg.max)

park.min.s <- st_as_sf(park.buf.min)
park.max.s <- st_as_sf(park.buf.max)

r.t <- st_as_sf(r.p)

# split total point features in parking data into parts for parellel splitting
parts.r.min <- split(1:nrow(osm.min.s[,]), cut(1:nrow(osm.min.s[,]), my.cores))
parts.r.max <- split(1:nrow(osm.max.s[,]), cut(1:nrow(osm.max.s[,]), my.cores))

parts.p.min <- split(1:nrow(park.min.s[,]), cut(1:nrow(park.min.s[,]), my.cores))
parts.p.max <- split(1:nrow(park.max.s[,]), cut(1:nrow(park.max.s[,]), my.cores))

# blank lists
r.min <- list()
r.max <- list()

rm(list=setdiff(ls(), c("my.cores", "script.start", "r", "r.t", "res", "p.min", "p.max", "r.min", "r.max",
                        "parts.r.min", "parts.r.max", "osm.min.s", "osm.max.s", "parts.p.min", "parts.p.max", "park.min.s", "park.max.s"
)))
gc()

# INTERSECT POLYGONIZED RASTER w/ PARKING + ROAD DATA
cl <- makeCluster(my.cores) # initiate cluster
registerDoParallel(cl) # register parallel backend
clusterCall(cl, function(x) .libPaths(x), .libPaths()) 

park.min.i.p <- foreach(i = 1:my.cores, .packages = c("sf")) %dopar% {
  p.min[[i]] <- st_intersection(park.min.s[parts.p.min[[i]],], r.t)
}
park.max.i.p <- foreach(i = 1:my.cores, .packages = c("sp")) %dopar% {
  p.max[[i]] <- st_intersection(park.max.s[parts.p.max[[i]],], r.t)
}
osm.min.i.p <- foreach(i = 1:my.cores, .packages = c("sf")) %dopar% {
  r.min[[i]] <- st_intersection(osm.min.s[parts.r.min[[i]],], r.t)
}
osm.max.i.p <- foreach(i = 1:my.cores, .packages = c("sp")) %dopar% {
  r.max[[i]] <- st_intersection(osm.max.s[parts.r.max[[i]],], r.t)
}

# stop cluster
stopCluster(cl)

# bind the buffered parking data output
osm.min.i <- do.call(rbind, osm.min.i.p) # bind list of spatial objects into single spatial obj
osm.max.i <- do.call(rbind, osm.max.i.p) # bind list of spatial objects into single spatial obj

park.min.i <- do.call(rbind, park.min.i.p) # bind list of spatial objects into single spatial obj
park.max.i <- do.call(rbind, park.max.i.p) # bind list of spatial objects into single spatial obj

# calc adjusted fractional area of parking/pavement in raster cell size
osm.min.i$area <- st_area(osm.min.i)
osm.min.i$frac <- osm.min.i$area / (res * res) # divide by raster cell area
osm.max.i$area <- st_area(osm.max.i)
osm.max.i$frac <- osm.max.i$area / (res * res) # divide by raster cell area

park.min.i$area <- st_area(park.min.i)
park.min.i$frac <- park.min.i$area / (res * res) # divide by raster cell area
park.max.i$area <- st_area(park.max.i)
park.max.i$frac <- park.max.i$area / (res * res) # divide by raster cell area

# convert clipped spatial parking area to centroids and data 
osm.min.c <- st_centroid(osm.min.i)
osm.max.c <- st_centroid(osm.max.i)

park.min.c <- st_centroid(park.min.i)
park.max.c <- st_centroid(park.max.i)

# convert to sp obj
park.min.p <- as(park.min.c, "Spatial")
park.max.p <- as(park.max.c, "Spatial")

osm.min.p <- as(osm.min.c, "Spatial")
osm.max.p <- as(osm.max.c, "Spatial")

# convert clipped spatial roads to centroids and data 
#osm.min.p <- gCentroid(osm.min.i, byid = T)
#osm.min.p <- SpatialPointsDataFrame(osm.min.p, osm.min.i@data)
#osm.max.p <- gCentroid(osm.max.i, byid = T)
#osm.max.p <- SpatialPointsDataFrame(osm.max.p, osm.max.i@data)

# split total point features in pavement/parking data into parts for parellel splitting
parts.min.r <- split(1:nrow(osm.min.p[,]), cut(1:nrow(osm.min.p[,]), my.cores))
parts.max.r <- split(1:nrow(osm.max.p[,]), cut(1:nrow(osm.max.p[,]), my.cores))

parts.min.p <- split(1:nrow(park.min.p[,]), cut(1:nrow(park.min.p[,]), my.cores))
parts.max.p <- split(1:nrow(park.max.p[,]), cut(1:nrow(park.max.p[,]), my.cores))

# number of unique fclassess in roadway SPDF for parellel splitting
#features.min.r <- unique(osm.min.p$fclass.min)
#features.max.r <- unique(osm.max.p$fclass.max)

# create temporary output directory
dir.create(here("data/outputs/temp/rasters"), showWarnings = F)

# clean up space
save.image(here("data/outputs/temp/rasterize-data-2.RData")) # first save for data backup
rm(list=setdiff(ls(), c("my.cores", "script.start", "r", "parts.min.r", "parts.max.r"
#                        , "osm.max.i", "osm.min.i", "parking.pts",
                        , "osm.min.p", "osm.max.p", "parts.min.p", "parts.max.p", "park.min.p", "park.max.p"
                        )))
gc()

# initiate cluster after only all the necessary objects present in R environment
cl <- makeCluster(my.cores)
registerDoParallel(cl) # register parallel backend
clusterCall(cl, function(x) .libPaths(x), .libPaths())

# ROADS
# rasterize in parellel min/max fractional road network area by fclass and save
# based on rasterize getCover which requires 1/10 the raster resolution to be greater than the min roadway width
# or this method will be very inaccurate (and it is slower than the point based alternative below)
#foreach(i = 1:length(features.min.r), .packages = c("raster", "here")) %dopar% {
#  rasterize(osm.min.i[osm.min.i$fclass.min == features.min.r[i],], r, getCover = T, #field = "frac", fun = sum, background = 0,
#            filename = here(paste0("data/outputs/temp/rasters/road-min-", features.min.r[i], "-gcv.tif")), 
#            overwrite = T)
#}
#foreach(i = 1:length(features.max.r), .packages = c("raster", "here")) %dopar% {
#  rasterize(osm.max.i[osm.max.i$fclass.max == features.max.r[i],], r, getCover = T,
#            filename = here(paste0("data/outputs/temp/rasters/road-max-", features.max.r[i], "-gcv.tif")), 
#            overwrite = T)
#}

# rasterize in parellel min/max fractional road network area by fclass and save
# based on roadway point summary data clipped by raster cells to ensure no loss of data from getCover
foreach(i = 1:length(parts.min.r), .packages = c("raster", "here")) %dopar% {
  rasterize(osm.min.p[parts.min.r[[i]],], r, field = "frac", fun = "first", background = 0,
            filename = here(paste0("data/outputs/temp/rasters/road-min-part-", i, ".tif")), 
            overwrite = T)
}
foreach(i = 1:length(parts.max.r), .packages = c("raster", "here")) %dopar% {
  rasterize(osm.max.p[parts.max.r[[i]],], r, field = "frac", fun = "first", background = 0,
            filename = here(paste0("data/outputs/temp/rasters/road-max-part-", i, ".tif")), 
            overwrite = T)
}

# PARKING 
# rasterize min/max parking area based on parking point summary data (no adjustments)
#foreach(i = 1:my.cores, .packages = c("raster", "here")) %dopar% {
#  rasterize(parking.pts[parts.p[[i]],], r, field = "min.frac", fun = sum, background = 0, 
#            filename = here(paste0("data/outputs/temp/rasters/park-min-part-", i, ".tif")), 
#            overwrite = T)}
#foreach(i = 1:my.cores, .packages = c("raster", "here")) %dopar% {
#  rasterize(parking.pts[parts.p[[i]],], r, field = "max.frac", fun = sum, background = 0, 
#            filename = here(paste0("data/outputs/temp/rasters/park-max-part-", i, ".tif")), 
#            overwrite = T)}

# rasterize min/max parking area based on buffered, raster clipped, and centrioded parking area data (adjusted)
foreach(i = 1:my.cores, .packages = c("raster", "here")) %dopar% {
  rasterize(park.min.p[parts.min.p[[i]],], r, field = "frac", fun = sum, background = 0,
            filename = here(paste0("data/outputs/temp/rasters/park-min-part-", i, ".tif")), 
            overwrite = T)
}
foreach(i = 1:my.cores, .packages = c("raster", "here")) %dopar% {
  rasterize(park.max.p[parts.max.p[[i]],], r, field = "frac", fun = sum, background = 0,
            filename = here(paste0("data/outputs/temp/rasters/park-max-part-", i, ".tif")), 
            overwrite = T)
}

# stop cluster
stopCluster(cl)

# SUMMARIZE RASTER DATA

# create list of pavement/parking raster parts from file
r.road.min.p <- lapply(1:my.cores, function (i) raster(here(paste0("data/outputs/temp/rasters/road-min-part-", i, ".tif"))))
r.road.max.p <- lapply(1:my.cores, function (i) raster(here(paste0("data/outputs/temp/rasters/road-max-part-", i, ".tif"))))

r.park.min.p <- lapply(1:my.cores, function (i) raster(here(paste0("data/outputs/temp/rasters/park-min-part-", i, ".tif"))))
r.park.max.p <- lapply(1:my.cores, function (i) raster(here(paste0("data/outputs/temp/rasters/park-max-part-", i, ".tif"))))

# merge all raster parts using sum function (function shouldn't really matter here w/ no overlaps)
r.road.min.p$fun <- sum
r.road.max.p$fun <- sum
r.road.min <- do.call(mosaic, r.road.min.p)
r.road.max <- do.call(mosaic, r.road.max.p)

r.park.min.p$fun <- sum
r.park.max.p$fun <- sum
r.park.min <- do.call(mosaic, r.park.min.p)
r.park.max <- do.call(mosaic, r.park.max.p)

# create raster stacks for min/max fractional area where each band is a different fclass
#r.road.min.s <- stack(here(paste0("data/outputs/temp/rasters/road-min-", features.min.r, ".tif"))) # points options
#r.road.max.s <- stack(here(paste0("data/outputs/temp/rasters/road-max-", features.max.r, ".tif"))) # points options

# summarize the data into a single raster
#r.road.min <- stackApply(r.road.min.s, indices = c(1), fun = sum)
#r.road.max <- stackApply(r.road.max.s, indices = c(1), fun = sum)

# make sure that if there are still overlapping of roads or abnormally high parking set max cell value to 1.0
values(r.road.min) <- ifelse(values(r.road.min) > 1.0, 1.0, values(r.road.min))
values(r.road.max) <- ifelse(values(r.road.max) > 1.0, 1.0, values(r.road.max))
values(r.park.min) <- ifelse(values(r.park.min) > 1.0, 1.0, values(r.park.min))
values(r.park.max) <- ifelse(values(r.park.max) > 1.0, 1.0, values(r.park.max))

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

# plots to check
plot(r.road.avg)
plot(r.park.avg)
plot(r.pave.avg) 

# create output directory if doesn't exist
dir.create(here("data/outputs/rasters"), showWarnings = F) 

# write out final road sum rasters
writeRaster(r.road.min, here("data/outputs/rasters/road-min.tif"), overwrite = T)
writeRaster(r.road.avg, here("data/outputs/rasters/road-avg.tif"), overwrite = T)
writeRaster(r.road.max, here("data/outputs/rasters/road-max.tif"), overwrite = T)

# write out final park sum rasters
writeRaster(r.park.min, here("data/outputs/rasters/park-min.tif"), overwrite = T)
writeRaster(r.park.avg, here("data/outputs/rasters/park-avg.tif"), overwrite = T)
writeRaster(r.park.max, here("data/outputs/rasters/park-max.tif"), overwrite = T)

# write out final total pave sum rasters
writeRaster(r.pave.min, here("data/outputs/rasters/pave-min.tif"), overwrite = T)
writeRaster(r.pave.avg, here("data/outputs/rasters/pave-avg.tif"), overwrite = T)
writeRaster(r.pave.max, here("data/outputs/rasters/pave-max.tif"), overwrite = T)

# save final image for backup 
save.image(here("data/outputs/temp/rasterize-data-3.RData"))

# paste final runtime
paste0("R model run complete on ", Sys.info()[4]," at ", Sys.time(),
       ". Model run length: ", round(difftime(Sys.time(), script.start, units = "mins"),2)," mins.")

