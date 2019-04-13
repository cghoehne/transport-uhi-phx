####################
## OSM DATA PREP  ##
####################

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
#osm <- readOGR(here("data/shapefiles/osm/maricopa_county_osm_roads.shp"))
osm <- shapefile(here("data/shapefiles/osm/maricopa_county_osm_roads.shp")) # import OSM data (maricopa county clipped raw road network data)
fclass.info <- fread(here("data/osm_fclass_info.csv")) # additional OSM info by roadway functional class (fclass)
blkgrp <- shapefile(here("data/shapefiles/boundaries/phx-blkgrp-geom.shp")) # census blockgroup shapfile (clipped to Maricopa UZA)
uza.buffer <- readRDS(here("data/outputs/temp/uza-buffer.rds")) # Maricopa UZA buffered ~1mi


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

# calc min/max road width based on 2way widths and 1way/2way ("B" == both, "T"/"F" True/False for driving in the opposite dir of linestring)
osm.dt[oneway == "B", max.width := as.numeric(max.2w.width.m)]
osm.dt[oneway == "B", min.width := as.numeric(min.2w.width.m)]
osm.dt[oneway == "T" | oneway == "F", max.width := as.numeric(max.2w.width.m / 2)]
osm.dt[oneway == "T" | oneway == "F", min.width := as.numeric(min.2w.width.m / 2)]

# merge filtered data with min/max roadway widths to spatial osm data
osm <- merge(osm, osm.dt[, .(osm_id, min.width, max.width)], by = "osm_id", all.x = F)

# remove tunnel and oneway as no longer needed
osm$tunnel <- NULL
osm$oneway <- NULL

# create node spatial file where a node is created at each link intersection 
# store the 2way widths of each intersection fclass with adjustment for direction
fclasses <- unique(osm$fclass)
test <- gNode(osm)

# clean up space
rm(list=setdiff(ls(), c("osm","blkgrp","script.start")))
gc()
memory.limit(size = 56000)

# create empty raster at desired extent (use uza buffer to ensure everything captured)
r <- raster(ext = extent(uza.buffer), crs = crs(uza.buffer), res = 32.8084) # create raster = ~10 x 10 m
#r <- raster(ext = extent(uza.buffer), crs = crs(uza.buffer), res = 3280.84) # create raster = ~1000 x 1000 m

# calculate the number of cores
my.cores <- parallel::detectCores() - 1 # store computers cores

# number of polygons features in SPDF
features <- 1:nrow(osm[,])

# split features in n parts
n <- my.cores
parts <- split(features, cut(features, n))

# initiate cluster after loading all the necessary object to R environment
cl <- makeCluster(my.cores)
registerDoParallel(cl) # register parallel backend
clusterCall(cl, function(x) .libPaths(x), .libPaths())
print(cl)

# rasterize parts of min/max road network area and save in parellel
system.time(foreach(i = 1:n, .packages = c("raster", "here")) %dopar% {
  rasterize(osm[parts[[i]],], r, getCover = T, background = 0, 
            filename = here(paste0("data/outputs/temp/road-min-part-", i, ".tif")), 
            overwrite = T)})

# stop cluster
stopCluster(cl)
