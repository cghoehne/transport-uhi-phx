####################
## OSM DATA PREP  ##
####################

# ** It is recommended to use Microsoft Open R (v3.5.1) for improved performance without compromsing compatibility **

## SCRIPT PREPERATION

# clear space and allocate memory
gc()
memory.limit(size = 56000) 
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
library(doParallel, quietly = T, warn.conflicts = F)
library(foreach, quietly = T, warn.conflicts = F)
library(doParallel, quietly = T, warn.conflicts = F)
library(foreach, quietly = T, warn.conflicts = F)
library(sp, quietly = T, warn.conflicts = F)
library(raster, quietly = T, warn.conflicts = F)
library(rgdal, quietly = T, warn.conflicts = F)
library(rgeos, quietly = T, warn.conflicts = F)
library(maptools, quietly = T, warn.conflicts = F)
library(cleangeo, quietly = T, warn.conflicts = F)
library(gdalUtils, quietly = T, warn.conflicts = F)
library(data.table, quietly = T, warn.conflicts = F)
library(here, quietly = T, warn.conflicts = F)

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

#traffic.net <- xmlParse(here("data/icarus network/optimizedNetwork.xml")) # traffic network from xml
#traffic <- fread(here("data/icarus_osm_traffic.csv")) # import traffic data aggregated by osm link id and hour
#traffic.net.dt <- xmlToList(traffic.net)
# setnames(traffic, "link_id", "osm_id") 
# merge traffic data to osm data by link id
#osm.dt <- merge(osm.dt, traffic, by = "id")


# OSM DATA FORMAT
# **Curently, OSM data in Phoenix does not have lane data. in light of this:
# assign roadway widths based on roadway fclass 
# assume roadway widths are constant by fclass
# 1way roadway width is based on typical regional street design 
# and average observed roadway widths by functional class
# this data is all proj in EPSG 2223 and units are in feet

# transform osm crs to EPSG:2223
osm <- spTransform(osm, crs(uza.buffer))

# store osm data for quick calcs of new variables and rebind later
osm.dt <- as.data.table(osm@data)

# merge fclass.info with osm data
osm.dt <- merge(osm.dt, fclass.info, by = "fclass", all.x = T)

# drop tunnels, and non pavement fclass
osm.dt <- osm.dt[tunnel == "F" & min.2w.width.m > 0 
                 & !(fclass %in% c("bridleway", "path", "track_grade3",
                                   "track_grade4", "track_grade5", "unknown")),]

# calc min/max buffer radius (in ft from m) of each road (to account for oneway roads)
osm.dt[, min.r.buf := min.2w.width.m * ifelse(oneway == "B", 0.5, 0.25) * 3.28084]
osm.dt[, max.r.buf := max.2w.width.m * ifelse(oneway == "B", 0.5, 0.25) * 3.28084]

# merge new relevant vars back to spatial osm data so they appear in osm@data w/ excluded fclasses and tunnels
osm <- merge(osm, osm.dt[, .(osm_id, pave.type, descrip, min.r.buf, max.r.buf)], by = "osm_id", all.x = F)

# remove unnecessary variables before exporting
osm$ref <- NULL # irrelevant variable
osm$layer <- NULL # irrelevant variable
osm$maxspeed <- NULL # most are 0 or missing so unuseable in this case

# clip osm data to buffer uza for quicker computing
osm <- intersect(osm, uza.buffer) # better than gIntersection b/c it keeps attributes

# save out as backup for QGIS 
#shapefile(osm, here("data/shapefiles/processed/osm-uza-processed"), overwrite = T) # station points shapefile

# clean up space
rm(list=setdiff(ls(), c("osm","blkgrp","script.start")))
gc()
memory.limit(size = 56000)

# buffer clipped osm data twice, for range of likely roadway area (min and max roadway widths)
# foreach loop in parallel to buffer links
# (stores as list of SpatialPointDataFrames)
my.cores <- parallel::detectCores()  # store computers cores
cl <- makeCluster(my.cores)
registerDoParallel(cl) # register parallel backend
clusterCall(cl, function(x) .libPaths(x), .libPaths())

w.min <- unique(osm$min.r.buf)  # list of unique min buffer widths based on oneway and estiamted roadway width
b.min <- list() # create empty list for foreach
osm.buf.min <- foreach(i = 1:length(w.min), .packages = c("sp","rgeos")) %dopar% {
  #b.min[[i]] <- buffer(osm[osm$min.r.buf == w.min[i], ], width = w.min[i])
  b.min[[i]] <- gBuffer(osm[osm$min.r.buf == w.min[i], ], byid = F, width = w.min[i], capStyle = "ROUND") # round b/c end of roads are usually cul-de-sac
} # buffer task could be seperated to two tasks by pave.type if we eventually want to estimate concrete vs. asphalt area, but for now assume all asphalt

w.max <- unique(osm$max.r.buf)  # list of unique min buffer widths based on oneway and estiamted roadway width
b.max <- list() # create empty list for foreach
osm.buf.max <- foreach(i = 1:length(w.max), .packages = c("sp","rgeos")) %dopar% {
  #b.max[[i]] <- buffer(osm[osm$max.r.buf == w.max[i], ], width = w.max[i])
  b.max[[i]] <- gBuffer(osm[osm$max.r.buf == w.max[i], ], byid = F, width = w.max[i], capStyle = "ROUND") # round b/c end of roads are usually cul-de-sac
} # buffer task could be seperated to two tasks by pave.type if we eventually want to estimate concrete vs. asphalt area, but for now assume all asphalt

# bind the buffered osm data output
osm.buf.mrg.min <- do.call(raster::bind, osm.buf.min) # bind list of spatial objects into single spatial obj
osm.buf.mrg.max <- do.call(raster::bind, osm.buf.max) # bind list of spatial objects into single spatial obj

# options for fixing invalid geometery issues if needed
#osm.cleaned.min <- clgeo_Clean(osm.buf.mrg.min)        # start w/ simple clean function
#osm.cleaned.min <- gSimplify(osm.cleaned.min, tol = 0.1)  # simplify polygons with Douglas-Peucker algorithm and a tolerance of 0.1 ft
#osm.cleaned.min <- gBuffer(osm.cleaned.min, width = 0)  # width = 0 as hack to clean polygon errors such as self intersetions

# dissolve the roadway buffer to a single polygon to calculate area w/o overlaps
osm.dissolved.min <- gUnaryUnion(osm.buf.mrg.min) #osm.cleaned.min
osm.dissolved.max <- gUnaryUnion(osm.buf.mrg.max) #osm.cleaned.max

# clip osm buffers by blockgroup boundaries
osm.block.min <- intersect(osm.dissolved.min, blkgrp) # better than gIntersection b/c it keeps attributes
osm.block.max <- intersect(osm.dissolved.max, blkgrp) # better than gIntersection b/c it keeps attributes

# save
save.image(here("data/outputs/temp/phx-pave-heat-map.RData")) # save workspace
saveRDS(osm.block.min, here("data/outputs/osm-blockgroup-dissolved-min.rds"))
saveRDS(osm.block.max, here("data/outputs/osm-blockgroup-dissolved-max.rds"))

# for QGIS
shapefile(osm.block.min, here("data/outputs/temp/osm-blkgrp-dissolved-min"), overwrite = T)
shapefile(osm.block.max, here("data/outputs/temp/osm-blkgrp-dissolved-max"), overwrite = T)

