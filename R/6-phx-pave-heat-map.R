
######################
## HEAT MAP FIGURE  ##
######################

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
lib.path <- paste0(getwd(),"/.checkpoint/2019-01-01/lib/x86_64-w64-mingw32/3.5.1")
library(doParallel, lib.loc = lib.path, quietly = T, warn.conflicts = F)
library(foreach, lib.loc = lib.path, quietly = T, warn.conflicts = F)
library(XML, lib.loc = lib.path, quietly = T, warn.conflicts = F)
library(zoo, lib.loc = lib.path, quietly = T, warn.conflicts = F)
library(lubridate, lib.loc = lib.path, quietly = T, warn.conflicts = F)
library(sp, lib.loc = lib.path, quietly = T, warn.conflicts = F)
library(raster, lib.loc = lib.path, quietly = T, warn.conflicts = F)
library(rgdal, lib.loc = lib.path, quietly = T, warn.conflicts = F)
library(rgeos, lib.loc = lib.path, quietly = T, warn.conflicts = F)
library(maptools, lib.loc = lib.path, quietly = T, warn.conflicts = F)
library(cleangeo, lib.loc = lib.path, quietly = T, warn.conflicts = F)
library(gdalUtils, lib.loc = lib.path, quietly = T, warn.conflicts = F)
library(tmap, lib.loc = lib.path, quietly = T, warn.conflicts = F)
library(data.table, lib.loc = lib.path, quietly = T, warn.conflicts = F)
library(here, lib.loc = lib.path, quietly = T, warn.conflicts = F)

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
#osm <- intersect(osm, blkgrp)

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
registerDoParallel(cores = my.cores) # register parallel backend

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

# fix invalid geometery issues
#osm.cleaned.min <- clgeo_Clean(osm.buf.mrg.min)        # start w/ simple clean function
#osm.cleaned.min <- gSimplify(osm.cleaned.min, tol = 0.1)  # simplify polygons with Douglas-Peucker algorithm and a tolerance of 0.1 ft
#osm.cleaned.min <- gBuffer(osm.cleaned.min, width = 0)  # width = 0 as hack to clean polygon errors such as self intersetions

# dissolve the roadway buffer to a single polygon to calculate area w/o overlaps
osm.dissolved.min <- gUnaryUnion(osm.buf.mrg.min) #osm.cleaned.min
osm.dissolved.max <- gUnaryUnion(osm.buf.mrg.max) #osm.cleaned.max

# clip osm buffers by blockgroup boundaries
osm.block.min <- intersect(osm.dissolved.min, blkgrp) # better than gIntersection b/c it keeps attributes
osm.block.max <- intersect(osm.dissolved.max, blkgrp) # better than gIntersection b/c it keeps attributes


save.image(here("data/outputs/temp/phx-pave-heat-map.RData")) # save workspace
saveRDS(osm.block.min, here("data/outputs/temp/osm-blockgroup-dissolved-min.rds"))
saveRDS(osm.block.max, here("data/outputs/temp/osm-blockgroup-dissolved-min.rds"))



# calc roadway area by blockgroup
x <- 1
blkgrp$min.road.area <- ifelse(is.null(gIntersection(blkgrp[[y]][x,], osm.block)), 0, # check if no intersection and return 0 as area if null
                               gArea(gIntersection(blkgrp[[y]][x,], osm.block))) # otherwise calc the area of intersection with the road area

for(y in 1:length(blkgrp)){
  # calculate intersecting osm roadway area aroun each station buffer for each buffer distance
  blkgrp[[y]]$min.road.area <- sapply(1:length(stations.buffered[[y]]), # for all stations (sp features) in station buffer of index y 
                                             function(x) ifelse(is.null(gIntersection(stations.buffered[[y]][x,], osm.dissolved)), 0, # check if no intersection and return 0 as area if null
                                                                gArea(gIntersection(stations.buffered[[y]][x,], osm.dissolved)))) # otherwise calc the area of intersection with the road area
  }

## PARKING
# ignore on-street spaces for area calcs because part of roadway

# import parking data and parcel data to merge together
parking <- fread(here("data/phx-parking-blkgrp.csv"))

# calculate the area parking by blockgroup
#parcels.trimmed$parcel.full.area <- sapply(1:length(parcels.trimmed), function(x) gArea(parcels.trimmed[x,]))  

# merge parcel.full.area to station parcels sp list and keep duplicates b/c some parcels in multiple buffers that overlap
for(y in 1:length(station.parcels)){ 
  station.parcels[[y]] <- sp::merge(station.parcels[[y]], parcels.trimmed, by = "APN", duplicateGeoms = T, all.x = T)}


# calc parking area in station buffers and adjust total spaces if total parking area + roadway area > station buffer
# (which means there is multistory parking and we just assume there is a 100% road + parking coverage)
for(y in 1:length(stations.buffered)){
  stations.buffered[[y]]$park.prct <- sapply(1:length(stations.buffered[[y]]), # for all stations (sp features) in station buffer of index y
                                             function(x) stations.buffered[[y]][x,]$tot.park.area / stations.buffered[[y]][x,]$buffer.area) # calc the % parking area in buffer
  
  # create coverage area flag to mark which features have the issue of too much parking coverage
  stations.buffered[[y]]$park.cov.flag <- sapply(1:length(stations.buffered[[y]]), # for all stations (sp features) in station buffer of index y
                                                 # if the % of parking + road area is over 100%
                                                 function(x) ifelse(stations.buffered[[y]][x,]$park.prct + stations.buffered[[y]][x,]$road.prct > 1,
                                                                    1, 0)) # mark 1 for flag, 0 for not.
  
  # re-adjust total parking spaces if parking coverage is too high
  stations.buffered[[y]]$tot.park.spaces <- sapply(1:length(stations.buffered[[y]]), # for all stations (sp features) in station buffer of index y
                                                   # if the parking coverage is flagged (% of parking + road area is over 100%)
                                                   function(x) ifelse(stations.buffered[[y]][x,]$park.cov.flag == 1, 
                                                                      # then reduce spaces based by excess parking area
                                                                      stations.buffered[[y]][x,]$tot.park.spaces - floor((stations.buffered[[y]][x,]$tot.park.area + stations.buffered[[y]][x,]$road.area - stations.buffered[[y]][x,]$buffer.area)/(9*18)),
                                                                      # otherwise total spaces stays the same
                                                                      stations.buffered[[y]][x,]$tot.park.spaces))
  
  # recalculate parking percent of area (will be max 100% if road is 0%, road + park <= 100%)
  stations.buffered[[y]]$park.prct <- sapply(1:length(stations.buffered[[y]]), # for all stations (sp features) in station buffer of index y
                                             function(x) stations.buffered[[y]][x,]$tot.park.area / stations.buffered[[y]][x,]$buffer.area) # calc the % parking area in buffer
  
  # calculate pavement percent of area 
  stations.buffered[[y]]$pave.prct <- sapply(1:length(stations.buffered[[y]]), # for all stations (sp features) in station buffer of index y
                                             function(x) stations.buffered[[y]][x,]$park.prct + stations.buffered[[y]][x,]$road.prct) # calc the % parking area in buffer
}

# save things
save.image(here("data/outputs/temp/phx-pave-heat-map.RData")) # save workspace
saveRDS(osm, here("data/outputs/temp/osm.rds"))
