
###################################################################
## OSM DATA IMPORT and FORMART to USE with WEATHER STATION DATA ##
#################################################################

# start by maxing out the memory allocation (we use high number to force max allocation)
gc()
memory.limit(size = 56000) 
t.start <- Sys.time() # start script timestamp

# list of all dependant packages
list.of.packages <- c(
  "data.table",
                      "XML",
                      "rgdal",
                      "rgeos",
                      "maptools",
                      "sp",
                      "cleangeo",
                      "doParallel",
                      "foreach",
                      "raster",
                      "here")

# install missing packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load packages
invisible(lapply(list.of.packages, library, character.only = T)) # invisible() just hides printing stuff in console

# IMPORT DATA
#osm <- readOGR(here("data/shapefiles/osm/maricopa_county_osm_roads.shp"))
osm <- shapefile(here("data/shapefiles/osm/maricopa_county_osm_roads.shp")) # import OSM data (maricopa county clipped raw road network data)
fclass.info <- fread(here("data/osm_fclass_info.csv")) # additional OSM info by roadway functional class (fclass)
blkgrp <- shapefile(here("data/shapefiles/boundaries/phx-blkgrp-geom.shp")) # census blockgroup shapfile (clipped to Phoenix UZA)

traffic.net <- xmlParse(here("data/icarus network/optimizedNetwork.xml")) # traffic network from xml
traffic <- fread(here("data/icarus_osm_traffic.csv")) # import traffic data aggregated by osm link id and hour

traffic.net.dt <- xmlToList(traffic.net)
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
osm.dt <- merge(osm.dt, fclass.info, by = "fclass")

# merge new relevant vars back to spatial osm data so they appear in osm@data
osm <- merge(osm, osm.dt[, .(osm_id, pave.type, descrip)], by = "osm_id")

# remove unnecessary variables before exporting
osm$ref <- NULL # irrelevant variable
osm$layer <- NULL # irrelevant variable
osm$maxspeed <- NULL # most are 0 or missing so unuseable in this case

# clean up space
rm(list=setdiff(ls(), c("stations.buffered","osm.clip.station","t.start","radii.buffers")))
gc()
memory.limit(size = 56000)

#  "min.2w.width.m"
#  "max.2w.width.m"

# clip osm buffers by blockgroup boundaries
#osm.block <- intersect(osm, blkgrp) # better than gIntersection b/c it keeps attributes

# buffer clipped osm data twice, for range of likely roadway area (min and max roadway widths)
# foreach loop in parallel to buffer links
# (stores as list of SpatialPointDataFrames)
my.cores <- parallel::detectCores()  # store computers cores
registerDoParallel(cores = my.cores) # register parallel backend
w.min <- unique(osm$min.2w.width.m) / 2 # list of unique buffer road widths to buffer radius
b.min <- list() # create empty list for foreach
osm.buf <- foreach(i = 1:length(w.min), .packages = c("sp","rgeos")) %dopar% {
  b[[i]] <- gBuffer(osm.clip.station[(osm$min.2w.width.m / 2) == w.min[i], ], byid = F, width = w.min[i], capStyle = "ROUND") # round b/c end of roads are usually cul-de-sac
} # buffer task could be seperated to two tasks by pave.type if we eventually want to estimate concrete vs. asphalt area, but for now assume all asphalt

# bind the buffered osm data output
osm.buf.mrg <- do.call(raster::bind, osm.buf) # bind list of spatial objects into single spatial obj

# fix invalid geometery issues
osm.cleaned <- clgeo_Clean(osm.buf.mrg)        # start w/ simple clean function
osm.cleaned <- gSimplify(osm.cleaned, tol = 0.1)  # simplify polygons with Douglas-Peucker algorithm and a tolerance of 0.1 ft
osm.cleaned <- gBuffer(osm.cleaned, width = 0)  # width = 0 as hack to clean polygon errors such as self intersetions

# dissolve the roadway buffer to a single polygon to calculate area w/o overlaps
osm.dissolved <- gUnaryUnion(osm.cleaned)

# clip osm buffers by blockgroup boundaries
osm.block <- intersect(osm.uza.car, blkgrp) # better than gIntersection b/c it keeps attributes

# calc roadway area by blockgroup
for(y in 1:length(stations.buffered)){
  # calculate intersecting osm roadway area aroun each station buffer for each buffer distance
  stations.buffered[[y]]$road.area <- sapply(1:length(stations.buffered[[y]]), # for all stations (sp features) in station buffer of index y 
                                             function(x) ifelse(is.null(gIntersection(stations.buffered[[y]][x,], osm.dissolved)), 0, # check if no intersection and return 0 as area if null
                                                                gArea(gIntersection(stations.buffered[[y]][x,], osm.dissolved)))) # otherwise calc the area of intersection with the road area
  
  # calculate the area of station buffer (for % area calc). Note it is the same for all features, same buffer
  stations.buffered[[y]]$buffer.area <- sapply(1:length(stations.buffered[[y]]), # for all stations (sp features) in station buffer of index y
                                               function(x) gArea(stations.buffered[[y]][x,])) # calc the station area buffer
  
  # calculate the % area of roadway w/in buffer
  stations.buffered[[y]]$road.prct <- sapply(1:length(stations.buffered[[y]]), # for all stations (sp features) in station buffer of index y
                                             function(x) stations.buffered[[y]][x,]$road.area / stations.buffered[[y]][x,]$buffer.area) # calc the % roadway area in buffer
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


# calc parking % area in station buffers and adjust total spaces if total parking area + roadway area > station buffer
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
