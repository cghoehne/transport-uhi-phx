
###################################################################
## OSM DATA IMPORT and FORMART to USE with WEATHER STATION DATA ##
#################################################################

# start by setting the useable memory equivalent to the installed RAM
gc()
memory <- memory.limit()
memory.size(max = memory)

# list of all dependant packages
list.of.packages <- c("tidyverse",
                      "data.table", 
                      "lubridate",
                      "rgdal",
                      "rgeos",
                      "raster",
                      "maptools",
                      "sp",
                      "tmap",
                      "doParallel",
                      "foreach",
                      "raster",
                      "here")

# install missing packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load packages
lapply(list.of.packages, library, character.only = TRUE)


# IMPORT DATA
w.stations <- readRDS(here("data/outputs/station-data.rds")) # import cleaned station data
uza.border <- shapefile(here("data/shapefiles/boundaries/maricopa_county_uza.shp")) # import Maricopa County UZA boundary
osm <- shapefile(here("data/shapefiles/osm/maricopa_county_osm_roads.shp")) # import OSM data (maricopa county clipped raw road network data)
fclass.info <- fread(here("data/shapefiles/osm/fclass_info.csv")) # additional OSM info by roadway functional class (fclass)

# STATION DATA FORMAT
# convert stations data.table w/ lat-lon to coordinates (SpatialPointDataFrame)
w.stations.spdf <- SpatialPointsDataFrame(coords = w.stations[, .(lon,lat)], data = w.stations,
                                          proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# transform stations crs to same as other shapefiles 
w.stations.spdf <- spTransform(w.stations.spdf, crs(uza.border))

# buffer uza boundary by 1 mile
uza.buffer <- gBuffer(uza.border, byid = F, width = 5280)

# clip station points to ones w/in uza 1mi buffer
uza.stations <- raster::intersect(w.stations.spdf, uza.buffer)

# filter station.list to uza stations
uza.stations.list <- w.stations[station.name %in% uza.stations$station.name]

# import and filter weather data to only stations in uza
weather.data <- readRDS(here("data/outputs/2017-weather-data.rds"))
uza.weather <- weather.data[station.name %in% uza.stations.list$station.name]

# foreach loop in parallel to buffer stations points by multiple radii
# note that the foreach loop in parallel probably isn't necessary
my.cores <- parallel::detectCores()  # store computers cores
registerDoParallel(cores = my.cores) # register parallel backend
radii.buffers <- c(50,100,200,500,1000,2500) # radii for buffer on each station point
stations.buffered <- foreach(i = 1:length(radii.buffers), .packages = c("sp","rgeos"), .combine = c) %dopar% {
  gBuffer(uza.stations, byid = T, width = radii.buffers[i]) } # (stores as list of SpatialPointDataFrames)


# OSM DATA FORMAT
# **Curently, OSM data in Phoenix does not have lane data. in light of this:
# assign roadway widths based on roadway fclass 
# assume roadway widths are constant by fclass
# 1way roadway width is based on typical regional street design 
# and average observed roadway widths by functional class
# proj is EPSG 2223 and units are in feet

# transform osm crs to EPSG:2223
osm <- spTransform(osm, crs(uza.buffer))

# clip osm data to the buffered uza
osm.uza <- raster::intersect(osm, uza.buffer)

# store osm data for quick calcs of new variables and rebind later
osm.dt <- as.data.table(osm.uza@data)

# merge fclass.info with osm data
osm.dt <- merge(osm.dt, fclass.info, by = "fclass")

# create id column for fclasses where cars are unsuitable or prohibited. assume 'unknown' is unsuitable without info to assume otherwise
# open the data dic to see details on this (page 15) by running in console: getOption('viewer')(here('data/shapefiles/osm/osm-data-dictionary.pdf'))
osm.dt[,auto.use := "Y"]
osm.dt[fclass %in% c("bridleway","cycleway","footway","path","steps", "pedestrian", "unknown"), auto.use := "N"]

# also id fclasses of 'track' as 'P' for partial use/suitablity. data dic: "For agricultural use, in forests, etc. Often gravel roads."
osm.dt[fclass %in% c("track","track_grade1","track_grade2","track_grade3","track_grade4","track_grade5"), auto.use := "P"]

# create final buffer variable (in feet b/c proj is uses units of feet)
osm.dt[, buf.ft := ifelse(oneway == "B", wdth.1way.ft,  # if roadway is bi-directional the buffer radius is equal to the one-way roadway width in feet
                          wdth.1way.ft / 2)] # otherwise divide by 2 as only 1 of 2 directions: buffer raduis = half of 1way width

# update new relevant vars so they appear in osm@data
osm.uza <- merge(osm.uza, osm.dt[, .(osm_id, auto.use, pave.type, buf.ft, descrip)], by = "osm_id")

# remove unnecessary variables before exporting
osm.uza$ref <- NULL # irrelevant variable
osm.uza$layer <- NULL # irrelevant variable
osm.uza$maxspeed <- NULL # most are 0 or missing so unuseable in this case

# filter to only car based paths (this also currenlty assumes all remaining are pavement.type == "concrete")
osm.uza.car <- subset(osm.uza, auto.use == "Y" & !is.na(buf.ft) & buf.ft > 0) # also make sure to exclude NA and 0 width roads

# buffer osm data
# foreach loop in parallel to buffer links
# (stores as list of SpatialPointDataFrames)
registerDoParallel(cores = my.cores) # register parallel backend
w <- unique(osm.uza.car$buf.ft) # list of unique buffer widths
b <- list() # empty list for foreach
osm.links.buffered <- foreach(i = 1:length(w), .packages = c("sp","rgeos")) %dopar% {
  b[[i]] <- gBuffer(osm.uza.car[osm.uza.car$buf.ft == w[i], ], byid = F, width = w[i], capStyle = "FLAT") # buf.ft is already adjusted to correct buffer distances
} # byid = FALSE means the output is 'disolved' which is fine b/c roadways will overlap & we only need the buffer to calc pavement areas 
# buffer task could be seperated to two tasks by pave.type if we eventually want to estimate concrete vs. asphalt area, but for now assume all asphalt

# bind and dissolve the buffered osm data output
osm.links.buffered.m <- do.call(raster::bind, osm.links.buffered) # bind list of spatial objects into single spatial obj

# SAVE DATA FOR ANALYSIS
saveRDS(stations.buffered, here("data/outputs/station-buffers-sp-list.rds")) # saves buffered station data as list of spatial r objects
saveRDS(osm.links.buffered, here("data/outputs/osm-links-buffers-sp-list.rds")) # saves buffered osm data as list of spatial r objects
saveRDS(uza.stations, here("data/outputs/uza-station-data.rds")) # saves station data as spatial r object (points)
saveRDS(uza.weather, here("data/outputs/2017-uza-weather-data.rds")) # saves weather data filtered to only uza stations

# clean up space, then dissolve the roadway buffer before saving as this is by far the most time/memory intensive task
rm(list=setdiff(ls(), "osm.links.buffered.m"))
gc()
osm.links.buffered.dissolved <- union(osm.links.buffered.m) # very slow (2+ hrs)
#osm.links.buffered.dissolved <- unionSpatialPolygons(osm.links.buffered.dissolved) # alternative, also slow
saveRDS(osm.links.buffered.dissolved, here("data/outputs/osm_buffered_dissolved.rds")) 
shapefile(osm.links.buffered.dissolved, here("data/outpupts/temp/osm_buffered_dissolved"), overwrite = T)


# TEMP FILESAVES FOR TESTING
saveRDS(osm.uza.car, here("data/outputs/temp/osm-uza-car.rds"))
#osm.uza.car <- readRDS(here("data/outputs/temp/osm-uza-car.rds"))

for(i in 1:length(w)){  # save osm roadway data to examine in QGIS for errors (TEMPORARY)
  shapefile(osm.links.buffered[[i]], here(paste0("data/outputs/temp/osm_buffered_",floor(w[i]),"ft")))}
#shapefile(stations.buffered[[3]], here("data/outpupts/temp/stations_200m_buffered"), overwrite = T)
#shapefile(uza.stations, here("data/outputs/temp/stations_pts"), overwrite = T)
#shapefile(osm.uza.car, here("data/outputs/temp/osm_uza_car"), overwrite = T)

