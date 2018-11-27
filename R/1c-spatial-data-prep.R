
###################################################################
## OSM DATA IMPORT and FORMART to USE with WEATHER STATION DATA ##
#################################################################

# start by maxing out the memory allocation (we use high number to force max allocation)
gc()
memory.limit(size = 56000) 
t.start <- Sys.time() # start script timestamp

# list of all dependant packages
list.of.packages <- c("tidyverse",
                      "data.table", 
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
fclass.info <- fread(here("data/shapefiles/osm/fclass_info.csv")) # additional OSM info by roadway functional class (fclass)
radii.buffers <- readRDS(here("data/outputs/radii-buffers.rds")) # vector of station buffer radii chosen
stations.buffered <- readRDS(here("data/outputs/temp/stations-buffered-prep.rds")) # buffered stations by various buffer radii chosen
uza.buffer <- readRDS(here("data/outputs/temp/uza-buffer.rds"))

# OSM DATA FORMAT
# **Curently, OSM data in Phoenix does not have lane data. in light of this:
# assign roadway widths based on roadway fclass 
# assume roadway widths are constant by fclass
# 1way roadway width is based on typical regional street design 
# and average observed roadway widths by functional class
# this data is all proj in EPSG 2223 and units are in feet

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

# import traffic data aggregated by osm link id and hour
traffic <- fread(here("data/icarus-osm-traffic.csv"))
setnames(traffic, "link_id", "osm_id") 

# merge traffic data to osm data by link id
#osm.dt <- merge(osm.dt, traffic, by = "id")

# merge new relevant vars back to spatial osm data so they appear in osm@data
osm.uza <- merge(osm.uza, osm.dt[, .(osm_id, auto.use, pave.type, buf.ft, descrip)], by = "osm_id")

# remove unnecessary variables before exporting
osm.uza$ref <- NULL # irrelevant variable
osm.uza$layer <- NULL # irrelevant variable
osm.uza$maxspeed <- NULL # most are 0 or missing so unuseable in this case

# filter to only car based links (this also filters all remaining pavement.types to "concrete" under current assumptions)
osm.uza.car <- subset(osm.uza, auto.use == "Y" & !is.na(buf.ft) & buf.ft > 0) # also make sure to exclude NA and 0 width roads

# clip osm links by largest weather station radii
# b/c we don't need all the network so this greatly reduces file size moving forward
osm.clip.station <- intersect(osm.uza.car, stations.buffered[[length(stations.buffered)]]) # better than gIntersection b/c it keeps attributes

# save some data 
saveRDS(osm.clip.station, here("data/outputs/2017-uza-weather-data.rds")) # saves clipped osm link data around stations (use for linking w/ traffic data)

# clean up space
rm(list=setdiff(ls(), c("stations.buffered","osm.clip.station","t.start","radii.buffers")))
gc()
memory.limit(size = 56000)

# buffer clipped osm data
# foreach loop in parallel to buffer links
# (stores as list of SpatialPointDataFrames)
my.cores <- parallel::detectCores()  # store computers cores
registerDoParallel(cores = my.cores) # register parallel backend
w <- unique(osm.clip.station$buf.ft) # list of unique buffer widths
b <- list() # create empty list for foreach
osm.buffered <- foreach(i = 1:length(w), .packages = c("sp","rgeos")) %dopar% {
  b[[i]] <- gBuffer(osm.clip.station[osm.clip.station$buf.ft == w[i], ], byid = F, width = w[i], capStyle = "FLAT") # buf.ft is already adjusted to correct buffer distances
} # buffer task could be seperated to two tasks by pave.type if we eventually want to estimate concrete vs. asphalt area, but for now assume all asphalt

# bind the buffered osm data output
osm.buffered.m <- do.call(raster::bind, osm.buffered) # bind list of spatial objects into single spatial obj

# fix invalid geometery issues
osm.cleaned <- clgeo_Clean(osm.buffered.m)        # start w/ simple clean function
osm.cleaned <- gSimplify(osm.cleaned, tol = 0.1)  # simplify polygons with Douglas-Peucker algorithm and a tolerance of 0.1 ft
osm.cleaned <- gBuffer(osm.cleaned, width = 0)  # width = 0 as hack to clean polygon errors such as self intersetions

# dissolve the roadway buffer to a single polygon to calculate area w/o overlaps
osm.dissolved <- gUnaryUnion(osm.cleaned)

# calc some new variables in the list of spatial objects using sapply
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
# calculate the # of spaces and area of parking within each station buffer for each buffer distance
# assume that for partial parcels, the area of parking scales with the fractional parcel area intersecting the buffer
# assume that parking + road area cannot be greater than 100% of the buffer area
# if parking area would put over 100% area, then assume there are parking garages and only use all the buffer area (e.g. 100% area assumed pavement)
# assume on-street spaces are 9x18 ft for area calcs

# import parking data and parcel data to merge together
parking <- readRDS(here("data/phoenix-parking-parcel.rds")) # estimated parking space data for all Maricopa County by APN

# parcels.trimmed is derived from all of regions parcels via script 1b-parcel-preprocessing.R
parcels.trimmed <- readRDS(here("data/outputs/temp/parcels-trimmed.rds"))

# clip parcels to those w/in station buffers and store as list of SpatialPointDataFrames
registerDoParallel(cores = my.cores) # register parallel backend
b <- list() # empty list
station.parcels <- foreach(i = 1:length(stations.buffered), .packages = c("sp","rgeos","raster"), .combine = c) %dopar% { 
  b[[i]] <- intersect(parcels.trimmed, stations.buffered[[i]]) } 

# calculate the full area of all relevant parcels
parcels.trimmed$parcel.full.area <- sapply(1:length(parcels.trimmed), function(x) gArea(parcels.trimmed[x,]))  

# merge parcel.full.area to station parcels sp list and keep duplicates b/c some parcels in multiple buffers that overlap
for(y in 1:length(station.parcels)){ 
  station.parcels[[y]] <- sp::merge(station.parcels[[y]], parcels.trimmed, by = "APN", duplicateGeoms = T, all.x = T)}

# create parking data from parcel data as spatial dataframes in list

# create empty lists for to store lists of spatial or data.table objects (one for each buffer size)
station.parking <- list() 
station.parking.dt <- list()
station.parking.dt.a <- list()

for(y in 1:length(station.parcels)){ 
  
  # merge parking data to spatial parcel files and store in newly named list
  station.parking[[y]] <- sp::merge(station.parcels[[y]], parking, by = "APN", duplicateGeoms = T, all.x = T)
  
  # calculate actual area of parcels (in ft^2)
  station.parking[[y]]$parcel.area <- sapply(1:length(station.parking[[y]]), function(x) gArea(station.parking[[y]][x,]))
  
  # extract the merged parking data w/ station ids to data.table for new variable calcs
  station.parking.dt[[y]] <- as.data.table(station.parking[[y]]@data)
  
  # re-calculate the total number of parking spaces adjusting for partial parcel areas and calc parking area
  station.parking.dt[[y]][, spaces := floor(spaces * parcel.area / parcel.full.area)]
  station.parking.dt[[y]][, parking.area := (9*18) * spaces]
  # so; parking area = (space size in ft) * (# of spaces at parcel) * (fraction of parcel area w/in station buffer)
  
  # aggregate total parking spaces and area by station 
  station.parking.dt.a[[y]] <- station.parking.dt[[y]][, .(tot.park.area = sum(parking.area, na.rm = T),
                                                           tot.park.spaces = sum(spaces, na.rm = T)),
                                                       by = .(station.name)]
}

# merge the total parking spaces and area to the buffered station data by station.name
for(y in 1:length(stations.buffered)){
  stations.buffered[[y]] <- sp::merge(stations.buffered[[y]], station.parking.dt.a[[y]], by = "station.name", duplicateGeoms = T, all.x = T)
}

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
save.image(here("data/outputs/temp/sp-prep.RData")) # save workspace
#load(here("data/outputs/temp/sp-prep.RData"))
saveRDS(stations.buffered, here("data/outputs/station-buffers-sp-list.rds")) # buffered station data as list of spatial r objects w/ all parking/road data
#shapefile(osm.dissolved, here("data/shapefiles/processed/osm_dissolved"), overwrite = T) # final osm cleaned/clipped/buffered/dissolved output
r <- which(radii.buffers == 200) # save the shapefile where the raduis of buffer is 200ft
shapefile(stations.buffered[[r]], here(paste0("data/shapefiles/processed/stations_r",radii.buffers[r],"ft_buffer")), overwrite = T) # shapefile output of largest station buffer

# print script endtime
t.end <- Sys.time() 
paste0("Completed task at ", t.end, ". Task took ", round(difftime(t.end,t.start, units = "mins"),1)," minutes to complete.") # paste total script time

shapefile(station.parcels[[1]], here("data/shapefiles/processed/station-parcels"), overwrite = T) # shapefile output of largest station buffer


