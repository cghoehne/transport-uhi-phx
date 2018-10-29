
###################################################################
## OSM DATA IMPORT and FORMART to USE with WEATHER STATION DATA ##
#################################################################

# start by maxing out the memory allocation (we use high number to force max allocation)
gc()
memory.limit(size = 56000) 
t.start <- Sys.time() # end script timestamp

# list of all dependant packages
list.of.packages <- c("tidyverse",
                      "data.table", 
                      "lubridate",
                      "rgdal",
                      "rgeos",
                      "raster",
                      "maptools",
                      "sp",
                      "cleangeo",
                      "tmap",
                      "doParallel",
                      "foreach",
                      "raster",
                      "here")

# install missing packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load packages
invisible(lapply(list.of.packages, library, character.only = TRUE)) # invisible() just hides printing stuff in console

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
radii.buffers <- c(50,100,200,500,1000,2500) # radii for buffer on each station point (in feet)
stations.buffered <- foreach(i = 1:length(radii.buffers), .packages = c("sp","rgeos"), .combine = c) %dopar% {
  gBuffer(uza.stations, byid = T, width = radii.buffers[i]) } # (stores as list of SpatialPointDataFrames)


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

# update new relevant vars so they appear in osm@data
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
saveRDS(w.stations.spdf, here("data/outputs/all-station-data.rds")) # saves station data (all) as spatial r object (points)
saveRDS(uza.stations, here("data/outputs/uza-station-data.rds")) # saves station data (uza) as spatial r object (points)
saveRDS(uza.weather, here("data/outputs/2017-uza-weather-data.rds")) # saves weather data filtered to only uza stations
saveRDS(osm.clip.station, here("data/outputs/2017-uza-weather-data.rds")) # saves clipped osm link data around stations (use for linking w/ traffic data)

# clean up space
rm(list=setdiff(ls(), c("my.cores", "stations.buffered", "osm.clip.station", "t.start")))
gc()
memory.limit(size = 56000)

# buffer clipped osm data
# foreach loop in parallel to buffer links
# (stores as list of SpatialPointDataFrames)
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


# PARKING

# calculate the # of spaces and area of parking within each station buffer for each buffer distance
# assume that for partial parcels, the area of parking scales with the fractional parcel area intersecting the buffer
# assume that parking + road area cannot be greater than 100% of the buffer area
# if parking area would put over 100% area, then assume there are parking garages and only use all the buffer area (e.g. 100% area assumed pavement)
# assume on-street spaces are 9x18 ft **FOR NOW**

# import parking data and parcel data to merge together
parking <- readRDS(here("data/phoenix-parking-parcel.rds")) # estimated parking space data for all Maricopa County by APN
#station.parcels <- readRDS(here("data/shapefiles/processed/station-parcels.rds")) # parcels pre-clipped station buffers to save space, 
# also has full parcel area calc'd. script for creating this object from larger parcel database is at bottom

# ** TEMP
# this data came with pre-clipped parcel data for which we then append parking data via parcel id
# if starting with larger parcel data, use the below script will
# load unclipped regional parcel data, create clipped parcels based on variable station buffers
# and filter raw parcels to parcels within station buffers to then calculate full parcel area (used for partial parking area calcs b/c some parcels intersect buffer boundary)
# merge full.parcel.area varaible back to clipped station.parcels with duplicates true to keep all instances (b/c if some buffers overlap, parcels can be in multiple station buffers)
parcels <- readRDS(here("data/outputs/temp/all_parcels_mag.rds")) # load full parcels in region
registerDoParallel(cores = my.cores) # register parallel backend
station.parcels <- foreach(i = 1:length(stations.buffered), .packages = c("sp","rgeos","raster"), .combine = c) %dopar% { 
  b[[i]] <- intersect(parcels, stations.buffered[[i]]) } # clip parcels to those w/in station buffers and store as list of SpatialPointDataFrames
parcels.keep <- parcels[parcels$APN %in% station.parcels[[6]]$APN,]  # create list of parcels that are within largest buffer (keeps all relevant parcels in station buffers)
parcels.keep$parcel.full.area <- sapply(1:length(parcels.keep), function(x) gArea(parcels.keep[x,]))  # calculate the full area of all relevant parcels
for(y in 1:length(station.parcels)){ # merge parcel.full.area to station parcels sp list and keep duplicates b/c some parcels in multiple buffers that overlap
  station.parcels[[y]] <- sp::merge(station.parcels[[y]], parcels.keep[,c("parcel.full.area","APN")], by = "APN", duplicateGeoms = T, all.x = T)}
saveRDS(station.parcels, here("data/shapefiles/processed/station-parcels.rds")) # save station parcel data 
save.image(here("data/outputs/temp/sp-prep-temp.RData")) # save temp workspace incase
rm(parcels)
gc()


# create parking data from parcel data as spatial dataframes in list
station.parking <- list() # create empty list 
for(y in 1:length(station.parcels)){ 
  
  # merge parking data to spatial parcel files and store in newly named list
  station.parking[[y]] <- sp::merge(station.parcels[[y]], parking, by = "APN", duplicateGeoms = T, all.x = T)
  
  # calculate area of parcels (in ft^2)
  station.parking[[y]]$parcel.area <- sapply(1:length(station.parking[[y]]), function(x) gArea(station.parking[[y]][x,]))
  }

# extract the merged parking data w/ station ids to data.table for new variable calcs
station.parking.dt <- as.data.table(station.parking@data)

# re-calculate the total number of parking spaces adjusting for partial parcel areas and calc parking area
station.parking.dt[, spaces := floor(spaces * parcel.area / parcel.full.area)]
station.parking.dt[, parking.area := (9*18) * spaces]
# so; parking area = (space size in ft) * (# of spaces at parcel) * (fraction of parcel area w/in station buffer)

# aggregate total parking spaces and area by station 
station.parking.dt.a <- station.parking.dt[, .(tot.park.area = sum(parking.area, na.rm = T),
                                               tot.park.spaces = sum(spaces, na.rm = T)),
                                           by = .(station.name)]

# merge the total parking spaces and area to the buffered station data by station.name
for(y in 1:length(stations.buffered)){
  stations.buffered[[y]] <- sp::merge(stations.buffered[[y]], station.parking.dt.a, by = "station.name", duplicateGeoms = T, all.x = T)
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
}

# save things
save.image(here("data/outputs/temp/sp-prep.RData")) # save workspace
saveRDS(stations.buffered, here("data/outputs/station-buffers-sp-list.rds")) # buffered station data as list of spatial r objects w/ all parking/road data

# shapefile outputs (to interactively investigate e.g. in QGIS)
shapefile(osm.dissolved, here("data/shapefiles/processed/osm_dissolved"), overwrite = T) # final osm cleaned/clipped/buffered/dissolved output
shapefile(stations.buffered[[6]], here("data/shapefiles/processed/stations_r2500ft_buffer"), overwrite = T) # shapefile output of largest station buffer
shapefile(uza.stations, here("data/shapefiles/processed/stations_pts"), overwrite = T) # station points shapefile

t.end <- Sys.time() # end script timestamp
paste0("Completed task at ", t.end, ". Task took ", round(difftime(t.end,t.start, units = "mins"),1)," minutes to complete.") # paste total script time
#load(here("data/outputs/temp/sp-prep.RData")) # if need to load previous data






