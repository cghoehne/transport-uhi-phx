
###################################################################
## OSM DATA IMPORT and FORMART to USE with WEATHER STATION DATA ##
#################################################################

# start by maxing out the memory allocation (use high number to force max)
gc()
memory.limit(size = 50000)
t.start <- Sys.time()

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
parking <- readRDS(here("data/phoenix-parking-parcel.rds")) # estimated parking space data for all Maricopa County by APN
station.parcels <- readRDS(here("data/shapefiles/processed/station-parcels.rds")) # pre-clipped to largest station buffer (r=2500ft) to save repo space

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
b <- list() # empty list for foreach
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

# ROADWAY AND PARKING DATA MERGE TO STATION BUFFERS
# calculate intersecting osm roadway area aroun each station buffer for each buffer distance
for(y in 1:length(stations.buffered)){
  stations.buffered[[y]]$road.area <- sapply(1:length(stations.buffered[[y]]), # for all stations (sp features) in station buffer of index y 
                                             function(x) gArea(gIntersection(stations.buffered[[y]][x,], osm.dissolved))) # calc the area of intersection with the road area
  
  # also calculate the percent area in each station buffer
  stations.buffered[[y]]$road.prct <- sapply(1:length(stations.buffered[[y]]), # for all stations (sp features) in station buffer of index y
                                             function(x) (stations.buffered[[y]][x,]$road.area / gArea(stations.buffered[[y]][x,]))) # calc the % road area in buffer
}

# calculate the # of spaces and area of parking within each station buffer for each buffer distance
# assume that for partial parcels, the area of parking scales with the fractional parcel area intersecting the buffer
# assume that parking + road area cannot be greater than 100% of the buffer area
# if parking area would put over 100% area, then assume there are parking garages and only use all the buffer area (e.g. 100% area assumed pavement)
# assume on-street spaces are 

# merge parking data to spatial parcel files (clipped to largest station buffer),
station.parking <- sp::merge(station.parcels, parking, by = "APN", duplicateGeoms = T) # multi geoms b/c some station buffers overlap so need duplicateGeoms = T 

# calculate area of parcels (in ft^2)
station.parking$parcel.area <- sapply(1:length(station.parking), function(x) gArea(station.parking[x,]))

# extract the merged parking data w/ station ids to data.table for new variable calcs
station.parking.dt <- as.data.table(station.parking@data)

# calculate the total number of parking spaces adjusting for partial parcel areas 
#station.parking.dt[, parking.area := ]

# calculate the total area of parking spaces adjusting for partial parcels areas

## TEMP ##
parcels <- readRDS(here("data/outputs/temp/all_parcels_mag.rds"))
parcels.keep <- parcels[parcels$APN %in% station.parking$APN,]
parcels.keep$parcel.full.area <- sapply(1:length(parcels.keep), function(x) gArea(parcels.keep[x,]))
station.parcels <- sp::merge(station.parcels, parcels.keep[,c("parcel.full.area","APN")], by = "APN", duplicateGeoms = T)

# save everything else
save.image(here("data/outputs/temp/sp-prep.RData")) # save workspace
saveRDS(stations.buffered, here("data/outputs/station-buffers-sp-list.rds")) # buffered station data as list of spatial r objects
saveRDS(osm.dissolved, here("data/outputs/osm-dissolved.rds")) # final osm cleaned/clipped/buffered/dissolved output as single spatial object

# shapefile outputs (to interactively investigate e.g. in QGIS)
shapefile(osm.dissolved, here("data/shapefiles/processed/osm_dissolved"), overwrite = T) # final osm cleaned/clipped/buffered/dissolved output
shapefile(stations.buffered[[6]], here("data/shapefiles/processed/stations_r2500ft_buffer"), overwrite = T) # shapefile output of largest station buffer
shapefile(uza.stations, here("data/shapefiles/processed/stations_pts"), overwrite = T) # station points shapefile

t.end <- Sys.time()
paste0("Completed task at ", t.end, ". Task took ", round(difftime(t.end,t.start, units = "mins"),1)," minutes to complete.")




