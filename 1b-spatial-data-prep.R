###################################################################
## OSM DATA IMPORT and FORMART to USE with WEATHER STATION DATA ##
#################################################################

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

# import data
w.stations <- readRDS(here("data/station-data.rds")) # import cleaned station data
uza.border <- shapefile(here("data/shapefiles/boundaries/maricopa_county_uza.shp")) # import uza boundary

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
weather.data <- readRDS(here("data/2017-weather-data.rds"))
uza.weather <- weather.data[station.name %in% uza.stations.list$station.name]

# foreach loop in parallel to buffer stations points by multiple radii
# note that the foreach loop in parallel probably isn't necessary
my.cores <- parallel::detectCores()  # store computers cores
registerDoParallel(cores = my.cores) # register parallel backend
radii.buffers <- c(50,100,200,500,1000,2500) # radii for buffer on each station point
stations.buffered <- foreach(i = 1:length(radii.buffers), .packages = c("sp","rgeos"), .combine = c) %dopar% {
  gBuffer(uza.stations, byid = T, width = radii.buffers[i]) } # (stores as list of SpatialPointDataFrames)

# save formatted spatial data for analysis scripts 
saveRDS(stations.buffered, here("data/station-buffers-sp-list.rds")) # saves buffered station data as list of spatial r objects
saveRDS(uza.stations, here("data/uza-station-data.rds")) # saves station data as spatial r object (points)
saveRDS(uza.weather, here("data/2017-uza-weather-data.rds")) # saves weather data filtered to only uza stations

shapefile(stations.buffered[[3]], here("data/shapefiles/stations_200m_buffered"), overwrite = T)
shapefile(uza.stations, here("data/shapefiles/stations_pts"), overwrite = T)
