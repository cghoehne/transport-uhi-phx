# this script pre-processes spatial data in prepareration for analysis

# start by maxing out the memory allocation (we use high number to force max allocation)
gc()
t.start <- Sys.time() # start script timestamp
#memory.limit(size = 56000) 

# list of all dependant packages
list.of.packages <- c("tidyverse",
                      "data.table", 
                      "rgdal",
                      "rgeos",
                      "maptools",
                      "sp",
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
w.stations <- readRDS(here("data/outputs/station-data.rds")) # import cleaned station data
uza.border <- shapefile(here("data/shapefiles/boundaries/maricopa_county_uza.shp")) # import Maricopa County UZA boundary

# convert stations data.table w/ lat-lon to coordinates (SpatialPointDataFrame)
longlat.prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
w.stations.spdf <- SpatialPointsDataFrame(coords = w.stations[, .(lon,lat)], data = w.stations,
                                          proj4string = CRS(longlat.prj))

# transform stations crs to same as other shapefiles 
w.stations.spdf <- spTransform(w.stations.spdf, crs(uza.border))

# buffer uza boundary by 1 mile (5,280 ft or ~1.6 km)
uza.buffer <- gBuffer(uza.border, byid = F, width = 5280)

# clip station points to ones w/in uza 1mi buffer
uza.stations <- raster::intersect(w.stations.spdf, uza.buffer)

# foreach loop in parallel to buffer stations points by multiple radii
# note that the foreach loop in parallel probably isn't necessary
my.cores <- parallel::detectCores()  # store computers cores
registerDoParallel(cores = my.cores) # register parallel backend
radii.buffers <- seq(from = 100, to = 300, by = 25) # radii for buffer on each station point (in feet)

stations.buffered <- foreach(i = 1:length(radii.buffers), .packages = c("sp","rgeos"), .combine = c) %dopar% {
  gBuffer(uza.stations, byid = T, width = radii.buffers[i]) } # (stores as list of SpatialPointDataFrames)

# this analysis uses parking data at the parcel level
# the next section of script imports the raw parcel database for the metro region and clips to only the maximum
# buffered radius of the stations in the region for quicker processing in the 1c-spatial-data-prep.R script
parcels <- readRDS(here("data/outputs/temp/all_parcels_mag.rds")) # load full parcels in region
radii.buffers <- seq(from = 100, to = 300, by = 25) # radii for buffer on each station point (in feet)
l <- length(radii.buffers) # store index of largest buffer radii
parcels.trimmed <- intersect(parcels, stations.buffered[[l]]) # trim parcels by largest station buffer
parcels.trimmed <- raster::aggregate(parcels.trimmed, by = "APN", dissolve = T) # dissolve
parcels.trimmed <- parcels.trimmed[,c("APN")] # only need the APN

# save files
#saveRDS(uza.stations, here("data/outputs/uza-station-data.rds")) # station data (uza) as spatial r object (points)
saveRDS(uza.buffer, here("data/outputs/temp/uza-buffer.rds"))
saveRDS(parcels.trimmed, here("data/outputs/temp/parcels-trimmed.rds")) # trimmed parcels for clipping to station buffers
saveRDS(stations.buffered, here("data/outputs/temp/stations-buffered-prep.rds"))
saveRDS(radii.buffers, here("data/outputs/radii-buffers.rds"))
shapefile(uza.stations, here("data/shapefiles/processed/stations_pts"), overwrite = T) # station points shapefile

# print script endtime
t.end <- Sys.time() 
paste0("Completed task at ", t.end, ". Task took ", round(difftime(t.end,t.start, units = "mins"),1)," minutes to complete.") # paste total script time
# end

