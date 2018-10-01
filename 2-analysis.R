###########################
## DATA ANALYSIS SCRIPT ##
#########################

library(data.table)
library(tidyverse)
library(here)

# import cleaned weather & station data
w.data <- readRDS(here("data/2017-all-data.rds"))
w.stations <- readRDS(here("data/2017-all-stations.rds"))

# convert stations data.table w/ lat-lon to coordinates (SpatialPointDataFrame)
w.stations.spdf <- SpatialPointsDataFrame(coords = w.stations[, .(lon,lat)], data = w.stations,
                                          proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))