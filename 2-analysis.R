###########################
## DATA ANALYSIS SCRIPT ##
#########################

library(tidyverse)
library(data.table)
library(here)

# import cleaned weather & station data
w.data <- readRDS(here("data/2017-all-data.rds"))
w.stations <- readRDS(here("data/2017-all-stations.rds"))

# convert stations data.table w/ lat-lon to spatial coordinates (SpatialPointDataFrame)
w.stations.spdf <- SpatialPointsDataFrame(coords = w.stations[, .(lon,lat)], data = w.stations,
                                          proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# import buffered OSM road data


# import buffered station data 
# (buffer stations by vector of variable lengths to get a spatial df for each buffer dist)


# for each buffer distance around stations, calc the fraction of pavement and the total traffic


# create dummy traffic data by osm_id and hour of day (dummy ICARUS output)
dummy.traffic <- as.data.table(matrix(runif(100, 100, 6000), nrow = 100, ncol = 24) * runif(24, 0, 1))
dummy.traffic <- dummy.traffic[, floor(.SD)] # round down to nearest whole number for whole data.table
colnames(dummy.traffic) <- paste0("hour", seq(1:24)) # rename cols to "hour#" (1:24)
dummy.traffic$osm_id <- seq.int(nrow(dummy.traffic)) # dummy osm_id column (1:100)

