###################################################################
## OSM DATA IMPORT and FORMART to USE with WEATHER STATION DATA ##
#################################################################

extrafont::loadfonts(device = "win") # load fonts
#list.of.packages <- c()  # a list of the dependant packages  
#new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages)

cat("\014")     # clear console (Cntl + L)

library(data.table)
library(tidyverse)
library(lubridate)
library(rgdal)
library(raster)
library(sp)
library(maptools)
library(deldir)
library(tmap)
library(here)

#-#-#-#-#-#-#-#
# Import data #
#-#-#-#-#-#-#-#

# import cleaned weather & station data
w.data <- readRDS(here("data/2017-all-data.rds"))
w.stations <- readRDS(here("data/2017-all-stations.rds"))

# import osm data (maricopa county clipped raw road network data)
phx.labels <- shapefile(here("data/shapefiles/maricopa_county_osm_roads.shp")) # city labels shpfile

# import uza boundary
uza.border <- shapefile(here("data/shapefiles/maricopa_county_uza.shp")) # uza shpfile

# convert station lat/lon to spatial df, assign crs to uza crs and clip to uza extent
w.stations.spdf <- SpatialPointsDataFrame(coords = w.stations[, .(lon,lat)], data = w.stations,
                                          proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
w.stations.spdf <- spTransform(w.stations.spdf, crs(uza.border))
uza.stations <- raster::intersect(w.stations.spdf, uza.border)


# filter out non-auto roads (pedestrian walkways, bike paths, footpaths, etc)

# assign roadway widths based on roadway class
# assume number of lanes is constant by class
# assume outside shoulder width is 10 ft
# assume inside shoulde width is 10 ft and only exists for highway/express roadway classes
# assume lane width is 12 ft
# 1 ft = 0.3048 meters

# buffer each weather station by a raduis of 500m 



