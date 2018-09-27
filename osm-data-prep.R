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
osm <- shapefile(here("data/shapefiles/osm/maricopa_county_osm_roads.shp")) # city labels shpfile

# temp store for the data to view its structre
osm.df <- osm@data

# table of roadway classes
road.classes <- as.data.table(table(osm@data$fclass))
setnames(road.classes, "V1", "fclass")

# create id column for 

road.classes[,auto.use := "Y"]

# id fclasses where cars are unsuitable or prohibited. assume 'unknown' is unsuitable without info to assume otherwise
# open the data dic to see details on this (page 15) by running in console: getOption('viewer')(here('data/shapefiles/osm/osm-data-dictionary.pdf'))
road.classes[fclass %in% c("bridleway","cycleway","footway","path","steps", "pedestrian", "unknown"), auto.use := "N"]

# also id fclasses of 'track' as 'P' for partial use/suitablity. data dic: "For agricultural use, in forests, etc. Often gravel roads."
road.classes[fclass %in% c("track","track_grade1","track_grade2","track_grade3","track_grade4","track_grade5"), auto.use := "P"]

# **Curently, OSM data in Phoenix does not have lane data. in light of this:
# assign roadway widths based on roadway class
# assume number of lanes is constant by class
# assume outside shoulder width is 10 ft
# assume inside shoulde width is 10 ft and only exists for highway/express roadway classes
# assume lane width is 12 ft
# 1 ft = 0.3048 meters

# import additional fclass info w/ assumptions of 1 way lanes keyed .csv and bind to road.classes
fclass.info <- fread(here("data/shapefiles/osm/fclass_info.csv"))
road.classes <- merge(road.classes, fclass.info, by = "fclass")

# import uza boundary
uza.border <- shapefile(here("data/shapefiles/boundaries/maricopa_county_uza.shp")) # uza shpfile

# convert station lat/lon to spatial df, assign crs to uza crs and clip to uza extent
w.stations.spdf <- SpatialPointsDataFrame(coords = w.stations[, .(lon,lat)], data = w.stations,
                                          proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
w.stations.spdf <- spTransform(w.stations.spdf, crs(uza.border))
uza.stations <- raster::intersect(w.stations.spdf, uza.border)


# filter out non-auto roads (pedestrian walkways, bike paths, footpaths, etc)


# buffer each weather station by a raduis of 500m 



