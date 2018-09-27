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

# import other shapefiles for plotting
phx.labels <- shapefile(here("data/shapefiles/phx_metro_labels.shp")) # city labels shpfile
uza.border <- shapefile(here("data/shapefiles/maricopa_county_uza.shp")) # UZA shpfile
cnty.border <- shapefile(here("data/shapefiles/maricopa_county.shp")) # county shpfile
hways <- shapefile(here("data/shapefiles/phx_metro_hways.shp")) # 2017 highways (trimmed outside of UZA)
