##################################################
## OSM DATA IMPORT and FORMART to BUFFER IN GIS ##
##################################################

# this script is used to take the raw osm road network data
# and prep it for buffering the roadway links to approx
# the area of pavement from roads that exist in the metro area
# the buffering is done in QGIS because R is slow/ineffective
# at variable spatial buffering. the output of this QGIS
# variable buffer is provided instead of a scirpt in this case
# because the process is time consuming but this script is 
# included for transparency

# list of all dependant packages
list.of.packages <- c("tidyverse",
                      "data.table", 
                      "rgdal",
                      "rgeos",
                      "raster",
                      "maptools",
                      "sp",
                      "raster",
                      "here")

# install missing packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load packages
lapply(list.of.packages, library, character.only = TRUE)


#-#-#-#-#-#-#-#
# Import data #
#-#-#-#-#-#-#-#

# import osm data (maricopa county clipped raw road network data)
osm <- shapefile(here("data/shapefiles/osm/maricopa_county_osm_roads.shp")) # city labels shpfile

# import uza boundary
uza.border <- shapefile(here("data/shapefiles/boundaries/maricopa_county_uza.shp")) # uza shpfile


#-#-#-#-#-#-#
# Prep data #
#-#-#-#-#-#-#

# store table of roadway classes as data.table, rename V1 to fclass
road.classes <- as.data.table(table(osm@data$fclass))
setnames(road.classes, "V1", "fclass")

# create id column for fclasses where cars are unsuitable or prohibited. assume 'unknown' is unsuitable without info to assume otherwise
# open the data dic to see details on this (page 15) by running in console: getOption('viewer')(here('data/shapefiles/osm/osm-data-dictionary.pdf'))
road.classes[,auto.use := "Y"]
road.classes[fclass %in% c("bridleway","cycleway","footway","path","steps", "pedestrian", "unknown"), auto.use := "N"]

# also id fclasses of 'track' as 'P' for partial use/suitablity. data dic: "For agricultural use, in forests, etc. Often gravel roads."
road.classes[fclass %in% c("track","track_grade1","track_grade2","track_grade3","track_grade4","track_grade5"), auto.use := "P"]

# **Curently, OSM data in Phoenix does not have lane data. in light of this:
# assign roadway widths based on roadway fclass
# assume roadway widths are constant by fclass
# 1way roadway width is based on typical regional street design 
# and average observed roadway widths by functional class
# proj is EPSG 2223 and units are feet

# import additional fclass info w/ roadway width data assigned by functional class
fclass.info <- fread(here("data/shapefiles/osm/fclass_info.csv"))
road.classes <- merge(road.classes, fclass.info, by = "fclass")

# store osm data for quick calcs of new variables and rebind
osm.dt <- as.data.table(osm@data)

# merge fclass.info with osm data
osm.dt <- merge(osm.dt, road.classes, by = "fclass")

# create final buffer variable (in feet b/c proj is uses units of feet)
# if roadway is bi-directional, the buffer radius is equal to the one-way roadway width in feet
osm.dt[, buf.ft := ifelse(oneway == "B", wdth.1way.ft, wdth.1way.ft / 2)] 

# update new relevant vars so they appear in osm@data
osm <- merge(osm, osm.dt[, .(osm_id, auto.use, pave.type, buf.ft, descrip)], by = "osm_id")

# buffer uza boundary by ~1 mile, then clip osm network by buffered uza 
uza.buffer <- gBuffer(uza.border, byid = F, width = 5280, capStyle = "FLAT")

# transform osm crs to EPSG:2223
osm <- spTransform(osm, crs(uza.buffer))

# clip osm data to the buffered uza
osm.uza <- raster::intersect(osm, uza.buffer)

# remove unnecessary variables before exporting to reduce size
osm.uza$ref <- NULL 
osm.uza$layer <- NULL
osm.uza$maxspeed <- NULL # most are 0 and don't need speed info when it does exist

# export formatted shapefile to be buffered in GIS (R is too slow/ineffective at doing this)
shapefile(osm.uza, here("data/shapefiles/osm/osm_formatted_unbuffered"), overwrite = T)

