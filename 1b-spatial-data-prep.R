###################################################################
## OSM DATA IMPORT and FORMART to USE with WEATHER STATION DATA ##
#################################################################

extrafont::loadfonts(device = "win") # load fonts

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

#-#-#-#-#-#-#-#
# Import data #
#-#-#-#-#-#-#-#

# import cleaned station data
w.stations <- readRDS(here("data/2017-all-stations.rds"))

# import osm data (maricopa county clipped raw road network data)
osm <- shapefile(here("data/shapefiles/osm/maricopa_county_osm_roads.shp")) # city labels shpfile

# import uza boundary
uza.border <- shapefile(here("data/shapefiles/boundaries/maricopa_county_uza.shp")) # uza shpfile


#-#-#-#-#-#-#
# Prep data #
#-#-#-#-#-#-#

# convert stations data.table w/ lat-lon to coordinates (SpatialPointDataFrame)
w.stations.spdf <- SpatialPointsDataFrame(coords = w.stations[, .(lon,lat)], data = w.stations,
                                          proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# transform stations crs to same as other shapefiles 
w.stations.spdf <- spTransform(w.stations.spdf, crs(uza.border))

# clip station points to ones w/in uza 
uza.stations <- raster::intersect(w.stations.spdf, uza.border)

# filter station.list to uza stations
uza.stations.list <- w.stations[station.name %in% uza.stations$station.name]

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
# assign roadway widths based on roadway class
# assume number of lanes is constant by class
# assume outside shoulder width is 10 ft
# assume inside shoulde width is 10 ft and only exists for highway/express roadway classes
# assume lane width is 12 ft
# 1 ft = 0.3048 meters

# import additional fclass info w/ assumptions of 1 way lanes keyed .csv
fclass.info <- fread(here("data/shapefiles/osm/fclass_info.csv"))
road.classes <- merge(road.classes, fclass.info, by = "fclass")

# store osm data for quick calcs of new variables and rebind
osm.dt <- as.data.table(osm@data)

# merge fclass.info with osm data
osm.dt <- merge(osm.dt, road.classes, by = "fclass")

# calulate the width of the road based on the number of lanes, 
# if it is 1 or 2way, and the shoulder widths
osm.dt$road.width.m <- ifelse(osm.dt$oneway == "B", # if the road has lanes in each direction
                              (osm.dt$lanes.1way * 2 * 12 * 0.3048) # width (m) = # lns/dir * 2 dir * 12 ft/ln * 0.3048 m/ft
                              + (osm.dt$in.shldr + osm.dt$out.shldr) * 0.3048, # in & out shoulder width (ft) * 0.3048 m/ft
                              (osm.dt$lanes.1way * 1 * 12 * 0.3048)
                              + (osm.dt$in.shldr + osm.dt$out.shldr) * 0.3048) # in & out shoulder width (ft) * 0.3048 m/ft) # same but in 1 dir

# update new relevant vars so they appear in osm@data
osm <- merge(osm, osm.dt[, .(osm_id,auto.use,road.width.m,descrip)], by = "osm_id")

# buffer uza boundary by ~1 mile, then clip osm network by buffered uza 
uza.buffer <- gBuffer(uza.border, byid = F, width = 5280, capStyle = "FLAT")

# transform osm crs to EPSG:2223
osm <- spTransform(osm, crs(uza.buffer))

# clip osm data to the buffered uza
osm.uza <- raster::intersect(osm, uza.buffer)

# buffer links by half of roadway width by loop for each roadway class type
w <- unique(osm.uza$road.width.m) # list of unique roadway widths to buffer by
w <- w[!is.na(w) & w > 0] # remove NA and 0 width roads
b <- list() # empty list for foreach
my.cores <- parallel::detectCores()  # store computers cores
registerDoParallel(cores = my.cores) # register parallel backend

# foreach loop in parallel to buffer links
foreach(i = 1:length(w), .packages = c("sp","rgeos")) %dopar% {
  x <- osm.uza[osm.uza$road.width.m == w[i], ]
  b[[i]] <- gBuffer(x, byid = F, width = w[i] / 2, capStyle = "FLAT")
}

# also buffer stations points by multiple radii
station.buffers <- c(50,100) #,200,500,1000) # radii for buffer on each station point

# foreach loop in parallel to buffer points by variable distances
foreach(i = 1:length(station.buffers), .packages = c("sp","rgeos"), .combine = c) %dopar% {
  b2 <- gBuffer(uza.stations, byid = T, width = station.buffers[i])
}

b2 <- gBuffer(uza.stations, byid = T, width = station.buffers[i])

##################
# 

# rebind together and check plot
osm.uza.buffer <- do.call(spRbind, b)
osm.uza.buffer <- SpatialPolygons(lapply(b, function(x){x@polygons[[1:length(b)]]}))

#Getting polygon IDs
IDs <- sapply(b, function(x)
  slot(slot(x, "polygons")[[1]], "ID"))

#Checking
length(unique(IDs)) == length(b)

#Making SpatialPolygons from list of polygons
osm.uza.buffer <- SpatialPolygons(lapply(b,
                               function(x) slot(x, "polygons")[[1]]))


plot(osm.uza.buffer)

# save prepped osm data

