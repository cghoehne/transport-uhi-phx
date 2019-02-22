# this script pre-processes spatial data in prepareration for analysis

# start by maxing out the memory allocation (we use high number to force max allocation)
gc()
t.start <- Sys.time() # start script timestamp
#memory.limit(size = 56000) 

# list of all dependant packages
list.of.packages <- c("data.table", 
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
w.stations <- readRDS(here("data/outputs/temp/2017-station-data.rds")) # import cleaned station data
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


## filter out duplicates and undesirable station locations
# first create data.table of uza.stations to filter 
uza.stations.dt <- as.data.table(uza.stations@data)

# remove stations with no temp obs
uza.stations.dt <- uza.stations.dt[n.temp != 0]

# remove iButton station as they are unreliable
uza.stations.dt <- uza.stations.dt[source != "iButton"]

# remove co-op stations due to reliability
uza.stations.dt <- uza.stations.dt[-grep("COOP", id)]

## determine duplicate stations that appear in station list and choose the more accurate station location
# for each station, iterate through all other stations 
# calc the distance between station 'i' and other stations
# if the closest station has a station.name with a partial string match (agrep),
# and it is within the range of lat/long coord precision 
# flag both stations in the original station list and keep the station w/ more accurate lat/lon (greater mean sig figs in lat/lon)
# flag: 0 no flag, 1 flag match but keep, -1 flag match and delete

# to store precision of decimial lat lon
# we assume the single direction precision of lat lon coords is approximatley = (111,131.96 meters) / (10^(num of sigfig decimal degrees))
# based on distance at equator per decimal degree: 
# https://gisjames.wordpress.com/2016/04/27/deciding-how-many-decimal-places-to-include-when-reporting-latitude-and-longitude/ 
# this allows us to ignore stations that are similaly named but outside the possibility of being duplicates due to the precision of coords
# this assumes that the coords are accurate but may not precise. 
inv.prec <- 111.13196 # store as km b/c earth.dist function calcs in kilometers too

# function to calculate distance in kilometers between two lat/lon points
earth.dist <- function (long1, lat1, long2, lat2){
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}

# first set/reset dupflag column to 0 (0 is default = no action keep, 1 for dupe but keep, -1 for dupe but delete)
uza.stations.dt$dupflag <- 0

# double pass 
for(pass in 1:2){
  
  
  # mark dupes 
  for(i in 1:nrow(uza.stations.dt)){
    
    n <- uza.stations.dt$station.name[i] # station name
    lat <- uza.stations.dt$lat[i] # station lat
    lon <- uza.stations.dt$lon[i] # station lon
    l <- uza.stations.dt[!i] # all other stations 
    
    for(k in 1:nrow(l)){ # for all stations
      
      # calc all euclidian dist btwn stations 
      l$d[k] <- earth.dist(lon, lat, l$lon[k], l$lat[k]) 
      
      # logic to determine if stations are dups
      l$L1[k] <- !is_empty(agrep(n, l$station.name[k], max.distance = 1, ignore.case = T)) # TRUE for station name partial string match
      l$L2[k] <- !is_empty(agrep(l$station.name[k], n, max.distance = 1, ignore.case = T)) # TRUE for station name partial string match (reverse)
      l$L3[k] <- isTRUE(if(!is.na(l$elevation[k]) & !is.na(uza.stations.dt$elevation[i]))
      {abs(l$elevation[k] - uza.stations.dt$elevation[k])}   # TRUE if station elevations are w/in 50ft of each other 
      else {0} < 50) # or if one or both station elevations are NA
    }
    
    l.match <- l[L1 == T & L2 == T] # which stations have similar names to station 'i'
    
    # if there are stations that matched, continue with marking otherwise skip to next station
    if(nrow(l.match) > 0){
      
      m <- which.min(l.match$d) # which index in the matches is the closest station
      lat2 <- l.match$lat[m] # closest station match 
      lon2 <- l.match$lon[m]
      
      # determine number of figs to right of decimal in lat/long for mean precision of coord
      p1 <- mean(nchar(strsplit(as.character(lat), "[.]")[2]),nchar(strsplit(as.character(lon), "[.]")[2]))
      p2 <- mean(nchar(strsplit(as.character(lat2), "[.]")[2]),nchar(strsplit(as.character(lon2), "[.]")[2]))
      
      # the max impercision (in km) btwn closest 2 pts is along the diag of both imprecision squares of coords, assume pathag therom and euclidian dist
      max.imprec <- (2 * sqrt(2)) * ((inv.prec / (10^(p1))) + (inv.prec / (10^p2)))
      
      # if names match in either direction of test, then flag stations as dupes based on precision, previous label, & number of temp obs
      if(l.match$d[m] < max.imprec){ # IF matched 2 stations that are closest are within the max impercision of lat/lon distance
        
        # flag station 'i' to keep or delete based on:
        uza.stations.dt$dupflag[i] <- ifelse(( p1 > p2 # IF station 'i' has greater lat/lon precision
                                          | p1 == p2 & uza.stations.dt$n.temp[i] > l.match$n.temp[m]  # OR they are equal precision but station 'i' has larger sample size
                                          | l.match$dupflag[m] == -1   # OR the closest station to station 'i' has already been flagged to delete
                                          | l.match$n.temp[m] == 0)    # OR the closest station to station 'i' has no temperature observations
                                        & uza.stations.dt$n.temp[i] > 0,  # AND station 'i' has greater than zero temp obs
                                        1, -1) # THEN mark to keep, otherwise mark to delete (1 is keep, -1 is delete)
        
      } # if none of the matched stations by name were within the imprecision distance, 
      #then the dupflag stays as is (zero) indicating there was not a duplicate station identified with station 'i' 
    }
  }
  # remove dupes and pass again in case missed some due to some station locations have more than 2 of same instance
  uza.stations.dt <- uza.stations.dt[dupflag != -1]
}

# remove any other duplicates manually that were not caught
# keep station.name:Desert Ridge over id:AZM27
# keep station.name:Greenway over id:AZM12
# keep station.name:Encanto over id:AZM15
# keep id:MAGC over station.name:GateWay Community College
# keep id:MAJP over station.name:Jefferson Park
# keep id:KFFZ over id:722783
# keep id:KCHD over id:722749
# keep id:KGEU over id:722787
# keep id:KLUF over id:722785
# keep id:KDVT over id:722784
# keep id:KPHX over id:722780
# keep id:KGYR over id:722788
# keep id:722786 over id:KIWA
# keep id:E7798 over id:E8973 (too close, choose larger sample)
del.ids <- c("AZM27","AZM12","AZM15","722783","722749","722787","722785","722784","722780","722788","KIWA","E8973")
del.names <- c("GateWay Community College","Jefferson Park")
uza.stations.dt <- uza.stations.dt[!(id %in% del.ids)]
uza.stations.dt <- uza.stations.dt[!(station.name %in% del.names)]

# filter uza.stations spatial data by remaining filtered stations
uza.stations <- subset(uza.stations, id %in% uza.stations.dt$id)

## buffer remaining stations with variable buffers for analysis
# foreach loop in parallel to buffer stations points by multiple radii
# note that the foreach loop in parallel probably isn't necessary
my.cores <- parallel::detectCores()  # store computers cores
registerDoParallel(cores = my.cores) # register parallel backend
radii.buffers <- seq(from = 100, to = 1000, by = 50) # radii for buffer on each station point (in feet)

stations.buffered <- foreach(i = 1:length(radii.buffers), .packages = c("sp","rgeos"), .combine = c) %dopar% {
  gBuffer(uza.stations, byid = T, width = radii.buffers[i]) } # (stores as list of SpatialPointDataFrames)

# this analysis uses parking data at the parcel level
# the next section of script imports the raw parcel database for the metro region and clips to only the maximum
# buffered radius of the stations in the region for quicker processing in the 1c-spatial-data-prep.R script
parcels <- readRDS(here("data/outputs/temp/all_parcels_mag.rds")) # load full parcels in region
l <- length(radii.buffers) # store index of largest buffer radii
parcels.trimmed <- intersect(parcels, stations.buffered[[l]]) # trim parcels by largest station buffer
parcels.trimmed <- raster::aggregate(parcels.trimmed, by = "APN", dissolve = T) # dissolve
parcels.trimmed <- parcels.trimmed[,c("APN")] # only need the APN

# save files
saveRDS(uza.stations, here("data/outputs/uza-station-data.rds")) # station data (uza) as spatial r object (points)
saveRDS(uza.buffer, here("data/outputs/temp/uza-buffer.rds")) # buffered uza (1 mi)
saveRDS(parcels.trimmed, here("data/outputs/parcels-trimmed.rds")) # trimmed parcels for clipping to station buffers
saveRDS(stations.buffered, here("data/outputs/temp/stations-buffered-prep.rds")) # list of spatial station buffers at variable radii
saveRDS(radii.buffers, here("data/outputs/temp/radii-buffers.rds")) # vector of chosen radii buffers
shapefile(uza.stations, here("data/shapefiles/processed/stations_pts"), overwrite = T) # station points shapefile

# print script endtime
t.end <- Sys.time() 
paste0("Completed task at ", t.end, ". Task took ", round(difftime(t.end,t.start, units = "mins"),1)," minutes to complete.") # paste total script time
# end
