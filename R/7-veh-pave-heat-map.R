######################
## HEAT MAP FIGURE  ##
######################

# ** It is recommended to use Microsoft Open R (v3.5.1) for improved performance without compromsing compatibility **

## SCRIPT PREPERATION

# clear space and allocate memory
gc()
memory.limit(size = 56000) 
script.start <- Sys.time() # start script timestamp

# first make sure checkpoint is installed locally
# this is the only package that is ok to not use a 'checkpointed' (i.e. archived version of a package)
# checkpoint does not archive itself and it should not create dependency issues
if (!require("checkpoint")){
  install.packages("checkpoint")
  library(checkpoint, quietly = T)
}

# calc roadway area by blockgroup
x <- 1
blkgrp$min.road.area <- ifelse(is.null(gIntersection(blkgrp[[y]][x,], osm.block)), 0, # check if no intersection and return 0 as area if null
                               gArea(gIntersection(blkgrp[[y]][x,], osm.block))) # otherwise calc the area of intersection with the road area

for(y in 1:length(blkgrp)){
  # calculate intersecting osm roadway area aroun each station buffer for each buffer distance
  blkgrp[[y]]$min.road.area <- sapply(1:length(stations.buffered[[y]]), # for all stations (sp features) in station buffer of index y 
                                      function(x) ifelse(is.null(gIntersection(stations.buffered[[y]][x,], osm.dissolved)), 0, # check if no intersection and return 0 as area if null
                                                         gArea(gIntersection(stations.buffered[[y]][x,], osm.dissolved)))) # otherwise calc the area of intersection with the road area
}

## PARKING
# ignore on-street spaces for area calcs because part of roadway

# import parking data and parcel data to merge together
parking <- fread(here("data/phx-parking-blkgrp.csv"))

# calculate the area parking by blockgroup
#parcels.trimmed$parcel.full.area <- sapply(1:length(parcels.trimmed), function(x) gArea(parcels.trimmed[x,]))  

# merge parcel.full.area to station parcels sp list and keep duplicates b/c some parcels in multiple buffers that overlap
for(y in 1:length(station.parcels)){ 
  station.parcels[[y]] <- sp::merge(station.parcels[[y]], parcels.trimmed, by = "APN", duplicateGeoms = T, all.x = T)}


# calc parking area in station buffers and adjust total spaces if total parking area + roadway area > station buffer
# (which means there is multistory parking and we just assume there is a 100% road + parking coverage)
for(y in 1:length(stations.buffered)){
  stations.buffered[[y]]$park.prct <- sapply(1:length(stations.buffered[[y]]), # for all stations (sp features) in station buffer of index y
                                             function(x) stations.buffered[[y]][x,]$tot.park.area / stations.buffered[[y]][x,]$buffer.area) # calc the % parking area in buffer
  
  # create coverage area flag to mark which features have the issue of too much parking coverage
  stations.buffered[[y]]$park.cov.flag <- sapply(1:length(stations.buffered[[y]]), # for all stations (sp features) in station buffer of index y
                                                 # if the % of parking + road area is over 100%
                                                 function(x) ifelse(stations.buffered[[y]][x,]$park.prct + stations.buffered[[y]][x,]$road.prct > 1,
                                                                    1, 0)) # mark 1 for flag, 0 for not.
  
  # re-adjust total parking spaces if parking coverage is too high
  stations.buffered[[y]]$tot.park.spaces <- sapply(1:length(stations.buffered[[y]]), # for all stations (sp features) in station buffer of index y
                                                   # if the parking coverage is flagged (% of parking + road area is over 100%)
                                                   function(x) ifelse(stations.buffered[[y]][x,]$park.cov.flag == 1, 
                                                                      # then reduce spaces based by excess parking area
                                                                      stations.buffered[[y]][x,]$tot.park.spaces - floor((stations.buffered[[y]][x,]$tot.park.area + stations.buffered[[y]][x,]$road.area - stations.buffered[[y]][x,]$buffer.area)/(9*18)),
                                                                      # otherwise total spaces stays the same
                                                                      stations.buffered[[y]][x,]$tot.park.spaces))
  
  # recalculate parking percent of area (will be max 100% if road is 0%, road + park <= 100%)
  stations.buffered[[y]]$park.prct <- sapply(1:length(stations.buffered[[y]]), # for all stations (sp features) in station buffer of index y
                                             function(x) stations.buffered[[y]][x,]$tot.park.area / stations.buffered[[y]][x,]$buffer.area) # calc the % parking area in buffer
  
  # calculate pavement percent of area 
  stations.buffered[[y]]$pave.prct <- sapply(1:length(stations.buffered[[y]]), # for all stations (sp features) in station buffer of index y
                                             function(x) stations.buffered[[y]][x,]$park.prct + stations.buffered[[y]][x,]$road.prct) # calc the % parking area in buffer
}

