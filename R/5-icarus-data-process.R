#########################################################
# get and clean XML network and plans from ICARUS model #
########################################################

# clear space and allocate memory
gc()
#memory.limit(size = 50000) 
script.start <- Sys.time() # start script timestamp

# first make sure checkpoint is installed locally
# this is the only package that is ok to not use a 'checkpointed' (i.e. archived version of a package)
# checkpoint does not archive itself and it should not create dependency issues
if (!require("checkpoint")){
  install.packages("checkpoint")
  library(checkpoint, quietly = T)
}

# load all other dependant packages from the local repo
lib.path <- paste0(getwd(),"/.checkpoint/2019-01-01/lib/x86_64-w64-mingw32/3.5.1")
library(XML, lib.loc = lib.path)
library(doParallel)
library(foreach)
library(sp, lib.loc = lib.path)
library(sf, lib.loc = lib.path)
library(raster, lib.loc = lib.path)
library(rgdal, lib.loc = lib.path)
library(rgeos, lib.loc = lib.path)
library(gdalUtils, lib.loc = lib.path)
library(data.table, lib.loc = lib.path)
library(here, lib.loc = lib.path)

# archive/update snapshot of packages at checkpoint date
checkpoint("2019-01-01", # Sys.Date() - 1  this calls the MRAN snapshot from yestersday
           R.version = "3.5.1", # will only work if using the same version of R
           checkpointLocation = here(), # calls here package
           verbose = F) 

# get XML data for network and plans
network <- xmlToList(xmlParse(here("data/icarus/full_network.xml")))  # traffic network (nodes + links) from xml file


# get data by getting each set of attributes as list of one row data.frames and bind together
nodes <- rbindlist(lapply(1:length(network[["nodes"]]), 
                          function (x) as.data.frame.list(network[["nodes"]][[x]][[".attrs"]])))

# for some reason, there is an "invisible link" or phantom child somewhere in the link tree,
# so we need to manually reduce the length by 1 or else the reterival fails...
links <- rbindlist(lapply(1:(length(network[["links"]])-1), 
                          function (x) as.data.frame.list(network[["links"]][[x]][[".attrs"]])))

# correct classes and names of x/y in nodes
nodes <- nodes[, .(id = as.character(id),
                   lon = as.numeric(as.character(x)),
                   lat = as.numeric(as.character(y)))]
links[, c(1:3,9)] <- as.data.table(sapply(links[, c(1:3,9)], as.character))
links[, c(4:8)] <- as.data.table(sapply(links[, c(4:8)], function (x) as.numeric(as.character(x))))

# create spatial lines network from nodes and links
points <- SpatialPointsDataFrame(coords = nodes[, .(lon,lat)], data = nodes,
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# create list of simple feature geometries (linestrings)
lines <- vector("list", links[,.N])
for (i in 1:links[,.N]){
  lines[[i]] <- st_linestring(as.matrix(rbind(nodes[id == links[i, from], .(lon,lat)],  # begin node coords
                                              nodes[id == links[i, to], .(lon,lat)]))) # end coords
}
# create simple feature geometry list column
lines.c <- st_sfc(lines, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# convert to `sp` object then SpatialLinesDataFrame
lines.sp <- as(lines.c, "Spatial")
lines.spdf <- SpatialLinesDataFrame(lines.sp, data = links, match.ID = F)
lines.spdf.lite <- SpatialLinesDataFrame(lines.sp, data = links[,.(id,capacity)], match.ID = F)

# project and crop to uza buffer
# becuase the projection we want to use causes some infinite 
# 
uza.buffer <- readRDS(here("data/outputs/temp/uza-buffer.rds")) # Maricopa UZA buffered ~1mi
uza.wgs84 <- spTransform(uza.buffer, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#lines.spdf.c <- intersect(lines.spdf, uza.wgs84) # FAILS
lines.spdf.c <- gIntersection(lines.spdf, uza.wgs84)
#sf::st_intersection(lines.spdf, uza.wgs84)
lines.prj <- spTransform(lines.spdf.c, crs(uza.buffer))

# plot
#plot(lines.sp)
#points(points, col = "blue", pch = 20)

# write as shapefile
shapefile(lines.prj, here("data/outputs/shapefiles/icarus-network"), overwrite = T) # station points shapefile
#shapefile(lines.spdf.lite, here("data/outputs/shapefiles/icarus-network-lite"), overwrite = T) # station points shapefile

save.image(here("data/outputs/temp/network2.RData"))
#load(here("data/outputs/temp/network2.RData"))

# source for method to make network
#https://stackoverflow.com/questions/20531066/convert-begin-and-end-coordinates-into-spatial-lines-in-r


####################################
# BACKUP READING TRAVEL DATA FROM XML
plans <- xmlToList(xmlParse(here("data/icarus/TEST_matsim_plans_from_mag.xml"))) # plans (trips + activities) from xml file

# get list of person ids
per.id <- lapply(1:length(plans), function (x) unlist(plans[[x]][[".attrs"]][["id"]]))

# get list of all trips by person
trips.l <- lapply(1:length(plans), function (x) lapply(which(!(names(plans[[x]][["plan"]]) %in% c("act",".attrs")), arr.ind = T), 
                                                       function (y) as.data.frame.list(plans[[x]][["plan"]][[y]])))
# get list of all activies by person
activ.l <- lapply(1:length(plans), function (x) lapply(which(!(names(plans[[x]][["plan"]]) %in% c("leg",".attrs")), arr.ind = T), 
                                                       function (y) as.data.frame.list(plans[[x]][["plan"]][[y]])))
# bind all trips together and add person id            
trips <- rbindlist(lapply(1:length(trips.l), function (x) cbind(rbindlist(trips.l[[x]]), pid = per.id[[x]])))

# bind all activities together and add person id            
activ <- rbindlist(lapply(1:length(activ.l), function (x) cbind(rbindlist(activ.l[[x]], fill = T), pid = per.id[[x]])), fill = T)

