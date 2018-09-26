####################################################################
## GIF of HOURLY TEMPERATURES in PHOENIX UZA across ALL STAITONS ##
##################################################################

extrafont::loadfonts(device = "win") # load fonts
cat("\014")     # clear console (Cntl + L)

library(data.table)
library(tidyverse)
library(lubridate)
library(rgdal)
library(raster)
library(sp)
library(deldir)
library(tmap)
library(here)

#-#-#-#-#-#-#-#
# Import data #
#-#-#-#-#-#-#-#

# cleaned weather & station data
w.data <- readRDS(here("data/2016-all-data.rds"))
w.stations <- readRDS(here("data/2016-all-stations.rds"))

# estimate thiessen polygons (voronoi diagram) around each station point then create gif of temp

# first clip stations points to ones w/in uza 
uza.stations <- raster::intersect(w.stations.spdf, uza.border)

# this function uses a spatial point df with @ coords and a polygon to create the 
# voronoi polyongs w/in the polygon as a bounding box
voronoi.polygons <- function(x, poly) {
  require(deldir)
  if (.hasSlot(x, 'coords')) {
    crds <- x@coords  
  } else crds <- x
  bb = bbox(poly)
  rw = as.numeric(t(bb))
  z <- deldir(crds[,1], crds[,2],rw=rw)
  w <- tile.list(z)
  polys <- vector(mode='list', length=length(w))
  require(sp)
  for (i in seq(along=polys)) {
    pcrds <- cbind(w[[i]]$x, w[[i]]$y)
    pcrds <- rbind(pcrds, pcrds[1,])
    polys[[i]] <- Polygons(list(Polygon(pcrds)), ID=as.character(i))
  }
  SP <- SpatialPolygons(polys)
  voronoi <- SpatialPolygonsDataFrame(SP, data=data.frame(x=crds[,1],
                                                          y=crds[,2], row.names=sapply(slot(SP, 'polygons'), 
                                                                                       function(x) slot(x, 'ID'))))
  return(voronoi)
}
stations.vpoly <- voronoi.polygons(uza.stations, uza.border)

# match crs 
crs(stations.vpoly) <- crs(uza.border)

# clip to extent of phx uza border
stations.vpoly.b <- raster::intersect(uza.border, stations.vpoly)

