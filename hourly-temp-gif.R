####################################################################
## GIF of HOURLY TEMPERATURES in PHOENIX UZA across ALL STAITONS ##
##################################################################

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

# cleaned weather & station data
w.data <- readRDS(here("data/2017-all-data.rds"))
w.stations <- readRDS(here("data/2017-all-stations.rds"))

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

# join station data to vpolys for linking temp data
stations.vpoly.data <- over(stations.vpoly.b, w.stations.spdf)
stations.vpoly.b <- spCbind(stations.vpoly.b, stations.vpoly.data)

# load previously formatted data for summer 2017
sum.data <- readRDS(here("data/2017-summer-aggregated-by-station.rds"))

# recast to join such that it can be used in a GIF
sum.data.mean <- dcast(sum.data[,c("station.name","hour","mean.temp.f")], station.name ~ hour, value.var = "mean.temp.f")
setnames(sum.data.mean, paste0(0:23), paste0("hr",0:23))

# join summer data to polys
stations.vpoly.b <- merge(stations.vpoly.b, sum.data.mean, by = "station.name")

# other shapefiles for plotting
phx.labels <- shapefile(here("data/shapefiles/phx_metro_labels.shp")) # city labels shpfile
uza.border <- shapefile(here("data/shapefiles/maricopa_county_uza.shp")) # UZA shpfile
cnty.border <- shapefile(here("data/shapefiles/maricopa_county.shp")) # county shpfile
hways <- shapefile(here("data/shapefiles/phx_metro_hways.shp")) # 2017 highways (trimmed outside of UZA)

# palette
heat.p <- c('#fff5f0','#ffe3d7','#fdc6af','#fca487','#fc8161','#eb362a','#cc181d','#a90f15','#67000d')

# create GIF of summer temps in vpolys
gif.tot <- 
  tm_shape(stations.vpoly.b) +
  tm_fill(col = as.character(paste0("hr",0:23)), 
          palette = heat.p, 
          style = "fixed", 
          breaks = c(50,60,70,80,90,100,110,120,130), 
          showNA = F, 
          colorNA = NULL,
          title = "Temperature (deg F)") +
  tm_shape(uza.border) +
  tm_borders(lwd = 0.2, 
             lty = "solid",
             col = "grey40",
             alpha = 0.7) +
  tm_compass(north = 0, 
             type = "4star", 
             size = 2,
             show.labels = 1, 
             position = c(0.9,0.85)) +
  tm_scale_bar(position = c(0.4,0),
               breaks = c(0,5,10,15,20),
               size = 0.75,
               color.light = "grey85") +
  #tm_shape(hways) +
  #tm_lines(lwd = 1, col = "grey40") +
  tm_shape(phx.labels) +
  tm_text("name", 
          size = .5, 
          fontfamily = my.font,
          fontface = "bold.italic",
          just = "center") +
  tm_layout(fontfamily = my.font, 
            fontface = "italic", 
            title.size = 2.5, 
            legend.title.size = 1.1, 
            legend.text.size = 0.9, 
            title = as.character(paste("hour:",0:23)), 
            bg.color = "grey95",
            title.position = c(0.07,0.88),
            legend.position = c(0.02,0.32)) +
  tm_facets(nrow=1, ncol=1) +
  tmap_options(limits = c(facets.plot = 120, facets.view = 1))






