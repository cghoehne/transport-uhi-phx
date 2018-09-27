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

# import cleaned weather & station data
w.data <- readRDS(here("data/2017-all-data.rds"))
w.stations <- readRDS(here("data/2017-all-stations.rds"))

# import other shapefiles for plotting
phx.labels <- shapefile(here("data/shapefiles/phx_metro_labels.shp")) # city labels shpfile
uza.border <- shapefile(here("data/shapefiles/maricopa_county_uza.shp")) # UZA shpfile
cnty.border <- shapefile(here("data/shapefiles/maricopa_county.shp")) # county shpfile
hways <- shapefile(here("data/shapefiles/phx_metro_hways.shp")) # 2017 highways (trimmed outside of UZA)

# other plot/formatting 
heat.p <- c('#fff5f0','#ffe3d7','#fdc6af','#fca487','#fc8161','#eb362a','#cc181d','#a90f15','#67000d') # palette
my.font <- "Century Gothic" # store master font

# convert stations data.table w/ lat-lon to coordinates (SpatialPointDataFrame)
w.stations.spdf <- SpatialPointsDataFrame(coords = w.stations[, .(lon,lat)], data = w.stations,
                                          proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# transform stations crs to same as other shapefiles 
w.stations.spdf <- spTransform(w.stations.spdf, crs(uza.border))


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
  z <- deldir(crds[,1], crds[,2], rw = rw, suppressMsge = T)
  w <- tile.list(z)
  polys <- vector(mode='list', length = length(w))
  require(sp)
  for (i in seq(along=polys)) {
    pcrds <- cbind(w[[i]]$x, w[[i]]$y)
    pcrds <- rbind(pcrds, pcrds[1,])
    polys[[i]] <- Polygons(list(Polygon(pcrds)), ID = as.character(i))
  }
  SP <- SpatialPolygons(polys)
  voronoi <- SpatialPolygonsDataFrame(SP, data=data.frame(x = crds[,1],
                                                          y = crds[,2], row.names = sapply(slot(SP, 'polygons'), 
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



# create GIF of summer temps in vpolys
# **WARNING** this will not work withoug ImageMagick installed: https://www.imagemagick.org/script/download.php
gif.tot <- 
  tm_shape(stations.vpoly.b) + # voronoi polygons
  tm_fill(col = as.character(paste0("hr",0:23)), # fill of polys is based on tempF at hour
          palette = heat.p, 
          style = "fixed", # scale is fixed for each frame
          breaks = c(50,60,70,80,90,100,110,120,130), # scale from 50 F to 130 F
          showNA = F, 
          colorNA = NULL,  # missing data is 'transparent'
          title = " 2017 Mean Summer \n Temperature (deg F)") +
  tm_shape(uza.border) +  # UZA border
  tm_borders(lwd = 0.5, 
             lty = "solid",
             col = "grey40",
             alpha = 0.7) +
  tm_compass(north = 0, # compass
             type = "4star", 
             size = 2,
             show.labels = 1, 
             position = c(0.9,0.85)) +
  tm_scale_bar(position = c(0.4,0), # scale bar
               breaks = c(0,5,10,15,20),
               size = 0.75,
               color.light = "grey85") +
  tm_shape(hways) +  # major metro highways
  tm_lines(lwd = 1, col = "grey20") +
  tm_shape(phx.labels) +  # city labels
  tm_text("name", 
          size = .5, 
          fontfamily = my.font,
          fontface = "bold.italic",
          just = "center") +
  tm_layout(fontfamily = my.font,   # formatting & layout properties
            fontface = "italic", 
            title.size = 1.5, 
            legend.title.size = 1.1, 
            legend.text.size = 0.9, 
            title = as.character(paste("Hour:",0:23)), 
            bg.color = "grey95",
            title.position = c(0.06,0.78),
            legend.position = c(0.02,0.40),
            outer.margins = c(0,0,0,0), 
            asp = 0) +
  tm_facets(nrow=1, ncol=1) +  # gif properties
  tmap_options(limits = c(facets.plot = 120, facets.view = 1))

# save total parking growth animation
tmap_animation(gif.tot, filename = here("figures/summer-tempF-hourly-2017-vpoly-stations.gif"),
               delay = 100, loop = T, restart.delay = 0)



