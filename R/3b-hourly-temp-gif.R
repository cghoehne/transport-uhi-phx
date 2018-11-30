####################################################################
## GIF of HOURLY TEMPERATURES in PHOENIX UZA across ALL STAITONS ##
##################################################################

extrafont::loadfonts(device = "win") # load fonts

# list of all dependant packages
list.of.packages <- c("tidyverse",
                      "data.table", 
                      "rgdal",
                      "rgeos",
                      "sp",
                      "raster",
                      "maptools",
                      "deldir",
                      "tmap",
                      "here")

# install missing packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load packages
invisible(lapply(list.of.packages, library, character.only = T)) # invisible() just hides printing stuff in console

#-#-#-#-#-#-#-#
# Import data #
#-#-#-#-#-#-#-#

# import cleaned weather & station data
w.data <- readRDS(here("data/outputs/temp/2017-weather-data.rds")) # cleaned hourly/sub-hourly weather data for all stations
uza.stations <- readRDS(here("data/outputs/uza-station-data.rds")) # station data (uza) as spatial r object (points)

## import other various shapefiles for plotting
city.labels <- shapefile(here("data/shapefiles/other/phx_metro_labels.shp")) # city labels (pre-cropped to UZA)
uza.border <- shapefile(here("data/shapefiles/boundaries/maricopa_county_uza.shp")) # UZA 
cnty.border <- shapefile(here("data/shapefiles/boundaries/maricopa_county.shp")) # county 
highways <- shapefile(here("data/shapefiles/other/phx_metro_hways.shp")) # 2017 highighways (pre-cFropped to UZA)

# other plot/formatting 
my.scale <- seq(from = 71, to = 111, by = 3)
heat.p <- rev(grDevices::heat.colors(n = length(my.scale))) # heat color palette
my.font <- "Century Gothic" # store master font


# filter weather data to all unique uza stations
all.station.ids <- unique(uza.stations$id[uza.stations$id != "UP549"]) # remove outlier station UP549
all.station.ids <- all.station.ids
uza.weather <- w.data[id %in% all.station.ids] 
rm(w.data)
gc()

# estimate thiessen polygons (voronoi diagram) around each station point then create gif of temp
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

# match crs of new vpolys
crs(stations.vpoly) <- crs(uza.border)

# clip to extent of phx uza border
stations.vpoly.b <- raster::intersect(uza.border, stations.vpoly)

# join station data to vpolys for linking temp data
stations.vpoly.data <- over(stations.vpoly.b, uza.stations)
stations.vpoly.b <- spCbind(stations.vpoly.b, stations.vpoly.data)

# aggregate uza weather data to data.table of summary variables
t.agg <-
  uza.weather %>%
  group_by(id,hour) %>%
  summarise(smr_mean_hr_temp = mean(temp.f[month %in% c("Jun","Jul","Aug")], na.rm = T)) %>%
  data.table() 

# recast to join such that it can be used in a GIF
sum.data.mean <- dcast(t.agg[,c("id","hour","smr_mean_hr_temp")], id ~ hour, value.var = "smr_mean_hr_temp")
setnames(sum.data.mean, paste0(0:23), paste0("hr",0:23))

# join summer data to polys
stations.vpoly.b <- merge(stations.vpoly.b, sum.data.mean, by = "id")

# create GIF of summer temps in vpolys
# **WARNING** this will not work withoug ImageMagick installed: https://www.imagemagick.org/script/download.php
gif <- 
  tm_shape(stations.vpoly.b) + # voronoi polygons
  tm_fill(col = as.character(paste0("hr",0:23)), # fill of polys is based on tempF at hour
          palette = heat.p, 
          style = "fixed", # scale is fixed for each frame
          breaks = my.scale, # scale from 70 F to 122 F by 4
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
  tm_shape(highways) +  # major metro highighways
  tm_lines(lwd = 1, col = "grey20") +
  tm_shape(city.labels) +  # city labels
  tm_text("name", 
          size = .5, 
          fontfamily = my.font,
          fontface = "bold.italic",
          just = "center") +
  tm_layout(fontfamily = my.font,   # formatting & layout properties
            fontface = "italic", 
            title.size = 1.5, 
            legend.title.size = 0.8, 
            legend.text.size = 0.6, 
            title = as.character(paste("Hour:",0:23)), 
            bg.color = "grey95",
            title.position = c(0.06,0.12),
            legend.position = c(0.02,0.40),
            outer.margins = c(0,0,0,0), 
            asp = 0) +
  tm_facets(nrow=1, ncol=1) +  # gif properties
  tmap_options(limits = c(facets.plot = 120, facets.view = 1))

# save gif
tmap_animation(gif, filename = here::here("figures/summer-tempF-hourly-2017-vpoly-stations_new.gif"),
               delay = 100, loop = T, restart.delay = 0)

# percent obs
#nrow(uza.weather[!is.na(temp.f)]) / nrow(uza.weather)
#nrow(uza.weather[!is.na(dewpt.f)]) / nrow(uza.weather)
#nrow(uza.weather[!is.na(winspd)]) / nrow(uza.weather)
#nrow(uza.weather[!is.na(solar)]) / nrow(uza.weather)




