################################################################
## PLOT STATIONS LOCATIONS for 2017 PHOENIX, AZ WEATHER DATA ##
##############################################################

extrafont::loadfonts(device = "win") # load fonts
cat("\014")     # clear console (Cntl + L)

library(data.table)
library(tidyverse)
library(lubridate)
library(raster)
library(sp)
library(tmap)
library(here)

#-#-#-#-#-#-#-#
# Import data #
#-#-#-#-#-#-#-#

# cleaned weather station data
w.data <- readRDS(here("data/2016-all-data.rds"))
w.stations <- readRDS(here("data/2016-all-stations.rds"))

# convert stations data.table w/ lat-lon to coordinates (SpatialPointDataFrame)
w.stations.xy <- w.stations[,.(lon,lat)]
w.stations.spdf <- SpatialPointsDataFrame(coords = w.stations.xy, data = w.stations,
                                          proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# various shapefiles for plotting
# load city labels shpfile
phx.labels <- shapefile(here("data/shapefiles/phx_metro_labels.shp"))

# load UZA border
uza.border <- shapefile(here("data/shapefiles/maricopa_county_uza.shp"))

# load 2017 travel ways
hways <- shapefile(here("data/shapefiles/phx_metro_hways.shp"))

# store master font
my.font <- "Century Gothic"

# store palette 
my.palette <- c("#0C120C", "#C20114", "#6D7275")

# plot station points with UZA, highways, & city labels for context.
stations.plot <-   
  tm_shape(w.stations.spdf) + tm_dots(col = "source", border.col = NULL, palette = my.palette, size = 0.2, alpha = 0.8) + # station points
  tm_shape(uza.border) + tm_borders(lwd = 0.8, lty = "solid", col = "grey40", alpha = 0.7) + # urbanized area (UZA) border
  tm_scale_bar(position = c(0.4,0.0), breaks = c(0,5,10,15,20), size = 0.90, color.light = "grey85") + # scalebar
  tm_compass(north = 0, type = "4star", size = 2, show.labels = 1, position = c(0.9,0.85)) + # compass
  tm_shape(hways) + tm_lines(lwd = 1.1, col = "grey40") + # highways
  tm_shape(phx.labels) + tm_text("name", size = .5, fontfamily = my.font,  fontface = "bold.italic", just = "center") + # city labels
  tm_layout(fontfamily = my.font, fontface = "italic", bg.color = "grey95", title.size = 1.5, legend.title.size = 1.10, # theme & formatting
            legend.text.size = 0.95, title = "", title.position = c(0.12,0.94),
            legend.position = c(0.02,0.34), outer.margins = c(0,0,0,0), asp = 0)

tmap_save(stations.plot, filename = here("figures/stations.png"))
rm(stations.plot,w.stations.xy)
gc()