################################################################
## PLOT STATIONS LOCATIONS for 2017 PHOENIX, AZ WEATHER DATA ##
##############################################################

extrafont::loadfonts(device = "win") # load fonts
cat("\014")     # clear console (Cntl + L)

library(data.table)
library(tidyverse)
library(lubridate)
library(rgdal)
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

# save stations as point shapefile
writeOGR(obj = w.stations.spdf, dsn = here("data/shapefiles/output"), layer = "phx_hourly_weather_stations", driver = "ESRI Shapefile", overwrite_layer = T)

# various shapefiles for plotting
# load city labels shpfile
phx.labels <- shapefile(here("data/shapefiles/phx_metro_labels.shp"))

# load UZA & county border
uza.border <- shapefile(here("data/shapefiles/maricopa_county_uza.shp"))
cnty.border <- shapefile(here("data/shapefiles/maricopa_county.shp"))

# load 2017 travel ways (note these are cropped to extend just outside of the UZA)
hways <- shapefile(here("data/shapefiles/phx_metro_hways.shp"))

# transform stations crs to same as other shapefiles 
w.stations.spdf <- spTransform(w.stations.spdf, crs(uza.border))

# store master font
my.font <- "Century Gothic"

# store palette 
my.palette <- c("#0C120C","#640D14","#C20114","#6D7275")

# plot station points with UZA, highways, & city labels for context.
stations.plot <-   
  tm_shape(uza.border) + tm_borders(lwd = 0.8, lty = "solid", col = "grey40", alpha = 0.7) + tm_fill(col = "grey90") + # urbanized area (UZA) border & fill
  tm_shape(w.stations.spdf, bbox = w.stations.spdf) + tm_dots(col = "source", border.col = NULL, palette = my.palette, size = 0.2, alpha = 0.8, title = "Data Source") + # station points
  tm_shape(cnty.border) + tm_borders(lwd = 0.8, lty = "solid", col = "grey20", alpha = 0.7) + # county border
  tm_scale_bar(position = c(0.4,0.0), breaks = c(0,5,10,15,20), size = 0.90, color.light = "grey85") + # scalebar
  tm_compass(north = 0, type = "4star", size = 2, show.labels = 1, position = c(0.9,0.85)) + # compass
  tm_shape(hways) + tm_lines(lwd = 1.1, col = "grey40") + # highways
  tm_shape(phx.labels) + tm_text("name", size = .5, fontfamily = my.font,  fontface = "bold.italic", just = "center") + # city labels
  tm_layout(fontfamily = my.font, fontface = "italic", bg.color = "grey95", title.size = 1.5, legend.title.size = 1.10, # theme & formatting
            legend.text.size = 0.95, title = "", title.position = c(0.12,0.94),
            legend.position = c(0.02,0.45), outer.margins = c(0,0,0,0), asp = 0)

tmap_save(stations.plot, filename = here("figures/stations_w_ibut.png"))


# calculate range of temps and mean temp for urbanized stations

# first clip stations points to ones w/in uza 
uza.stations <- raster::intersect(w.stations.spdf, uza.border)

# filter station list to uza stations
uza.stations.list <- w.stations[station.name %in% uza.stations$station.name]

# filter data to uza.station.list
uza.data <- w.data[station.name %in% uza.stations.list$station.name]

# if time is within 15 min of hour, round to hour, otherwise drop, then
# aggregate to the min, mean, max temp at each unique timestep for all stations
uza.data[, date.time.round := as.POSIXct(round.POSIXt(date.time, "hours"))]
uza.data <- uza.data[abs(difftime(date.time, date.time.round, units = "min")) < 15]
uza.data.agg <- uza.data[, .(min.temp.f = min(temp.f, na.rm = T),
                             med.temp.f = median(temp.f, na.rm = T),
                             max.temp.f = max(temp.f, na.rm = T)), by = .(date.time.round)]

# hourly temp min/mean/max by 2017 month aggregate across all stations
uza.data[, month := as.factor(month(date.time.round, label = T))]
uza.data[, hour := hour(date.time.round)]
uza.data.month.agg <- uza.data[, .(min.temp.f = min(temp.f, na.rm = T),
                                   med.temp.f = median(temp.f, na.rm = T),
                                   max.temp.f = max(temp.f, na.rm = T)), by = .(hour,month)]

# plot hourly temp.f range by month for 2017 for all stations
# Plot historical tempeature plots of metro regions
month.x.hour.p <- ggplot(uza.data.month.agg, aes(x = hour, y = med.temp.f , group = month)) +
  geom_ribbon(aes(ymin = min.temp.f, ymax = max.temp.f),  fill = "grey50", alpha = 0.5) +
  geom_line(size = 1.2) +
  geom_segment(aes(x = 0, y = 25, xend = 23, yend = 25)) +
  geom_segment(aes(x = 0, y = 25, xend = 0, yend = 135)) +
  facet_wrap(~ month , scales = "fixed") +
  labs(title = "Hourly Temperature Ranges for Urbanized Metro Phoenix, 2017", x = "Hour", y = "Temperature (deg F)") +
  scale_x_continuous(limits = c(0,23), breaks = c(0,6,12,18)) +
  scale_y_continuous(limits = c(25,135), breaks = c(25,50,75,100,125)) +
  theme_minimal() +
  theme(text = element_text(colour = "black", size = 11, family = my.font),
        plot.title = element_text(size = 11, face = "bold", family = my.font, hjust = .5, vjust = 2),
        plot.margin=unit(c(1, 1, 1, 1), units="mm"),
        axis.title = element_text(face = "bold"),
        axis.text.y = element_text(size = 11.5, vjust = 0.5, hjust = 0.7, margin = unit(c(0, 2, 0, 2), "mm"), colour = "black"),
        strip.text.x = element_text(size = 12, colour = "black")
  )

ggsave("hourly_tempF_by_month_UZA_w_ibut.png", month.x.hour.p, device = "png", path = here("figures"),
       scale = 1, width = 6.5, height = 8, dpi = 300, units = "in")



