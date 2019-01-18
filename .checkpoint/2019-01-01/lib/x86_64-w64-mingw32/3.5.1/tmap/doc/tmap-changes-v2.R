## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(collapse = TRUE, fig.width=6, fig.height=3)
#devtools::install_github(`mtennekes/tmaptools`)
library(tmap)
library(sf)

## ---- eval = FALSE-------------------------------------------------------
#  data(World, metro)
#  tmap_mode("view")
#  
#  tm_basemap(leaflet::providers$CartoDB.PositronNoLabels, group = "CartoDB basemap") +
#  tm_shape(World) +
#      tm_polygons("HPI", group = "Countries") +
#  tm_tiles(leaflet::providers$CartoDB.PositronOnlyLabels, group = "CartoDB labels") +
#  tm_shape(metro) +
#      tm_dots(col = "red", group = "Metropolitan areas")

## ---- fig.height=3-------------------------------------------------------
data(World)
qtm(World, fill = "life_exp")

tmap_style("classic")

qtm(World, fill = "life_exp")

tmap_options_diff()

tmap_options_reset()

## ------------------------------------------------------------------------
tmap_format()
panorama <- tmap_format("World")
panorama$asp <- 6
tmap_format_add(panorama, name = "panorama")
tmap_format()

## ---- fig.height = 1-----------------------------------------------------
tm_shape(World) + tm_polygons("HPI") + tm_format("panorama")
# or: qtm(World, fill = "HPI", format = "panorama")

## ---- eval = FALSE-------------------------------------------------------
#  tm_shape(World, filter = World$continent=="Europe") +
#  	tm_polygons("HPI", id = "name")

