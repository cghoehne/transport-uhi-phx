## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(collapse = T, fig.width=6, fig.height=3)
# devtools::install_github("mtennekes/tmaptools")

## ------------------------------------------------------------------------
library(tmap)
data("World")

tm_shape(World) +
    tm_polygons("HPI")

## ---- eval = FALSE-------------------------------------------------------
#  tmap_mode("view")
#  
#  tm_shape(World) +
#      tm_polygons("HPI")

## ------------------------------------------------------------------------
data(World, metro, rivers, land)

tmap_mode("plot")
tm_shape(land) +
    tm_raster("elevation", palette = terrain.colors(10)) +
tm_shape(World) +
    tm_borders("white", lwd = .5) +
    tm_text("iso_a3", size = "AREA") +
tm_shape(metro) +
    tm_symbols(col = "red", size = "pop2020", scale = .5) +
tm_legend(show = FALSE)

## ---- eval = FALSE-------------------------------------------------------
#  tmap_mode("view")
#  tm_shape(World) +
#      tm_polygons(c("HPI", "economy")) +
#      tm_facets(sync = TRUE, ncol = 2)

## ------------------------------------------------------------------------
tmap_mode("plot")

data(NLD_muni)

NLD_muni$perc_men <- NLD_muni$pop_men / NLD_muni$population * 100

tm_shape(NLD_muni) +
    tm_polygons("perc_men", palette = "RdYlBu") +
    tm_facets(by = "province")

## ------------------------------------------------------------------------
tmap_mode("plot")

data(NLD_muni)
tm1 <- tm_shape(NLD_muni) + tm_polygons("population", convert2density = TRUE)
tm2 <- tm_shape(NLD_muni) + tm_bubbles(size = "population")

tmap_arrange(tm1, tm2)

## ------------------------------------------------------------------------
tmap_mode("view")
tm_basemap("Stamen.Watercolor") +
tm_shape(metro) + tm_bubbles(size = "pop2020", col = "red") +
tm_tiles("Stamen.TonerLabels")

## ------------------------------------------------------------------------
tmap_mode("plot")

tm_shape(World) +
    tm_polygons("HPI") +
tm_layout(bg.color = "skyblue", inner.margins = c(0, .02, .02, .02))

## ------------------------------------------------------------------------
tmap_options(bg.color = "black", legend.text.color = "white")

tm_shape(World) +
    tm_polygons("HPI", legend.title = "Happy Planet Index")

## ------------------------------------------------------------------------
tmap_style("classic")

tm_shape(World) +
    tm_polygons("HPI", legend.title = "Happy Planet Index")

## ------------------------------------------------------------------------
# see what options have been changed
tmap_options_diff()

# reset the options to the default values
tmap_options_reset()

## ---- eval = FALSE-------------------------------------------------------
#  tm <- tm_shape(World) +
#      tm_polygons("HPI", legend.title = "Happy Planet Index")
#  
#  ## save an image ("plot" mode)
#  tmap_save(tm, filename = "world_map.png")
#  
#  ## save as stand-alone HTML file ("view" mode)
#  tmap_save(tm, filename = "world_map.html")

## ---- eval = FALSE-------------------------------------------------------
#  # in UI part:
#  leafletOutput("my_tmap")
#  
#  # in server part
#  output$my_tmap = renderLeaflet({
#      tm <- tm_shape(World) + tm_polygons("HPI", legend.title = "Happy Planet Index")
#      tmap_leaflet(tm)
#  })

## ------------------------------------------------------------------------
qtm(World, fill = "HPI", fill.pallete = "RdYlGn")

## ---- eval = FALSE-------------------------------------------------------
#  tmap_tip()

