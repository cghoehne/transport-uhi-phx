# start by maxing out the memory allocation (we use high number to force max allocation)
gc()
memory.limit(size = 56000) 
t.start <- Sys.time() # end script timestamp

# list of all dependant packages
list.of.packages <- c("data.table", 
                      "rgdal",
                      "rgeos",
                      "ncdf4",
                      "maptools",
                      "sp",
                      "cleangeo",
                      "daymetr",
                      "doParallel",
                      "foreach",
                      "raster",
                      "here")

# install missing packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load packages
invisible(lapply(list.of.packages, library, character.only = TRUE)) # invisible() just hides printing stuff in console

# load previous data if needed
#load(here("data/outputs/temp/sp-prep-daymet.RData"))

# lat lon projection
longlat.prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

# buffer uza boundary by 1 mile (1.6 km)
uza.border <- shapefile(here("data/shapefiles/boundaries/maricopa_county_uza.shp")) # import Maricopa County UZA boundary
uza.buffer <- gBuffer(uza.border, byid = F, width = 5280)

# create bounding box for uza for DAYMET data download, which requires top left and bottom right coords c(lat, lon, lat, lon)
uza.bbox <- spTransform(uza.buffer, longlat.prj)
uza.bbox <- bbox(uza.bbox)

# DAYMET DATA DOWNLOAD @ 1km for PHOENIX METRO
# tmax: daily maximum 2-meter air temperature in degrees Celsius
# from bounding box, pull top left and bottom right coords c(lat, lon, lat, lon)
# bounding box is give as:     xmin xmax 
#                              ymin ymax 
# top lft (in lat,lon)  (ymax, xmin); 
# bot rgt (in lat,lon): (ymin, xmax)
download_daymet_tiles(location = c(uza.bbox[2,2], uza.bbox[1,1], uza.bbox[2,1], uza.bbox[1,2]), 
                      start = 2016, end = 2016, path = here("data/daymet"), param = "tmax")
nc2tif(path = here("data/daymet"), overwrite = T) # convert to tif to work w/ easier

# create list of daymet netcdf files and import as list of objects
tile.list <- list.files(here("data/daymet"), recursive= T, pattern="tif$", full.names= T)  
tiles.raw <- list() # empty list
tiles.prj <- list() # empty list

# stack the raw geotiff tiles
tiles.raw <- lapply(tile.list, stack)

# merge the tiles into one
tiles.m <- do.call(merge, c(tiles.raw, tolerance = 1))

# project uza buffer to crs of daymet data to crop
uza.buffer.prj <- spTransform(uza.buffer, crs(tiles.m))
uza.border.prj <- spTransform(uza.border, crs(tiles.m))

# crop the merged tiles by the projected uza buffer
tiles.mc <- crop(tiles.m, uza.buffer.prj)

# plot daymet tmax for 2016 day 181 (jun 29)
plot(tiles.m$layer.181, col = rev(heat.colors(40)))
plot(uza.buffer.prj, add = T)

# plot daymet tmax for 2016 day 181 (jun 29)
ncol <- floor(maxValue(tiles.mc$layer.181) - minValue(tiles.mc$layer.181))
png(filename = here("figures/DAYMET_20160629_tmax_phx.png"))
plot(tiles.mc$layer.181, col = rev(heat.colors(ncol+1)))
plot(uza.border.prj, add = T)
title(main = "DAYMET June 29th, 2016 Max Temperature (deg C)")
dev.copy(png, filename = here("figures/DAYMET_20160629_tmax_phx.png"), width = 3600, height = 3000)
dev.off()
dev.off()

#save.image(here("data/outputs/temp/sp-prep-daymet.RData"))

## clip + aggregate pavement and parking data by each grid cell (1km x 1km) 
## and merge parking and pavement data to each grid cell in daymet tiles (raster bricks)


