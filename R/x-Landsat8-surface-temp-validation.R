## INTRO

# this script details the surface temperature validation via Landsat 8 ARD data
# for year 2016 at custom chosen sites to validate the 1D heat transfer model

# ** It is recommended to use Microsoft Open R (v3.5.1) for improved performance without compromsing compatibility **


## SCRIPT PREPERATION

# clear space and allocate memory
gc()
memory.limit(size = 56000) 
script.start <- Sys.time() # start script timestamp

# first make sure checkpoint is installed locally
# this is the only package that is ok to not use a 'checkpointed' (i.e. archived version of a package)
# checkpoint does not archive itself and it should not create dependency issues
if (!require("checkpoint")){
  install.packages("checkpoint")
  library(checkpoint)
}

# load all other dependant packages from the local repo
lib.path <- paste0(getwd(),"/.checkpoint/2019-01-01/lib/x86_64-w64-mingw32/3.5.1")
library(mailR, lib.loc = lib.path)
library(zoo, lib.loc = lib.path)
library(lubridate, lib.loc = lib.path)
library(sp, lib.loc = lib.path)
library(rgdal, lib.loc = lib.path)
library(rgeos, lib.loc = lib.path)
library(tmap, lib.loc = lib.path)
library(raster, lib.loc = lib.path)
library(data.table, lib.loc = lib.path)
library(here, lib.loc = lib.path)

# archive/update snapshot of packages at checkpoint date
checkpoint("2019-01-01", # Sys.Date() - 1  this calls the MRAN snapshot from yestersday
           R.version = "3.5.1", # will only work if using the same version of R
           checkpointLocation = here(), # calls here package
           verbose = F) 

# load windows fonts and store as string
windowsFonts(Century=windowsFont("TT Century Gothic"))
#windowsFonts(Times=windowsFont("TT Times New Roman"))
my.font <- "Century"

## IMPORT VALIDATION SITE DATA 

# load csv with site lat longs (change to your points of intreset)
my.sites <- fread(here("data/validation_sites.csv"))
sites.proj <- my.sites # convert data frame of lat-long points to coordinates (SpatialPointDataFrame)
coordinates(sites.proj) <- c("X", "Y") # assign X & Y
proj4string(sites.proj) <- CRS("+proj=longlat +datum=WGS84") # project points to long-lat projection WSG84
print(sites.proj@coords, digits = 10) # check coords are correct


## IMPORTING and PROCESSING LANDSAT DATA

# Landsat 8 (L8) ARD data is in .tif files 
# 30-meter spatial resolution in Albers Equal Area (AEA) projection 
# World Geodetic System 1984 (WGS84) datum 

# list the subfolders that contain collections of geotif files for each day for each Landsat product
st.tiles <- list.dirs(here("data/landsat/ST"), recursive = F, full.names = F)

# create a list of lists that contain the .tif files by day
st.tile.list <- lapply(1:length(st.tiles), function (x) 
  list.files(here(paste0("data/landsat/ST/", st.tiles[x])), recursive= T, pattern="tif$", full.names = T))

# create a raster stack from the list of lists such that each top level is a day
# and under it is the stacked geotifs. e.g. to refrence 1st day 2nd layer/band: stack[[1]][[2]]
st.tile.stack <- lapply(st.tile.list, stack)

# load tile metadata (all available tiles of Phx metro in 2016)
tile.metadata <- fread(here("data/landsat/tile-metadata.csv"), check.names = T)

# filter metadata to tiles downloaded
tile.metadata <- tile.metadata[Tile.Identifier %in% gsub("_ST", "", st.tiles),] # remove "_ST" from end of folder names to match

# order the metadata to follow the same order as the files were imported in the the list of raster stacks
tile.metadata <- tile.metadata[order(match(Tile.Identifier, gsub("_ST", "", st.tiles)))]

# Variables (via https://www.usgs.gov/land-resources/nli/landsat/landsat-provisional-surface-temperature?qt-science_support_page_related_con=0#qt-science_support_page_related_con)
# Atmospheric Transmittance layer (ATRAN): Displays the ratio of the transmitted radiation to the total radiation incident upon the medium (atmosphere).
# Distance to Cloud (CDIST): Represents the distance, in kilometers, that a pixel is from the nearest cloud pixel. 

# Thermal Radiance layer (TRAD): Displays the values produced when thermal band reflectance is converted to radiance.
# Upwelled Radiance layer (URAD): Displays the amount of electromagnetic radiation reflected upward from the ground's surface.
# Downwelled Radiance layer (DRAD): Displays the thermal energy radiated onto the ground by all objects in a hemisphere surrounding it.
# Emissivity layer (EMIS): Displays the ratio of the energy radiated from a material's surface to that radiated from a blackbody.
# Emissivity Standard Deviation (EMSD): The extent of deviation of the emissivity product.
# Surface Temperature Quality Assessment (STQA): Provides the Surface Temperature product uncertainty using a combination of uncertainty values and distance to cloud values
# ok this works # Surface Temperature (ST): Represents the temperature of the Earth's surface in Kelvin (K). Divide by 100. Hottest ever: 344 K
# can merge tiles to get complete coveage of metro area but note that dates are usually different for these mosaics
#tile.merge <- do.call(merge, c(tile.stack, tolerance = 1))


# convert surface temps to celcius with decimal. -273.15 C == 0 K.
for(r in 1:length(st.tile.stack)){
  values(st.tile.stack[[r]][[9]]) <- (values(st.tile.stack[[r]][[9]]) - 2731.5) / 10
}



## PLOTTING of LANDSAT DATA



# get Phoenix Urbanized Area (UZA) data and match projection to overlay UZA boundary on plots
longlat.prj <- "+proj=longlat +datum=WGS84 +units=m +ellps=WGS84 +towgs84=0,0,0" # lat lon projection that matches Landsat

# buffer uza boundary by 1 mile (1.6 km)
uza.border <- shapefile(here("data/shapefiles/boundaries/maricopa_county_uza.shp")) # import Maricopa County UZA boundary
uza.buffer <- gBuffer(uza.border, byid = F, width = 5280) # add a one mile buffer to ensure no L8 data is lost when clipped

# create bounding box for UZA
uza.bbox <- spTransform(uza.buffer, longlat.prj)
uza.bbox <- bbox(uza.bbox)

# project uza buffer to crs of daymet data to crop
uza.buffer.prj <- spTransform(uza.buffer, crs(st.tile.stack[[1]]))
uza.border.prj <- spTransform(uza.border, crs(st.tile.stack[[1]]))

# crop the merged tiles by the projected uza buffer
#tiles.mc <- crop(tiles.m, uza.buffer.prj)


tmap_options(max.raster = c(plot = 2.5e+07, view = 2.5e+07))

# loop through all raster stacks and create surface temperature plots
for(s in 1:length(st.tile.stack)){
  tryCatch({  # catch and print errors, avoids stopping model run 
  tile.crop <- crop(st.tile.stack[[s]][[9]], uza.buffer.prj)
  
  my.date <- ymd(substr((names(tile.crop)), 16, 23))
  my.sat <- paste0("Landsat ",substr((names(tile.crop)), 4, 4))
  my.tile <- paste0("Tile ", substr((names(tile.crop)), 9, 14))
  
  plot.1 <- tm_shape(tile.crop) + 
    tm_raster(palette = rev(heat.colors(40)),
              style = "cont",
              title = "Surface Temperature \n(deg C)") +
    tm_shape(uza.border.prj) +
    tm_borders(lwd = 0.1, 
               lty = "solid",
               col = "grey50",
               alpha = 1) +
    tm_layout(fontfamily = my.font, 
              legend.title.size = 0.9, 
              legend.text.size = 0.8,
              legend.position = c(0.02,0.72),
              title = paste0(my.date,"\n",my.sat,"\n",my.tile),
              title.size = 0.7,
              title.position = c(0.08,0.08))
  
  dir.create(here("figures/landsat/"), showWarnings = FALSE) # creates output folder if it doesn't already exist
  tmap_save(plot.1, filename = here(paste0("figures/landsat/", names(tile.crop),".png"))) #width = 3509, height = 2482
  }, error = function(e){cat("ERROR:",conditionMessage(e), "\n")}) # print error message if model run had error
}


