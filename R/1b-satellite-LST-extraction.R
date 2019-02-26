## INTRO

# this script details the surface temperature validation via
# ASTER Surface Kinetic Temperature (AST_08)
# at custom chosen sites to validate the 1D heat transfer model

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
  library(checkpoint, quietly = T)
}

# load all other dependant packages from the local repo
lib.path <- paste0(getwd(),"/.checkpoint/2019-01-01/lib/x86_64-w64-mingw32/3.5.1")
library(RColorBrewer, lib.loc = lib.path, quietly = T, warn.conflicts = F)
library(zoo, lib.loc = lib.path, quietly = T, warn.conflicts = F)
library(lubridate, lib.loc = lib.path, quietly = T, warn.conflicts = F)
library(sp, lib.loc = lib.path, quietly = T, warn.conflicts = F)
library(raster, lib.loc = lib.path, quietly = T, warn.conflicts = F)
library(rgdal, lib.loc = lib.path, quietly = T, warn.conflicts = F)
library(gdalUtils, lib.loc = lib.path, quietly = T, warn.conflicts = F)
library(rgeos, lib.loc = lib.path, quietly = T, warn.conflicts = F)
library(tmap, lib.loc = lib.path, quietly = T, warn.conflicts = F)
library(data.table, lib.loc = lib.path, quietly = T, warn.conflicts = F)
library(here, lib.loc = lib.path, quietly = T, warn.conflicts = F)

# archive/update snapshot of packages at checkpoint date
checkpoint("2019-01-01", # Sys.Date() - 1  this calls the MRAN snapshot from yestersday
           R.version = "3.5.1", # will only work if using the same version of R
           checkpointLocation = here(), # calls here package
           verbose = F) 

## IMPORT VALIDATION SITE DATA 

# load csv with site lat longs (change to your points of intreset)
my.sites <- fread(here("data/validation_sites.csv"))
n.sites <- my.sites[,.N]
sites.prj <- my.sites # convert data frame of lat-long points to coordinates (SpatialPointDataFrame)
coordinates(sites.prj) <- c("X", "Y") # assign X & Y
lon.lat.prj <- crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
proj4string(sites.prj) <- lon.lat.prj # project points to long-lat projection WSG84   "+proj=longlat +datum=WGS84"
#print(sites.prj@coords, digits = 10) # check coords are correct

## GET ASTER SCENE METADATA

# get file paths and names for all aster metadata files
meta.files <- list.files(here("data/aster/raw"), recursive = F, full.names = T, pattern = "met$")
meta.names <- gsub(".zip.met", "", list.files(here("data/aster/raw"), recursive = F, full.names = F, pattern = "met$"))

# create empty data.table then bind file names (stripped of extensions) as id to match with tiff file names 
meta.data <- data.table(date = character(), time = character())
meta.data <- rbind(list(id = meta.names), meta.data, fill = T)

# loop through each .met file and use readLines and force strip relevant data and store into master metadata dt
# indexing was done manually so this requires the formatting to be consistent
for(m in 1:length(meta.files)){
  a <- readLines(meta.files[m])
  
  # strip date & time of scene
  meta.data[m, date := gsub("\"", "", gsub(".*=  \"", "", a[131]))]
  meta.data[m, time := gsub("\"", "", gsub(".*=  \"", "", a[127]))]
  
  # strip lat/lon at 4 corners of scene 
  meta.data[m, lon1 := unlist(strsplit(gsub(".*\\(", "", a[99]), ","))[1]]
  meta.data[m, lon2 := gsub( " ", "", unlist(strsplit(gsub(".*\\(", "", a[99]), ","))[2])]
  meta.data[m, lon3 := gsub( " ", "", unlist(strsplit(gsub(".*\\(", "", a[99]), ","))[3])]
  meta.data[m, lon4 := gsub( "\\)", "", gsub( " ", "", unlist(strsplit(gsub(".*\\(", "", a[99]), ","))[4]))]
  
  meta.data[m, lat1 := unlist(strsplit(gsub(".*\\(", "", a[104]), ","))[1]]
  meta.data[m, lat2 := gsub( " ", "", unlist(strsplit(gsub(".*\\(", "", a[104]), ","))[2])]
  meta.data[m, lat3 := gsub( " ", "", unlist(strsplit(gsub(".*\\(", "", a[104]), ","))[3])]
  meta.data[m, lat4 := gsub( "\\)", "", gsub( " ", "", unlist(strsplit(gsub(".*\\(", "", a[104]), ","))[4]))]
  
  # strip cloud cover
  meta.data[m, cloud := gsub("\\\"", "", gsub(".*=  \\\"", "", a[249]))] # strip total cloud coverage (%)
  meta.data[m, cloud.UL := gsub("\\\"", "", gsub(".*=  \\\"", "", a[217]))] # strip cloud coverage (%) upper left (UL)
  meta.data[m, cloud.UR := gsub("\\\"", "", gsub(".*=  \\\"", "", a[233]))] # strip cloud coverage (%) upper right (UR)
  meta.data[m, cloud.LL := gsub("\\\"", "", gsub(".*=  \\\"", "", a[185]))] # strip cloud coverage (%) lower left (LL)
  meta.data[m, cloud.LR := gsub("\\\"", "", gsub(".*=  \\\"", "", a[201]))] # strip cloud coverage (%) lower right (LR)
}

# convert date and time to date/time class. Time is GMT [1]
meta.data[, date.time := ymd_hms(paste(date, time), tz = "GMT")] # assign GMT timezone
meta.data[, date.time := with_tz(date.time, tz = "US/Arizona")] # convert to local (AZ) time
meta.data[, c("date","time") := NULL] # remove date & time cols now that we have date.time

# convert other columns to correct classes
meta.data[,2:9] <- lapply(meta.data[,2:9], as.numeric) # lat & lon
meta.data[,10:13] <- lapply(meta.data[,10:13], as.integer) # cloud coverage
#lapply(meta.data, class) # check

# day or night?
meta.data[hour(meta.data$date.time) >= 7 & hour(meta.data$date.time) < 18, day.time := "day"]
meta.data[hour(meta.data$date.time) < 7 | hour(meta.data$date.time) >= 18, day.time := "night"]

# cloud coverage quantiles
meta.data[, quantile(cloud, probs = c(0,0.1,0.25,0.5,0.6,0.7,0.75,0.8,0.9,1))]


############################################################
# DETERMINE CRITERIA TO SELECT SCENES TO MANUALLY PROCESS* #
############################################################
# *ASTER data has bugs in stored georeferenced info but somehow QGIS can decode, no R libraries work

# create list of ids that we are interested in 
meta.data[, idx := .I] # id for row number
my.idx <- meta.data[cloud == 0 &
           #lubridate::month(date.time, label = T, abbr = T) %in% c("May","Jun","Jul","Aug") &   # specific months
           lubridate::year(date.time) %in% c(2010:2017), idx] # specific years

# create a list of lists that contain the .tif files by day
#st.tile.list <- list.files(here("data/aster/all"), recursive = T, full.names = T, pattern="tif$")

# create a raster stack from the list of lists
#st.tile.stack <- lapply(st.tile.list, function(x) raster(x, layer = 1)) # make sure each element is a raster layer not a brick/stack

# export the desired scenes to process in qgis
#dir.create(here("data/aster processed/preprocessed"), showWarnings = FALSE) # creates output folder if it doesn't already exist
#for(i in 1:length(my.idx)){
#  j <- as.integer(my.idx[i])
#  writeRaster(st.tile.stack[[j]], here(paste0("data/aster processed/preprocessed/", j,".tif")), overwrite = T)
#}


####################################################################
# IMPORT DESIRED SCENES INTO QGIS AND SAVE TO FIX PROJECTION ISSUE #
####################################################################

# new list of processed ASTER rasters
new.list <- list.files(here("data/aster/processed"), recursive = T, full.names = T, pattern="tif$")

# new stack of processed ASTER rasters
new.stack <- lapply(new.list, function(x) raster(x, layer = 1)) # make sure each element is a raster layer not a brick/stack

# days of reference of processed ASTER rasters
new.data <- meta.data[idx %in% my.idx, ] # .(id,cloud,date.time,day.time)

# create data.table to store surface temperature extracted from each scene 
#st.by.site <- setnames(data.table(matrix(nrow = 0, ncol = length(my.sites$Location)+1)), c("id",my.sites$Location))
st.by.site <- setnames(data.table(matrix(nrow = 0, ncol = length(my.sites$Location))), my.sites$Location)
st.by.site <- rbind(list(id = new.data$id), st.by.site, fill = T)
st.by.site[,2:(n.sites+1)] <- lapply(st.by.site[,2:(n.sites+1)], as.numeric)

# update projections
sites.prj <- spTransform(sites.prj, crs(new.stack[[1]]))

# extract surface temps at locations
# order is same so no need to check if each extraction id matches
for(r in 1:length(new.stack)){
  
  # extract data
  st.by.site[id == gsub(".SurfaceKineticTemperature.KineticTemperature", "", names(new.stack[[r]])), 
             names(st.by.site[,2:(my.sites[,.N]+1)]) := 
               as.list(raster::extract(new.stack[[r]], sites.prj, method = 'simple', small = F, cellnumbers = F,
                                       df = F, factors = F))]
}

# merge surface temp data to meta.data
new.data <- merge(new.data, st.by.site, by = "id", all = T)

# identify best days for validation in new.data
new.idx <- c(877,77,1102)
new.data[idx %in% new.idx,]

########################################
# CREATE ASTER PLOTS OF SELECTED SITES #
########################################

# load windows fonts and store as string
windowsFonts(Century=windowsFont("TT Century Gothic"))
my.font <- "Century"

# buffer uza boundary by 1 mile (1.6 km)
uza.border <- shapefile(here("data/shapefiles/boundaries/maricopa_county_uza.shp")) # import Maricopa County UZA boundary
uza.buffer <- gBuffer(uza.border, byid = F, width = 5280) # add a one mile buffer to ensure no L8 data is lost when clipped
uza.border.prj <- spTransform(uza.border, crs(new.stack[[1]])) # project uza buffer to kat lon

# set plot options
tmap_options(max.raster = c(plot = 2.5e+07, view = 2.5e+07)) # expand max extent of raster for tmap
my.palette <- rev(RColorBrewer::brewer.pal(11, "Spectral")) # color palette

# loop through relevant raster scenes and create surface temperature plots
for(s in new.idx){ #length(st.tile.stack)
  tryCatch({  # catch and print errors, avoids stopping model run 

    my.date <- new.data[idx == s, date.time] # store date.time
    s <- new.data[idx == s, which = T] # correct index to new subsetted data
    
    # create plot
    plot.2 <- tm_shape(new.stack[[s]]) + 
      tm_raster(palette = my.palette,
                style = "cont") +
      tm_shape(uza.border.prj) +
      tm_borders(lwd = 0.1, 
                 lty = "dashed", #"dashed", "dotted", "dotdash", "longdash", or "twodash"
                 col = "grey50",
                 alpha = 0.5) +
      tm_shape(sites.prj) +
      tm_dots(size = 0.1, title = "Validation Sites", shape = 4) +
      tm_layout(fontfamily = my.font, 
                legend.title.size = 0.9, 
                legend.text.size = 0.8,
                legend.position = c(0.02,0.72),
                title = paste0("Surface Temperature (deg C)\n", my.date),
                title.size = 0.7,
                title.position = c(0.08,0.08))
    
    dir.create(here("figures/aster-best/"), showWarnings = FALSE) # creates output folder if it doesn't already exist
    tmap_save(plot.2, filename = here(paste0("figures/aster-best/", names(new.stack[[s]]),".png"))) # save plot
  }, error = function(e){cat("ERROR:",conditionMessage(e), "\n")}) # print error message if model run had error
}

write.csv(new.data[idx %in% new.idx,], here("data/aster/best-aster-dates.csv"), row.names = F)

# References

# [1] https://asterweb.jpl.nasa.gov/content/03_data/04_Documents/ASTERHigherLevelUserGuideVer2May01.pdf