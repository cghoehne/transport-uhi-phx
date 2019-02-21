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

# load windows fonts and store as string
windowsFonts(Century=windowsFont("TT Century Gothic"))
my.font <- "Century"

## IMPORT VALIDATION SITE DATA 

# load csv with site lat longs (change to your points of intreset)
my.sites <- fread(here("data/validation_sites.csv"))
n.sites <- my.sites[,.N]
sites.proj <- my.sites # convert data frame of lat-long points to coordinates (SpatialPointDataFrame)
coordinates(sites.proj) <- c("X", "Y") # assign X & Y
lon.lat.prj <- crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
proj4string(sites.proj) <- lon.lat.prj # project points to long-lat projection WSG84   "+proj=longlat +datum=WGS84"
print(sites.proj@coords, digits = 10) # check coords are correct

# buffer uza boundary by 1 mile (1.6 km)
uza.border <- shapefile(here("data/shapefiles/boundaries/maricopa_county_uza.shp")) # import Maricopa County UZA boundary
uza.buffer <- gBuffer(uza.border, byid = F, width = 5280) # add a one mile buffer to ensure no L8 data is lost when clipped

# set plot options
tmap_options(max.raster = c(plot = 2.5e+07, view = 2.5e+07)) # expand max extent of raster for tmap
my.palette <- rev(RColorBrewer::brewer.pal(11, "Spectral")) # color palette

## ASTER DATA:
# Coordinate System	Universal Transverse Mercator (UTM)
# Datum WGS84
# units: Kelvin
# 16-bit unsigned integer
# Fill value: N/A
# Valid range: 200 to 3200
# Scale Factor: 0.1

# list the subfolders that contain collections of geotif files for each day for each Landsat product
st.tiles <- list.dirs(here("data/aster"), recursive = F, full.names = F)

# create a list of lists that contain the .tif files by day
st.tile.list <- list.files(here("data/aster"), recursive = T, full.names = T, pattern="tif$")

# create a raster stack from the list of lists such that each top level is a day
# and under it is the stacked geotifs. e.g. to refrence 1st day 2nd layer/band: stack[[1]][[2]]
#st.tile.stack <- stack(st.tile.list)
st.tile.stack <- lapply(st.tile.list, function(x) raster(x, layer = 1)) # make sure each element is a raster layer not a brick/stack


# convert surface temps to celcius with decimal. -273.15 C == 0 K.
# if a temp is 2000 (200 K, - 73 C), it is outside the scope and should be adjusted to NA
for(r in 1:length(st.tile.stack)){
  values(st.tile.stack[[r]]) <- ifelse(values(st.tile.stack[[r]]) == 2000, NA, (values(st.tile.stack[[r]]) - (273.15 * 10)) / 10)
}

dir.create(here("data/aster/all"), showWarnings = FALSE) # creates output folder if it doesn't already exist
# for qgis
for(r in 1:length(st.tile.stack)){
  writeRaster(st.tile.stack[[r]], here("data/aster/all",paste0(names(st.tile.stack[[r]]),".tif")))
}



# get file paths and names for all aster metadata files
meta.files <- list.files(here("data/aster"), recursive = F, full.names = T, pattern = "met$")
meta.names <- gsub(".zip.met", "", list.files(here("data/aster"), recursive = F, full.names = F, pattern = "met$"))

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

# create data.table to store surface temperature extracted from each scene 
#st.by.site <- setnames(data.table(matrix(nrow = 0, ncol = length(my.sites$Location)+1)), c("id",my.sites$Location))
st.by.site <- setnames(data.table(matrix(nrow = 0, ncol = length(my.sites$Location))), my.sites$Location)
st.by.site <- rbind(list(id = meta.names), st.by.site, fill = T)
st.by.site[,2:(n.sites+1)] <- lapply(st.by.site[,2:(n.sites+1)], as.numeric)

# extract surface temps at locations
# order is same so no need to check if each extraction id matches
for(r in 1:st.by.site[,.N]){
  
  # extract data
  st.by.site[id == gsub(".SurfaceKineticTemperature.KineticTemperature", "", names(st.tile.stack[[r]])), 
             names(st.by.site[,2:(my.sites[,.N]+1)]) := 
               as.list(raster::extract(st.tile.stack[[r]], sites.crs, method = 'simple', small = F, cellnumbers = F,
                                       df = F, factors = F))]
}

# merge surface temp data to meta.data
meta.data <- merge(meta.data, st.by.site, by = "id", all = T)
meta.data[, idx := .I]

# day or night?
meta.data[hour(meta.data$date.time) >= 7 & hour(meta.data$date.time) < 18, day.time := "day"]
meta.data[hour(meta.data$date.time) < 7 | hour(meta.data$date.time) >= 18, day.time := "night"]

# cloud coverage quantiles
meta.data[, quantile(cloud, probs = c(0,0.1,0.25,0.5,0.6,0.7,0.75,0.8,0.9,1))]

# summaries of surface temps at night
meta.data[day.time == "day" & 
          cloud <= 10 & 
          lubridate::month(date.time, label = T, abbr = T) %in% c("May","Jun","Jul","Aug"), 
          max(A4, na.rm = T)]

my.idx <- meta.data[cloud <= 30 & 
           lubridate::month(date.time, label = T, abbr = T) %in% c("May","Jun","Jul","Aug") &
           lubridate::year(date.time) == 2017, idx]

# for qgis
#dir.create(here("data/aster processed/all new"), showWarnings = FALSE) # creates output folder if it doesn't already exist
#for(i in 1:length(my.idx)){
#  j <- as.integer(my.idx[i])
#  writeRaster(st.tile.stack[[j]], here("data/aster processed/all new",paste0(names(st.tile.stack[[j]]),".tif")), overwrite = T)
#}

# days of reference
meta.data[idx %in% my.idx, .(id,cloud,date.time,day.time)]

# create a list of lists that contain the .tif files by day
new.list <- list.files(here("data/aster processed/processed"), recursive = T, full.names = T, pattern="tif$")

# create a raster stack from the list of lists such that each top level is a day
# and under it is the stacked geotifs. e.g. to refrence 1st day 2nd layer/band: stack[[1]][[2]]
#st.tile.stack <- stack(st.tile.list)
new.stack <- lapply(new.list, function(x) raster(x, layer = 1)) # make sure each element is a raster layer not a brick/stack


# determine the bounding box of the raster cell for each validation site for the relevant scenes 


# function to calculate distance in kilometers between two lat/lon points
earth.dist <- function (long1, lat1, long2, lat2){
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}


# we need to determine of the 4 lat and lon points, which are UL, LL, UR, LR
# long: more negative (larger abs value in AZ) is west, closer to zero is east
# lat: larger (more positive) values is north

# determine min1st min2nd.... for all points at each row
meta.data[, `:=` (min.lon.1 = min(.SD),
                  min.lon.2 = quantile(c(lon1,lon2,lon3,lon4), 1/3),
                  min.lon.3 = quantile(c(lon1,lon2,lon3,lon4), 2/3),
                  min.lon.4 = max(.SD)),
          .SDcols = c("lon1","lon2","lon3","lon4"),
          by = 1:nrow(meta.data)]

meta.data[, `:=` (min.lat.1 = min(.SD),
                  min.lat.2 = quantile(c(lat1,lat2,lat3,lat4), 1/3),
                  min.lat.3 = quantile(c(lat1,lat2,lat3,lat4), 2/3),
                  min.lat.4 = max(.SD)),
          .SDcols = c("lat1","lat2","lat3","lat4"),
          by = 1:nrow(meta.data)]

# upper left
# if min.lon.2 and min.lat.4 are paired -> UL (slight right tilt)
meta.data[lon1 == min.lon.2 & lat1 == min.lat.4, `:=` (lon.UL = lon1, lat.UL = lat1)]
meta.data[lon2 == min.lon.2 & lat2 == min.lat.4, `:=` (lon.UL = lon2, lat.UL = lat2)]
meta.data[lon3 == min.lon.2 & lat3 == min.lat.4, `:=` (lon.UL = lon3, lat.UL = lat3)]
meta.data[lon4 == min.lon.2 & lat4 == min.lat.4, `:=` (lon.UL = lon4, lat.UL = lat4)]

# if min.lon.1 and min.lat.3 are paired -> UL (slight left tilt)
meta.data[lon1 == min.lon.1 & lat1 == min.lat.3, `:=` (lon.UL = lon1, lat.UL = lat1)]
meta.data[lon2 == min.lon.1 & lat2 == min.lat.3, `:=` (lon.UL = lon2, lat.UL = lat2)]
meta.data[lon3 == min.lon.1 & lat3 == min.lat.3, `:=` (lon.UL = lon3, lat.UL = lat3)]
meta.data[lon4 == min.lon.1 & lat4 == min.lat.3, `:=` (lon.UL = lon4, lat.UL = lat4)]

# lower left
# if min.lon.1 and min.lat.2 are paired -> LL (slight right tilt)
meta.data[lon1 == min.lon.1 & lat1 == min.lat.2, `:=` (lon.LL = lon1, lat.LL = lat1)]
meta.data[lon2 == min.lon.1 & lat2 == min.lat.2, `:=` (lon.LL = lon2, lat.LL = lat2)]
meta.data[lon3 == min.lon.1 & lat3 == min.lat.2, `:=` (lon.LL = lon3, lat.LL = lat3)]
meta.data[lon4 == min.lon.1 & lat4 == min.lat.2, `:=` (lon.LL = lon4, lat.LL = lat4)]

# if min.lon.2 and min.lat.1 are paired -> LL (slight left tilt)
meta.data[lon1 == min.lon.2 & lat1 == min.lat.1, `:=` (lon.LL = lon1, lat.LL = lat1)]
meta.data[lon2 == min.lon.2 & lat2 == min.lat.1, `:=` (lon.LL = lon2, lat.LL = lat2)]
meta.data[lon3 == min.lon.2 & lat3 == min.lat.1, `:=` (lon.LL = lon3, lat.LL = lat3)]
meta.data[lon4 == min.lon.2 & lat4 == min.lat.1, `:=` (lon.LL = lon4, lat.LL = lat4)]

# upper right
# if min.lon.4 and min.lat.3 are paired -> UL (slight right tilt)
meta.data[lon1 == min.lon.4 & lat1 == min.lat.3, `:=` (lon.UR = lon1, lat.UR = lat1)]
meta.data[lon2 == min.lon.4 & lat2 == min.lat.3, `:=` (lon.UR = lon2, lat.UR = lat2)]
meta.data[lon3 == min.lon.4 & lat3 == min.lat.3, `:=` (lon.UR = lon3, lat.UR = lat3)]
meta.data[lon4 == min.lon.4 & lat4 == min.lat.3, `:=` (lon.UR = lon4, lat.UR = lat4)]

# if min.lon.3 and min.lat.4 are paired -> UL (slight left tilt)
meta.data[lon1 == min.lon.3 & lat1 == min.lat.4, `:=` (lon.UR = lon1, lat.UR = lat1)]
meta.data[lon2 == min.lon.3 & lat2 == min.lat.4, `:=` (lon.UR = lon2, lat.UR = lat2)]
meta.data[lon3 == min.lon.3 & lat3 == min.lat.4, `:=` (lon.UR = lon3, lat.UR = lat3)]
meta.data[lon4 == min.lon.3 & lat4 == min.lat.4, `:=` (lon.UR = lon4, lat.UR = lat4)]

# lower right
# if min.lon.3 and min.lat.1 are paired -> LL (slight right tilt)
meta.data[lon1 == min.lon.3 & lat1 == min.lat.1, `:=` (lon.LR = lon1, lat.LR = lat1)]
meta.data[lon2 == min.lon.3 & lat2 == min.lat.1, `:=` (lon.LR = lon2, lat.LR = lat2)]
meta.data[lon3 == min.lon.3 & lat3 == min.lat.1, `:=` (lon.LR = lon3, lat.LR = lat3)]
meta.data[lon4 == min.lon.3 & lat4 == min.lat.1, `:=` (lon.LR = lon4, lat.LR = lat4)]

# if min.lon.4 and min.lat.2 are paired -> LL (slight left tilt)
meta.data[lon1 == min.lon.4 & lat1 == min.lat.2, `:=` (lon.LR = lon1, lat.LR = lat1)]
meta.data[lon2 == min.lon.4 & lat2 == min.lat.2, `:=` (lon.LR = lon2, lat.LR = lat2)]
meta.data[lon3 == min.lon.4 & lat3 == min.lat.2, `:=` (lon.LR = lon3, lat.LR = lat3)]
meta.data[lon4 == min.lon.4 & lat4 == min.lat.2, `:=` (lon.LR = lon4, lat.LR = lat4)]


# with all UL/LL/UR/LR assigned correctly we can resample rasters

# for every tile, extract the lat lon, convert to spatial points, 
# convert to extent, and create raster with extent


# update projections
aster.crs <- crs(st.tile.stack[[1]])
sites.crs <- spTransform(sites.proj, aster.crs)
uza.border.prj <- spTransform(uza.border, aster.crs) # project uza buffer to kat lon

# rectify
#tile.crop <- rectify(st.tile.stack[[s]], method = "bilinear")
#tile.crop <- rectify(st.tile.stack[[s]], method = "bilinear", res = c(90,90))
#tile.crop <- rectify(st.tile.stack[[s]], method = "bilinear", ext = extent(corners))
#tile.crop <- rectify(st.tile.stack[[s]], method = "bilinear", res = c(90,90), ext = extent(corners))

#tile.crop <- st.tile.stack[[s]][[1]]
#extent(tile.crop) <- extent(corners) # correct the extent

#new.tile <- crop(extend(st.tile.stack[[s]][[1]], corners), corners)
#all.equal(extent(st.tile.stack[[s]][[1]]), extent(new.tile))

#tile.crop <- extent(st.tile.stack[[s]][[1]])
#tile.crop <- spTransform(st.tile.stack[[s]][[1]], lon.lat.prj)


#spts <- rasterToPoints(st.tile.stack[[s]][[1]], spatial = TRUE)
#llpts <- spTransform(spts, lon.lat.prj)


# loop through all raster stacks and create surface temperature plots
for(s in 1:length(new.stack)){ #length(st.tile.stack)
  tryCatch({  # catch and print errors, avoids stopping model run 

    # corner points
    corners <- rbind(meta.data[s, .(lon.UL,lat.UL)], 
                     meta.data[s, .(lon.UR,lat.UR)],
                     meta.data[s, .(lon.LL,lat.LL)],
                     meta.data[s, .(lon.LR,lat.LR)], use.names = F)
    
    colnames(corners) <- c("X","Y") # name X & Y (lon & lat)
    coordinates(corners) <- c("X", "Y") # assign X & Y
    proj4string(corners) <- lon.lat.prj
    corners <- spTransform(corners, aster.crs)

    # fix the raster file parameters because the extents are incorrect 
    # we can't rectify b/c rotation isn't recognized so forced to resample
    #tile.crop <- rectify(st.tile.stack[[s]], method = "bilinear", res = c(90,90))
    tile.crop <- new.stack[[s]]
    
    #tile.crop <- resample(st.tile.stack[[s]], tile.crop, method = "bilinear") # resample to new extent w/ same resolution
    #tile.crop <- raster(here("data/aster-test.tif"))


    # create plot
    plot.2 <- tm_shape(tile.crop) + 
      tm_raster(palette = my.palette,
                style = "cont",
                title = "Surface Temperature \n(deg C)") +
      tm_shape(uza.border.prj) +
      tm_borders(lwd = 0.1, 
                 lty = "dashed", #"dashed", "dotted", "dotdash", "longdash", or "twodash"
                 col = "grey50",
                 alpha = 0.5) +
      tm_shape(sites.crs) +
      tm_dots(size = 0.1, title = "Validation Sites", shape = 4) +
      tm_shape(corners) +
      tm_dots(size = 0.15, alpha = 0.5) +
      tm_layout(fontfamily = my.font, 
                legend.title.size = 0.9, 
                legend.text.size = 0.8,
                legend.position = c(0.02,0.72),
                #title = paste0(my.date,"\n",my.sat,"\n",my.tile),
                title.size = 0.7,
                title.position = c(0.08,0.08))
    
    dir.create(here("figures/aster-best/"), showWarnings = FALSE) # creates output folder if it doesn't already exist
    tmap_save(plot.2, filename = here(paste0("figures/aster-best/", s,".png"))) # save plot
  }, error = function(e){cat("ERROR:",conditionMessage(e), "\n")}) # print error message if model run had error
}


##############

test1 <- fread(here("data/aster/AST_08_00301012005054020_20190206151852_27906/AST_08_00301012005054020_20190206151852_27906.SurfaceKineticTemperature.Longitude.txt"))
test2 <- fread(here("data/aster/AST_08_00301012005054020_20190206151852_27906/AST_08_00301012005054020_20190206151852_27906.SurfaceKineticTemperature.GeodeticLatitude.txt"))

test1 <- melt(test1)[,X := as.numeric(value)][,variable := NULL][,value := NULL]
test2 <- melt(test2)[,Y := as.numeric(value)][,variable := NULL][,value := NULL]

test.xy <- cbind(test1,test2)
coordinates(test.xy) <- c("X", "Y") # assign X & Y
proj4string(test.xy) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
test.xy <- spTransform(test.xy, crs(uza.border))


#corners1 <- rbind(meta.data[, .(lon1,lat1)])
#corners2 <- rbind(meta.data[, .(lon2,lat2)])
#corners3 <- rbind(meta.data[, .(lon3,lat3)])
#corners4 <- rbind(meta.data[, .(lon4,lat4)])

corners1 <- rbind(meta.data[, .(lon.UL,lat.UL)])
corners2 <- rbind(meta.data[, .(lon.LL,lat.LL)])
corners3 <- rbind(meta.data[, .(lon.UR,lat.UR)])
corners4 <- rbind(meta.data[, .(lon.LR,lat.LR)])

max.box <- data.table(rbind(c(max(meta.data$lon.UL),max(meta.data$lat.UL)),
                            c(min(meta.data$lon.UL),min(meta.data$lat.UL))))

colnames(max.box) <- c("X","Y") # name X & Y (lon & lat)
coordinates(max.box) <- c("X", "Y") # assign X & Y
proj4string(max.box) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


colnames(corners1) <- c("X","Y") # name X & Y (lon & lat)
coordinates(corners1) <- c("X", "Y") # assign X & Y
proj4string(corners1) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

colnames(corners2) <- c("X","Y") # name X & Y (lon & lat)
coordinates(corners2) <- c("X", "Y") # assign X & Y
proj4string(corners2) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

colnames(corners3) <- c("X","Y") # name X & Y (lon & lat)
coordinates(corners3) <- c("X", "Y") # assign X & Y
proj4string(corners3) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

colnames(corners4) <- c("X","Y") # name X & Y (lon & lat)
coordinates(corners4) <- c("X", "Y") # assign X & Y
proj4string(corners4) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

corners1 <- spTransform(corners1, crs(uza.border))
corners2 <- spTransform(corners2, crs(uza.border))
corners3 <- spTransform(corners3, crs(uza.border))
corners4 <- spTransform(corners4, crs(uza.border))

# create plot
plot <- #tm_shape(uza.border.prj) +
  #tm_borders(lwd = 0.1, 
  #           lty = "dashed", #"dashed", "dotted", "dotdash", "longdash", or "twodash"
  #           col = "grey50") +
  tm_shape(max.box) +
  tm_dots(size = 0.1, shape = 4, col = "gray") +
  tm_shape(test.xy) +
  tm_dots(size = 0.1, shape = 4, col = "gray") +
  tm_shape(corners1) +
  tm_dots(size = 0.1, shape = 4, col = "red") +
  tm_shape(corners2) +
  tm_dots(size = 0.1, shape = 4, col = "blue") +
  tm_shape(corners3) +
  tm_dots(size = 0.1, shape = 4, col = "green") +
  tm_shape(corners4) +
  tm_dots(size = 0.1, shape = 4, col = "black")

dir.create(here("figures/aster/"), showWarnings = FALSE) # creates output folder if it doesn't already exist
tmap_save(plot, filename = here(paste0("figures/aster-best/corners.png"))) # save plot

###########
test.xy.dt <- cbind(test1,test2)
test.xy.dt[, dist.km := earth.dist(X, Y, shift(X, "lag", n = 1), shift(Y, "lag", n = 1))] 
test.xy.dt[, dist.km]



# CHECK ALL POINTS TO MAKE SURE THE ARE CORRECT
for(s in 1:100){ #length(st.tile.stack)
  tryCatch({  # catch and print errors, avoids stopping model run 
    corners1 <- rbind(meta.data[, .(lon.UL,lat.UL)])
    corners2 <- rbind(meta.data[, .(lon.LL,lat.LL)])
    corners3 <- rbind(meta.data[, .(lon.UR,lat.UR)])
    corners4 <- rbind(meta.data[, .(lon.LR,lat.LR)])
    
    max.box <- data.table(rbind(c(max(meta.data$lon.UL),max(meta.data$lat.UL)),
                                c(max(meta.data$lon.UR),max(meta.data$lat.UR)),
                                c(min(meta.data$lon.LL),min(meta.data$lat.LL)),
                                c(min(meta.data$lon.LR),min(meta.data$lat.LR))))
    
    colnames(max.box) <- c("X","Y") # name X & Y (lon & lat)
    coordinates(max.box) <- c("X", "Y") # assign X & Y
    proj4string(max.box) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    
    
    colnames(corners1) <- c("X","Y") # name X & Y (lon & lat)
    coordinates(corners1) <- c("X", "Y") # assign X & Y
    proj4string(corners1) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    
    colnames(corners2) <- c("X","Y") # name X & Y (lon & lat)
    coordinates(corners2) <- c("X", "Y") # assign X & Y
    proj4string(corners2) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    
    colnames(corners3) <- c("X","Y") # name X & Y (lon & lat)
    coordinates(corners3) <- c("X", "Y") # assign X & Y
    proj4string(corners3) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    
    colnames(corners4) <- c("X","Y") # name X & Y (lon & lat)
    coordinates(corners4) <- c("X", "Y") # assign X & Y
    proj4string(corners4) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    
    corners1 <- spTransform(corners1, crs(uza.border))
    corners2 <- spTransform(corners2, crs(uza.border))
    corners3 <- spTransform(corners3, crs(uza.border))
    corners4 <- spTransform(corners4, crs(uza.border))
    
    # create plot
    plot <- 
      tm_shape(max.box) +
      tm_dots(size = 0.2, shape = 16, col = "black") +
      tm_shape(uza.border.prj) +
      tm_borders(lwd = 0.1, 
                 lty = "dashed", #"dashed", "dotted", "dotdash", "longdash", or "twodash"
                 col = "grey50") +
      tm_shape(corners1) +
      tm_dots(size = 0.2, shape = 4, col = "red") +
      tm_shape(corners2) +
      tm_dots(size = 0.2, shape = 4, col = "blue") +
      tm_shape(corners3) +
      tm_dots(size = 0.2, shape = 4, col = "green") +
      tm_shape(corners4) +
      tm_dots(size = 0.2, shape = 4, col = "black")
    
    dir.create(here("figures/aster-test/"), showWarnings = FALSE) # creates output folder if it doesn't already exist
    tmap_save(plot, filename = here(paste0("figures/aster-test/corners_","all",".png"))) # save plot
  }, error = function(e){cat("ERROR:",conditionMessage(e), "\n")}) # print error message if model run had error
}


# References

# [1] https://asterweb.jpl.nasa.gov/content/03_data/04_Documents/ASTERHigherLevelUserGuideVer2May01.pdf