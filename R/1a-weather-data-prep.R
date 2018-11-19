#############################################################################
## DATA IMPORT SCRIPT FOR 2017 PHOENIX, AZ WEATHER DATA - VARIOUS SOURCES ##
###########################################################################

# list of all dependant packages
list.of.packages <- c("httr",
                      "jsonlite",
                      "lubridate",
                      "weathermetrics",
                      "tidyverse",
                      "data.table",
                      "rnoaa",
                      "here")

# install missing packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load packages
invisible(lapply(list.of.packages, library, character.only = TRUE))


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Import Arizona Meteorological Network data  #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

encanto <- rbindlist(lapply(list.files(here(path = "data/weather data/AZMET/Encanto/raw"), pattern="txt$", full.names= T), read.csv, header = F))
desert.ridge <- rbindlist(lapply(list.files(here(path = "data/weather data/AZMET/Desert Ridge/raw"), pattern="txt$", full.names= T), read.csv, header = F))
mesa <- rbindlist(lapply(list.files(here(path = "data/weather data/AZMET/Mesa/raw"), pattern="txt$", full.names= T), read.csv, header = F))
greenway <- rbindlist(lapply(list.files(here(path = "data/weather data/AZMET/Greenway/raw"), pattern="txt$", full.names= T), read.csv, header = F))

# add station ID to each dataset
encanto$station <- "Encanto"
desert.ridge$station <- "Desert Ridge"
mesa$station <- "Mesa"
greenway$station <- "Greenway"

# bind all together, remove individual dfs
azmet.data <- rbind(encanto,desert.ridge,mesa,greenway)

# clean up space
rm(encanto,desert.ridge,mesa,greenway)
gc()

# assign column names (see data dictionary)
colnames(azmet.data) <- c("year","day.year","hour","temp.c","rh","vap","sol","precip","soil.4.in","soil.20.in","winspd","wind.mag","windir","wind.dir.dev","max.wind.spd","ETo","act.vap","dewpt.c","station.name")

# fix temp = 999 to NA
azmet.data$temp.c <- ifelse(azmet.data$temp.c == 999, NA, azmet.data$temp.c)

# create fahrenheit temp/dew
azmet.data[, temp.f := ((temp.c * 1.8) + 32)]
azmet.data[, dewpt.f := ((dewpt.c * 1.8) + 32)]

# add date column
azmet.data$date <- as.Date(azmet.data$day.year-1, origin = paste0(azmet.data$year,"-01-01"))

# format date + time in new column as YYYY-MM-DD HH:MM:SS
azmet.data$date.time  <- ymd_hm(paste0(azmet.data$date," ",azmet.data$hour,":00"), tz = "US/Arizona")

# read AZMET station data
azmet.stations <- fread(here("data/weather data/AZMET/stations.csv"))

# add data source column and id column (NA, but for ordered cols later)
azmet.data$source <- "AZMET"
azmet.stations$source <- "AZMET"
azmet.data$id <- NA
azmet.stations$id <- NA

# keep relevant columns only
azmet.data <- azmet.data[,c("source","station.name","date.time","temp.f","dewpt.f","temp.c","dewpt.c","winspd","windir")]
azmet.stations <- azmet.stations[,c("source","station.name","lat","lon","elevation","id")]

# keep 2017 data only
azmet.data <- azmet.data[year(date.time) == 2017]


#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
# Import National Centers for Environmental Information data  #
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#

ncei.data <- read_fwf(here("data/weather data/NCEI/2016_data.txt"), skip = 1,
                       fwf_empty(here("data/weather data/NCEI/2016_data.txt"),
                                 col_names = c("id","WBAN","date","windir","winspd","GUS","CLG","SKC","L","M","H","VSB","MW1","MW2","MW3","MW4","AW1","AW2","AW3","AW4","W","temp.f","dewpt.f","SLP","ALT","STP","MAX","MIN","PCP01","PCP06","PCP24","PCPXX","SD")))

ncei.stations <- read_fwf(here(path = "data/weather data/NCEI/stations.txt"), skip = 2, fwf_widths(c(7,6,31,51,31,9,9,10), c("id","WBAN","station.name","COUNTRY","STATE","lat","lon","elevation")))

# set as data.table
setDT(ncei.data)
setDT(ncei.stations)

# convert variables to numeric (via characters because "NA" is "**")
ncei.data[, temp.f := as.numeric(as.character(temp.f))]
ncei.data[, dewpt.f := as.numeric(as.character(dewpt.f))]
ncei.data[, winspd := as.numeric(as.character(winspd))]
ncei.data[, windir := as.numeric(as.character(windir))]

# create celcius temp/dew
ncei.data[, temp.c := ((temp.f - 32) / 1.8)]
ncei.data[, dewpt.c := ((dewpt.f - 32) / 1.8)]

# format date + time in new column as YYYY-MM-DD HH:MM:SS
ncei.data$date.time  <- ymd_hm(ncei.data$date, tz = "UTC")

# convert to AZ timezone
ncei.data$date.time <- with_tz(ncei.data$date.time, tz = "US/Arizona") # change timezone to local (AZ)

# merge station.name to data
ncei.data <- merge(ncei.data, ncei.stations[,.(station.name,id)], by = "id")

# add data source column
ncei.data$source <- "NCEI"
ncei.stations$source <- "NCEI"

# keep relevant columns only
ncei.data <- ncei.data[,c("source","station.name","date.time","temp.f","dewpt.f","temp.c","dewpt.c","winspd","windir")]
ncei.stations <- ncei.stations[,c("source","station.name","lat","lon","elevation","id")]

# keep 2017 data only
ncei.data <- ncei.data[year(date.time) == 2017]


#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
# Import Maricopa County Flood Control District data  #
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#

# temp data
temp.files <- list.files(here(path = "data/weather data/MCFCD/2017/Temp"), pattern="txt$", full.names = T) # full file path names
temp.names <- gsub(".txt", "", list.files(here(path = "data/weather data/MCFCD/2017/Temp"), pattern = "txt$", full.names = F)) # names (stations) of files
mcfcd.temp.data <- rbindlist(lapply(temp.files, fread), idcol = "id") # load all station data 
mcfcd.temp.data[, id := factor(id, labels = basename(temp.names))] # add station names to column 'station'
mcfcd.temp.data[, id := as.character(id)] # convert to character for matching
colnames(mcfcd.temp.data) <- c("id","date","time","temp.f") # add rest of column names
mcfcd.temp.data$date.time  <- mdy_hms(paste0(mcfcd.temp.data$date," ",mcfcd.temp.data$time), tz = "US/Arizona") # MCFCD data is all local

# dewpt data
dewpt.files <- list.files(here(path = "data/weather data/MCFCD/2017/Dewpoint"), pattern="txt$", full.names = T) # full file path names
dewpt.names <- gsub(".txt", "", list.files(here(path = "data/weather data/MCFCD/2017/Dewpoint"), pattern = "txt$", full.names = F)) # names (stations) of files
mcfcd.dewpt.data <- rbindlist(lapply(dewpt.files, fread), idcol = "id") # load all station data 
mcfcd.dewpt.data[, id := factor(id, labels = basename(dewpt.names))] # add station names to column 'station'
mcfcd.dewpt.data[, id := as.character(id)] # convert to character for matching
colnames(mcfcd.dewpt.data) <- c("id","date","time","dewpt.f") # add rest of column names
mcfcd.dewpt.data$date.time  <- mdy_hms(paste0(mcfcd.dewpt.data$date," ",mcfcd.dewpt.data$time), tz = "US/Arizona") # MCFCD data is all local

# windir data
windir.files <- list.files(here(path = "data/weather data/MCFCD/2017/Wdir"), pattern="txt$", full.names = T) # full file path names
windir.names <- gsub(".txt", "", list.files(here(path = "data/weather data/MCFCD/2017/Wdir"), pattern = "txt$", full.names = F)) # names (stations) of files
mcfcd.windir.data <- rbindlist(lapply(windir.files, fread), idcol = "id") # load all station data 
mcfcd.windir.data[, id := factor(id, labels = basename(windir.names))] # add station names to column 'station'
mcfcd.windir.data[, id := as.character(id)] # convert to character for matching
colnames(mcfcd.windir.data) <- c("id","date","time","windir") # add rest of column names
mcfcd.windir.data$date.time  <- mdy_hms(paste0(mcfcd.windir.data$date," ",mcfcd.windir.data$time), tz = "US/Arizona") # MCFCD data is all local

# winspd data
winspd.files <- list.files(here(path = "data/weather data/MCFCD/2017/Wspd"), pattern="txt$", full.names = T) # full file path names
winspd.names <- gsub(".txt", "", list.files(here(path = "data/weather data/MCFCD/2017/Wspd"), pattern = "txt$", full.names = F)) # names (stations) of files
mcfcd.winspd.data <- rbindlist(lapply(winspd.files, fread), idcol = "id") # load all station data 
mcfcd.winspd.data[, id := factor(id, labels = basename(winspd.names))] # add station names to column 'station'
mcfcd.winspd.data[, id := as.character(id)] # convert to character for matching
colnames(mcfcd.winspd.data) <- c("id","date","time","winspd") # add rest of column names
mcfcd.winspd.data$date.time  <- mdy_hms(paste0(mcfcd.winspd.data$date," ",mcfcd.winspd.data$time), tz = "US/Arizona") # MCFCD data is all local

# solar data
solar.files <- list.files(here(path = "data/weather data/MCFCD/2017/Solar"), pattern="txt$", full.names = T) # full file path names
solar.names <- gsub(".txt", "", list.files(here(path = "data/weather data/MCFCD/2017/Solar"), pattern = "txt$", full.names = F)) # names (stations) of files
mcfcd.solar.data <- rbindlist(lapply(solar.files, fread), idcol = "id") # load all station data 
mcfcd.solar.data[, id := factor(id, labels = basename(solar.names))] # add station names to column 'station'
mcfcd.solar.data[, id := as.character(id)] # convert to character for matching
colnames(mcfcd.solar.data) <- c("id","date","time","solar") # add rest of column names
mcfcd.solar.data$date.time  <- mdy_hms(paste0(mcfcd.solar.data$date," ",mcfcd.solar.data$time), tz = "US/Arizona") # MCFCD data is all local

# station data
mcfcd.stations <- fread(here("data/weather data/MCFCD/stations.csv")) # load station data
mcfcd.stations <- mcfcd.stations[!is.na(id)]  # remove stations without an ID
mcfcd.stations[, id := as.character(id)] # convert id to character for matching
dms2dec <- function(x){ 
  z <- sapply(strsplit(x, " "), as.numeric)
  z[1, ] + z[2, ]/60 + z[3, ]/3600
}
mcfcd.stations$lat <- dms2dec(mcfcd.stations$lat.dms) # convert station deg-min-sec to decimal lat
mcfcd.stations$lon <- dms2dec(mcfcd.stations$lon.dms) # convert station deg-min-sec to decimal lon
mcfcd.stations$lon <- (-1) * mcfcd.stations$lon # arizona is west of prime meridian so longitude should be negative

# in temp data, stations 1001 and 28301 are missing (both outside urbanized area, so not essential to replace)
# in temp data, station 1002 is extra (removed, station is humidity data w/ 0 < x > 100)
# in dewpt data, stations 30816 and 34916 are extra (removed, no stations w/ those IDs in index, data is consistent with dewpt data tho)

# bind station names to each data type, then full join all data together by station.name and date.time 
#(note: solar rad data is only every 30 min instead of other four at 15 min)
mcfcd.temp.data <- merge(mcfcd.temp.data, mcfcd.stations[,.(station.name,id)], by = "id")
mcfcd.dewpt.data <- merge(mcfcd.dewpt.data, mcfcd.stations[,.(station.name,id)], by = "id")
mcfcd.windir.data <- merge(mcfcd.windir.data, mcfcd.stations[,.(station.name,id)], by = "id")
mcfcd.winspd.data <-merge(mcfcd.winspd.data, mcfcd.stations[,.(station.name,id)], by = "id")
mcfcd.solar.data <- merge(mcfcd.solar.data, mcfcd.stations[,.(station.name,id)], by = "id")

mcfcd.data <- merge(mcfcd.temp.data[,.(date.time,station.name,temp.f)], mcfcd.dewpt.data[,.(date.time,station.name,dewpt.f)], by = c("date.time","station.name"), all = T)
mcfcd.data <- merge(mcfcd.data, mcfcd.windir.data[,.(date.time,station.name,windir)], by = c("date.time","station.name"), all = T)
mcfcd.data <- merge(mcfcd.data, mcfcd.winspd.data[,.(date.time,station.name,winspd)], by = c("date.time","station.name"), all = T)
mcfcd.data <- merge(mcfcd.data, mcfcd.solar.data[,.(date.time,station.name,solar)], by = c("date.time","station.name"), all = T)

# clean up space
rm(mcfcd.temp.data, mcfcd.dewpt.data, mcfcd.windir.data, mcfcd.winspd.data, mcfcd.solar.data, 
   temp.files, temp.names, dewpt.files, dewpt.names, windir.files, windir.names, winspd.files, winspd.names, solar.files, solar.names)
gc()

# create celcius temp/dew
mcfcd.data[, temp.c := ((temp.f - 32) / 1.8)]
mcfcd.data[, dewpt.c := ((dewpt.f - 32) / 1.8)]

# add data source column
mcfcd.data$source <- "MCFCD"
mcfcd.stations$source <- "MCFCD"

# filter out extraneous stations by sensor.type
mcfcd.stations <- mcfcd.stations[sensor.type %in% c("Temperature","Dewpoint","Wind Dir.","Peak Wind","Solar Rad.")]

# keep relevant columns only
mcfcd.stations <- mcfcd.stations[,c("source","station.name","lat","lon","elevation")]

# simplify station list
mcfcd.stations <- unique(mcfcd.stations)

# keep 2017 data only
mcfcd.data <- mcfcd.data[year(date.time) == 2017]


#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#
# Import UWIN Phoenix iButton data  #
#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#

# import temp and rel humidity data
ibut.temp <- fread(here("data/weather data/iButton/phxbuttons.csv"))
ibut.rhum <- fread(here("data/weather data/iButton/phxbuttonsrh.csv"))

# import station info and data
ibut.stations <- fread(here("data/weather data/iButton/phxdatsite_all.csv"))
ibut.stations.col.info <- fread(here("data/weather data/iButton/phxdatsiteall_col_description.csv"))

# rename some cols
setnames(ibut.temp, "Date - Time", "date.time") 
setnames(ibut.rhum, "Date - Time", "date.time")
setnames(ibut.stations, "ibuttonID", "station.name")
setnames(ibut.stations, "Lat", "lat")
setnames(ibut.stations, "Lon", "lon")

# keep only relevant columns
ibut.stations <- ibut.stations[, .(station.name,lat,lon)]

# remove hour column
ibut.temp[, hour := NULL] 
ibut.rhum[, hour := NULL] 

# melt and recast temp & hum data b/c in wide format
ibut.temp <- melt(ibut.temp, id = "date.time", variable.name = "station.name", value.name = "temp.c")
ibut.rhum <- melt(ibut.rhum, id = "date.time", variable.name = "station.name", value.name = "rhum.c")

# format date.time as YYYY-MM-DD HH:MM:SS
ibut.temp[, date.time := mdy_hm(date.time, tz = "US/Arizona")]
ibut.rhum[, date.time := mdy_hm(date.time, tz = "US/Arizona")]

# round times of temp cause they are 1 min shifted off
ibut.temp[, date.time := as.POSIXct(round.POSIXt(date.time, "hours"))]

# bind temp and hum toegether by id & date.time
ibut.data <- merge(ibut.temp, ibut.rhum, by = c("date.time","station.name"), all = T)

# convert rhum to dewpt
ibut.data[, dewpt.c := humidity.to.dewpoint(rh = rhum.c, t = temp.c, temperature.metric = "celsius")]
ibut.data[, rhum.c := NULL] 

# create fahrenheit temp/dew
ibut.data[, temp.f := ((temp.c * 1.8) + 32)]
ibut.data[, dewpt.f := ((dewpt.c * 1.8) + 32)]

# add data source column
ibut.data$source <- "iButton"
ibut.stations$source <- "iButton"

# clean up space
rm(ibut.temp,ibut.rhum,ibut.stations.col.info)
gc()


#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#
# Retrieve Global Historical Climatology Network daily (GHCND) weather data #
#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#

# note this is the only data set that is currently not hourly, and is only daily summaries (min/max temps)

# get global list of all ghcnd stations
#ghcnd.stations <- as.data.table(ghcnd_stations())
#saveRDS(ghcnd.stations, here("data/outputs/ghcnd-station-data.rds"))
ghcnd.stations <- readRDS(here("data/outputs/ghcnd-station-data.rds"))

# set the center of ea downtown to search for nearest stations in radius of metro
coords <- data.frame(id = c("phx","la"), lat = c(33.453808, 34.055997), lon = c(-112.071277, -117.715813)) 

# filter stations to list of stations in 60km radius of (approx.) metro center
# search radius = 90km (should capture all of metro)
my.stations <- meteo_nearby_stations(lat_lon_df = coords, lat_colname = "lat",
                                     lon_colname = "lon", station_data = ghcnd.stations, var = c("TMAX", "TMIN"),
                                     year_min = 2016, year_max = 2018, radius = 90, limit = NULL)

# pull 2017 data from all stations and binds together (phx)
phx.ghcnd.data <- as.data.table(meteo_pull_monitors(my.stations$phx$id, date_min = "2017-01-01", date_max = "2017-12-31", var = c("TMAX", "TMIN")))
la.ghcnd.data <- as.data.table(meteo_pull_monitors(my.stations$la$id, date_min = "2017-01-01", date_max = "2017-12-31", var = c("TMAX", "TMIN")))

# create function to create the decimal in the temp data (raw data does not have the decminal but we'll need it)
fix.ghcnd.temp <- function(t) {
  as.numeric(
    paste0(
      substr(t, 1, nchar(t) - 1),
      ".",
      substr(t, nchar(t), nchar(t))))
}

# apply function and fix data
phx.ghcnd.data$tmax <- fix.ghcnd.temp(phx.ghcnd.data$tmax)
phx.ghcnd.data$tmin <- fix.ghcnd.temp(phx.ghcnd.data$tmin)

la.ghcnd.data$tmax <- fix.ghcnd.temp(la.ghcnd.data$tmax)
la.ghcnd.data$tmin <- fix.ghcnd.temp(la.ghcnd.data$tmin)

# add elevation to station data from metadata of all stations
my.stations$phx <- unique(merge(my.stations$phx, ghcnd.stations[, .(elevation,id)], by = "id"))
my.stations$la <- unique(merge(my.stations$la, ghcnd.stations[, .(elevation,id)], by = "id"))

# rename to merge w/ all stations
setnames(my.stations$phx, "name", "station.name")
setnames(my.stations$phx, "latitude", "lat")
setnames(my.stations$phx, "longitude", "lon")

setnames(my.stations$la, "name", "station.name")
setnames(my.stations$la, "latitude", "lat")
setnames(my.stations$la, "longitude", "lon")

# create source column
my.stations$phx$source <- "GHCND"
my.stations$la$source <- "GHCND"

# remove 'distance' column
my.stations$phx$distance <- NULL
my.stations$la$distance <- NULL


#^#^#^#^#^#^#^#^#^#^#^#^#^#
# Merge all data together #
#^#^#^#^#^#^#^#^#^#^#^#^#^#

# combine all houlry weather data into one object, and all stations into one object
w.data <- rbindlist(list(azmet.data,ncei.data,mcfcd.data,ibut.data), use.names = T, fill = T)
w.stations <- rbindlist(c(list(azmet.stations,ncei.stations,mcfcd.stations,ibut.stations), my.stations), use.names = T, fill = T)

# calculate heat index, (National Weather Surface improved estimate of Steadman eqn. (heat index calculator eqns)
w.data[, heat.f := heat.index(t = temp.f, dp = dewpt.f, temperature.metric = "fahrenheit", output.metric = "fahrenheit", round = 0)]
w.data[, heat.c := heat.index(t = temp.c, dp = dewpt.c, temperature.metric = "celsius", output.metric = "celsius", round = 1)]

# add some other variables 
w.data[, month := as.factor(month(date.time, label = T))]
w.data[, week := as.factor(week(date.time))]
w.data[, day := as.factor(day(date.time))]
w.data[, wday := as.factor(wday(date.time, label = T))]
w.data[, hour := hour(date.time)]

# create rounded data.time to more easily filter by observations on/near the hour
w.data[, date.time.round := as.POSIXct(round.POSIXt(date.time, "hours"))]

# calculate observations in 2017 for each station by each data type
 for(station in w.stations$station.name){
   w.stations[station == station.name, n.temp := sum(!is.na(w.data$temp.f[station == w.data$station.name]))]
   w.stations[station == station.name, n.dewpt := sum(!is.na(w.data$dewpt.f[station == w.data$station.name]))]
   w.stations[station == station.name, n.windir := sum(!is.na(w.data$windir[station == w.data$station.name]))]
   w.stations[station == station.name, n.winspd := sum(!is.na(w.data$winspd[station == w.data$station.name]))]
   w.stations[station == station.name, n.solar := sum(!is.na(w.data$solar[station == w.data$station.name]))]
   w.stations[station == station.name, n.heat := sum(!is.na(w.data$heat.f[station == w.data$station.name]))]
 }

# remove stations with no temp obs
#w.stations <- w.stations[n.temp != 0]

# save final data object
saveRDS(w.data, here("data/outputs/2017-weather-data.rds")) # all houlry data
saveRDS(phx.ghcnd.data, here("data/outputs/2017-ghcnd-weather-data.rds")) # daily summary (non-hourly) data
saveRDS(w.stations, here("data/outputs/station-data.rds")) # all station data


## MESO WEST DATA RETREIVAL VIA API

# station metadata link w/o token
s.link <- "http://api.mesowest.net/v2/stations/metadata?state=AZ&county=Maricopa&status=ACTIVE&token="

# load personal api token (note you'll need your own)
token <- read_file(here("local-token.txt"))

# get station metadata (as JSON)
meso.metadata <- fromJSON(paste0(s.link,token))
meso.s.data <- as.data.table(meso.metadata$STATION)

# store partial weather data links (front half and variables list)
w.link.f <- "http://api.mesowest.net/v2/stations/timeseries?&stid="
w.link.vars <- "&vars=air_temp,relative_humidity,wind_speed,wind_direction&obtimezone=local"

# define start and end times in correct format for api link retrieval
start <- "201701010000" # start time in YYYYMMDDHHSS (UTC)
end <- "201712312359"   # end time in YYYYMMDDHHSS (UTC)

# loop through each station and retrieve sson data
meso.w.data <- list()
for(i in 1:nrow(meso.s.data)){
  
  # retieve json data for station 'i' based on start and end time, custom api token, and desired variables
  j <- fromJSON(paste0(w.link.f,meso.s.data$STID[i],"&start=",start,"&end=",end,"&token=",token,w.link.vars))
  
  # if there is data at the location, get it
  if(is_empty(j$STATION) == F){
    
    # coerce to data.table in list (need to unlist columns)
    meso.w.data[[i]] <- as.data.table(sapply(j$STATION$OBSERVATIONS, unlist))
    
    # if vars exists, rename it for consistency w/ other data
    if(is.null(meso.w.data[[i]]$date_time) == F){setnames(meso.w.data[[i]], "date_time", "date.time")}
    if(is.null(meso.w.data[[i]]$relative_humidity_set_1) == F){setnames(meso.w.data[[i]], "relative_humidity_set_1", "rh")}
    if(is.null(meso.w.data[[i]]$wind_speed_set_1) == F){setnames(meso.w.data[[i]], "wind_speed_set_1", "winspd")}
    if(is.null(meso.w.data[[i]]$wind_direction_set_1) == F){setnames(meso.w.data[[i]], "wind_direction_set_1", "windir")}
    if(is.null(meso.w.data[[i]]$air_temp_set_1) == F){setnames(meso.w.data[[i]], "air_temp_set_1", "temp.c")}
    
    # create station column name and list index name based on the station id (STID) for refrencing
    names(meso.w.data)[i] <- j$STATION$STID
    meso.w.data[[i]]$station.name <- j$STATION$STID
  }
}

# save retrieved MesoWest station and weather data
saveRDS(meso.s.data, here("data/outputs/meso-station-data.rds"))
saveRDS(meso.w.data, here("data/outputs/meso-weather-data.rds"))
