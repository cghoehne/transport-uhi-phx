#############################################################################
## DATA IMPORT SCRIPT FOR 2017 PHOENIX, AZ WEATHER DATA - VARIOUS SOURCES ##
###########################################################################

library(data.table)
library(tidyverse)
library(lubridate)
library(weathermetrics)
library(here)


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Import Arizona Meteorological Network data  #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

encanto <- rbindlist(lapply(list.files(here(path = "data/AZMET/Encanto/raw"), pattern="txt$", full.names= T), read.csv, header = F))
desert.ridge <- rbindlist(lapply(list.files(here(path = "data/AZMET/Desert Ridge/raw"), pattern="txt$", full.names= T), read.csv, header = F))
mesa <- rbindlist(lapply(list.files(here(path = "data/AZMET/Mesa/raw"), pattern="txt$", full.names= T), read.csv, header = F))
greenway <- rbindlist(lapply(list.files(here(path = "data/AZMET/Greenway/raw"), pattern="txt$", full.names= T), read.csv, header = F))

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
azmet.stations <- fread(here("data/AZMET/stations.csv"))

# add data source column and station.id column (NA, but for ordered cols later)
azmet.data$source <- "AZMET"
azmet.stations$source <- "AZMET"
azmet.data$station.id <- NA
azmet.stations$station.id <- NA

# keep relevant columns only
azmet.data <- azmet.data[,c("source","station.name","date.time","temp.f","dewpt.f","temp.c","dewpt.c","winspd","windir")]
azmet.stations <- azmet.stations[,c("source","station.name","lat","lon","elevation")]

# keep 2017 data only
azmet.data <- azmet.data[year(date.time) == 2017]


#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
# Import National Centers for Environmental Information data  #
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#

ncei.data <- read_fwf(here("data/NCEI/2016_data.txt"), skip = 1,
                       fwf_empty(here("data/NCEI/2016_data.txt"),
                                 col_names = c("station.id","WBAN","date","windir","winspd","GUS","CLG","SKC","L","M","H","VSB","MW1","MW2","MW3","MW4","AW1","AW2","AW3","AW4","W","temp.f","dewpt.f","SLP","ALT","STP","MAX","MIN","PCP01","PCP06","PCP24","PCPXX","SD")))

ncei.stations <- read_fwf(here(path = "data/NCEI/stations.txt"), skip = 2, fwf_widths(c(7,6,31,51,31,9,9,10), c("station.id","WBAN","station.name","COUNTRY","STATE","lat","lon","elevation")))

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
ncei.data <- merge(ncei.data, ncei.stations[,.(station.name,station.id)], by = "station.id")

# add data source column
ncei.data$source <- "NCEI"
ncei.stations$source <- "NCEI"

# keep relevant columns only
ncei.data <- ncei.data[,c("source","station.name","date.time","temp.f","dewpt.f","temp.c","dewpt.c","winspd","windir")]
ncei.stations <- ncei.stations[,c("source","station.name","lat","lon","elevation")]

# keep 2017 data only
ncei.data <- ncei.data[year(date.time) == 2017]


#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
# Import Maricopa County Flood Control District data  #
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#

# temp data
temp.files <- list.files(here(path = "data/MCFCD/2017/Temp"), pattern="txt$", full.names = T) # full file path names
temp.names <- gsub(".txt", "", list.files(here(path = "data/MCFCD/2017/Temp"), pattern = "txt$", full.names = F)) # names (stations) of files
mcfcd.temp.data <- rbindlist(lapply(temp.files, fread), idcol = "station.id") # load all station data 
mcfcd.temp.data[, station.id := factor(station.id, labels = basename(temp.names))] # add station names to column 'station'
mcfcd.temp.data[, station.id := as.character(station.id)] # convert to character for matching
colnames(mcfcd.temp.data) <- c("station.id","date","time","temp.f") # add rest of column names
mcfcd.temp.data$date.time  <- mdy_hms(paste0(mcfcd.temp.data$date," ",mcfcd.temp.data$time), tz = "US/Arizona") # MCFCD data is all local

# dewpt data
dewpt.files <- list.files(here(path = "data/MCFCD/2017/Dewpoint"), pattern="txt$", full.names = T) # full file path names
dewpt.names <- gsub(".txt", "", list.files(here(path = "data/MCFCD/2017/Dewpoint"), pattern = "txt$", full.names = F)) # names (stations) of files
mcfcd.dewpt.data <- rbindlist(lapply(dewpt.files, fread), idcol = "station.id") # load all station data 
mcfcd.dewpt.data[, station.id := factor(station.id, labels = basename(dewpt.names))] # add station names to column 'station'
mcfcd.dewpt.data[, station.id := as.character(station.id)] # convert to character for matching
colnames(mcfcd.dewpt.data) <- c("station.id","date","time","dewpt.f") # add rest of column names
mcfcd.dewpt.data$date.time  <- mdy_hms(paste0(mcfcd.dewpt.data$date," ",mcfcd.dewpt.data$time), tz = "US/Arizona") # MCFCD data is all local

# windir data
windir.files <- list.files(here(path = "data/MCFCD/2017/Wdir"), pattern="txt$", full.names = T) # full file path names
windir.names <- gsub(".txt", "", list.files(here(path = "data/MCFCD/2017/Wdir"), pattern = "txt$", full.names = F)) # names (stations) of files
mcfcd.windir.data <- rbindlist(lapply(windir.files, fread), idcol = "station.id") # load all station data 
mcfcd.windir.data[, station.id := factor(station.id, labels = basename(windir.names))] # add station names to column 'station'
mcfcd.windir.data[, station.id := as.character(station.id)] # convert to character for matching
colnames(mcfcd.windir.data) <- c("station.id","date","time","windir") # add rest of column names
mcfcd.windir.data$date.time  <- mdy_hms(paste0(mcfcd.windir.data$date," ",mcfcd.windir.data$time), tz = "US/Arizona") # MCFCD data is all local

# winspd data
winspd.files <- list.files(here(path = "data/MCFCD/2017/Wspd"), pattern="txt$", full.names = T) # full file path names
winspd.names <- gsub(".txt", "", list.files(here(path = "data/MCFCD/2017/Wspd"), pattern = "txt$", full.names = F)) # names (stations) of files
mcfcd.winspd.data <- rbindlist(lapply(winspd.files, fread), idcol = "station.id") # load all station data 
mcfcd.winspd.data[, station.id := factor(station.id, labels = basename(winspd.names))] # add station names to column 'station'
mcfcd.winspd.data[, station.id := as.character(station.id)] # convert to character for matching
colnames(mcfcd.winspd.data) <- c("station.id","date","time","winspd") # add rest of column names
mcfcd.winspd.data$date.time  <- mdy_hms(paste0(mcfcd.winspd.data$date," ",mcfcd.winspd.data$time), tz = "US/Arizona") # MCFCD data is all local

# solar data
solar.files <- list.files(here(path = "data/MCFCD/2017/Solar"), pattern="txt$", full.names = T) # full file path names
solar.names <- gsub(".txt", "", list.files(here(path = "data/MCFCD/2017/Solar"), pattern = "txt$", full.names = F)) # names (stations) of files
mcfcd.solar.data <- rbindlist(lapply(solar.files, fread), idcol = "station.id") # load all station data 
mcfcd.solar.data[, station.id := factor(station.id, labels = basename(solar.names))] # add station names to column 'station'
mcfcd.solar.data[, station.id := as.character(station.id)] # convert to character for matching
colnames(mcfcd.solar.data) <- c("station.id","date","time","solar") # add rest of column names
mcfcd.solar.data$date.time  <- mdy_hms(paste0(mcfcd.solar.data$date," ",mcfcd.solar.data$time), tz = "US/Arizona") # MCFCD data is all local

# station data
mcfcd.stations <- fread(here("data/MCFCD/stations.csv")) # load station data
mcfcd.stations <- mcfcd.stations[!is.na(station.id)]  # remove stations without an ID
mcfcd.stations[, station.id := as.character(station.id)] # convert station.id to character for matching
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
mcfcd.temp.data <- merge(mcfcd.temp.data, mcfcd.stations[,.(station.name,station.id)], by = "station.id")
mcfcd.dewpt.data <- merge(mcfcd.dewpt.data, mcfcd.stations[,.(station.name,station.id)], by = "station.id")
mcfcd.windir.data <- merge(mcfcd.windir.data, mcfcd.stations[,.(station.name,station.id)], by = "station.id")
mcfcd.winspd.data <-merge(mcfcd.winspd.data, mcfcd.stations[,.(station.name,station.id)], by = "station.id")
mcfcd.solar.data <- merge(mcfcd.solar.data, mcfcd.stations[,.(station.name,station.id)], by = "station.id")

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
ibut.temp <- fread(here("data/iButton/phxbuttons.csv"))
ibut.rhum <- fread(here("data/iButton/phxbuttonsrh.csv"))

# import station info and data
ibut.stations <- fread(here("data/iButton/phxdatsite_all.csv"))
ibut.stations.col.info <- fread(here("data/iButton/phxdatsiteall_col_description.csv"))

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

# bind temp and hum toegether by station.id & date.time
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

#^#^#^#^#^#^#^#^#^#^#^#^#^#
# Merge all data together #
#^#^#^#^#^#^#^#^#^#^#^#^#^#

# combine all data into one object, and all stations into one object
w.data <- rbindlist(list(azmet.data,ncei.data,mcfcd.data,ibut.data), use.names = T, fill = T)
w.stations <- rbindlist(list(azmet.stations,ncei.stations,mcfcd.stations,ibut.stations), use.names = T, fill = T)

# calculate heat index, (National Weather Surface improved estimate of Steadman eqn. (heat index calculator eqns)
w.data[, heat.f := heat.index(t = temp.f, dp = dewpt.f, temperature.metric = "fahrenheit", output.metric = "fahrenheit", round = 0)]
w.data[, heat.c := heat.index(t = temp.c, dp = dewpt.c, temperature.metric = "celsius", output.metric = "celsius", round = 1)]

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
w.stations <- w.stations[n.temp != 0]

# save final data object
saveRDS(w.data, here("data/2017-all-data.rds"))
saveRDS(w.stations, here("data/2017-all-stations.rds"))

# reload, & check data unchanged 
#w.data2 <- readRDS(here("data", "2017-all-data.rds"))
#w.stations2 <- readRDS(here("data", "2017-all-stations.rds"))
#all.equal(w.data,w.data2)
#all.equal(w.stations,w.stations2)
