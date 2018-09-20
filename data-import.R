# import and format 2016 Phoenix, AZ weather data from various sources

library(data.table)
library(tidyverse)
library(lubridate)
library(here)

#-#-#
# import AZMET DATA
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
rm(encanto,desert.ridge,mesa,greenway)

# assign column names (see data dictionary)
colnames(azmet.data) <- c("year","day.year","hour","temp","rh","vap","sol","precip","soil.4.in","soil.20.in","wind.spd","wind.mag","wind.dir","wind.dir.dev","max.wind.spd","ETo","act.vap","dew","station")

# add date column
azmet.data$date <- as.Date(azmet.data$day.year-1, origin = paste0(azmet.data$year,"-01-01"))

# Format date + time in new column as YYYY-MM-DD HH:MM:SS
azmet.data$date.time  <- ymd_hm(paste0(azmet.data$date," ",azmet.data$hour,":00"), tz = "US/Arizona")

# fix temp = 999 to NA
azmet.data$temp <- ifelse(azmet.data$temp == 999, NA, azmet.data$temp)

# read AZMET station data
azmet.stations <- fread(here(path = "data/AZMET/stations.csv"))

#~#~#
# load NCEI data
ncei.data <- read_fwf(here(path = "data/NCEI/2016_data.txt"), skip = 1,
                       fwf_empty(here(path = "data/NCEI/2016_data.txt"),
                                 col_names = c("USAF","WBAN","DATE","DIR","SPD","GUS","CLG","SKC","L","M","H","VSB","MW1","MW2","MW3","MW4","AW1","AW2","AW3","AW4","W","TEMP","DEWP","SLP","ALT","STP","MAX","MIN","PCP01","PCP06","PCP24","PCPXX","SD")))

ncei.stations <- read_fwf(here(path = "data/NCEI/stations.txt"), skip = 2, fwf_widths(c(7,6,31,51,31,9,9,10), c("USAF","WBAN","NAME","COUNTRY","STATE","LAT","LON","ELEV")))

# Convert TEMP and DEWP to numeric 
ncei.data$TEMP <- as.numeric(as.character(ncei.data$TEMP))
ncei.data$DEWP <- as.numeric(as.character(ncei.data$DEWP))

# Remove NAs in TEMP and DEWP
#ncei.data <- ncei.data[(!is.na(ncei.data$TEMP) & !is.na(ncei.data$DEWP)),]

# Format date + time in new column as YYYY-MM-DD HH:MM:SS
ncei.data$DateTime  <- ISOdatetime(format(strptime(ncei.data$DATE, format="%Y%m%d%H%M"), format="%Y"),  # Year
                            format(strptime(ncei.data$DATE, format="%Y%m%d%H%M"), format="%m"),  # Month
                            format(strptime(ncei.data$DATE, format="%Y%m%d%H%M"), format="%d"),  # Day
                            format(strptime(ncei.data$DATE, format="%Y%m%d%H%M"), format="%H"),  # Hour
                            format(strptime(ncei.data$DATE, format="%Y%m%d%H%M"), format="%M"),  # Min
                            "00",                                                         # Sec (=":00")
                            tz = "UTC") # NCEI data is all in GMT/UTC (see data dictionary)
# Timezone
ncei.data$DateTime <- lubridate::with_tz(ncei.data$DateTime, tz = "US/Arizona") # change timezone to local (AZ)

# Create Year-Month column
ncei.data$YrMth <- format(strptime(ncei.data$DATE, format="%Y%m%d%H%M"), format="%Y-%m")  # add year-month column

# Keep relevant columns only
ncei.data <- ncei.data[,c("USAF","WBAN","DateTime","TEMP","DEWP","TEMPC","DEWPC","YrMth","SPD")]

#*#*#
# import Maricopa County Flood Control District data

# temp data
temp.files <- list.files(here(path = "data/MCFCD/2017/Temp"), pattern="txt$", full.names = T) # full file path names
temp.names <- gsub(".txt", "", list.files(here(path = "data/MCFCD/2017/Temp"), pattern = "txt$", full.names = F)) # names (stations) of files
mcfcd.temp.data <- rbindlist(lapply(temp.files, fread), idcol = "station.id") # load all station data 
mcfcd.temp.data[, station.id := factor(station.id, labels = basename(temp.names))] # add station names to column 'station'
mcfcd.temp.data[, station.id := as.character(station.id)] # convert to character for matching
colnames(mcfcd.temp.data) <- c("station.id","date","time","temp") # add rest of column names
mcfcd.temp.data$date.time  <- mdy_hms(paste0(mcfcd.temp.data$date," ",mcfcd.temp.data$time), tz = "US/Arizona") # MCFCD data is all local

# dewpt data
dewpt.files <- list.files(here(path = "data/MCFCD/2017/Dewpoint"), pattern="txt$", full.names = T) # full file path names
dewpt.names <- gsub(".txt", "", list.files(here(path = "data/MCFCD/2017/Dewpoint"), pattern = "txt$", full.names = F)) # names (stations) of files
mcfcd.dewpt.data <- rbindlist(lapply(dewpt.files, fread), idcol = "station.id") # load all station data 
mcfcd.dewpt.data[, station.id := factor(station.id, labels = basename(dewpt.names))] # add station names to column 'station'
mcfcd.dewpt.data[, station.id := as.character(station.id)] # convert to character for matching
colnames(mcfcd.dewpt.data) <- c("station.id","date","time","dewpt") # add rest of column names
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
mcfcd.stations <- fread(here(path = "data/MCFCD/stations.csv")) # load station data
mcfcd.stations <- mcfcd.stations[!is.na(station.id)]  # remove stations without an ID
mcfcd.stations[, station.id := as.character(station.id)] # convert station.id to character for matching

# in temp data, stations 1001 and 28301 are missing (both outside urbanized area, so not essential to replace)
# in temp data, station 1002 is extra (removed, station is humidity data w/ 0 < x > 100)
# in dewpt data, stations 30816 and 34916 are extra (removed, no stations w/ those IDs in index, data is consistent with dewpt data tho)

{# check for missing stations of data
  #all.temp.stations <- mcfcd.stations$station.id[mcfcd.stations$sensor.type == "Temperature"]
  #all.dewpt.stations <- mcfcd.stations$station.id[mcfcd.stations$sensor.type == "Dewpoint"]
  #all.windir.stations <- mcfcd.stations$station.id[mcfcd.stations$sensor.type == "Wind Dir."]
  #all.winspd.stations <- mcfcd.stations$station.id[mcfcd.stations$sensor.type == "Peak Wind"]
  #all.solar.stations <- mcfcd.stations$station.id[mcfcd.stations$sensor.type == "Solar Rad."]
  
  # missing stations for each data type
  #all.temp.stations[which(!all.temp.stations %in% temp.names)] 
  #all.dewpt.stations[which(!all.dewpt.stations %in% dewpt.names)] 
  #all.windir.stations[which(!all.windir.stations %in% windir.names)] 
  #all.winspd.stations[which(!all.winspd.stations %in% winspd.names)] 
  #all.solar.stations[which(!all.solar.stations %in% solar.names)] 
  
  # extraneous stations for each data type
  #temp.names[which(!temp.names %in% all.temp.stations)]
  #dewpt.names[which(!dewpt.names %in% all.dewpt.stations)]
  #windir.names[which(!windir.names %in% all.windir.stations)]
  #winspd.names[which(!winspd.names %in% all.winspd.stations)]
  #solar.names[which(!solar.names %in%all.solar.stations)]
  } 

# bind station names to each data type, then full join all data together by station.name and date.time 
#(note: solar rad data is only every 30 min instead of other four at 15 min)



rm(temp.names,temp.files) 

# Format date + time in new column as YYYY-MM-DD HH:MM:SS
mcfcd.data$date.time  <- mdy_hms(paste0(mcfcd.data$date," ",mcfcd.data$time), tz = "US/Arizona") # MCFCD data is all local

# convert station deg-min-sec to decimal lat/long
dms2dec <- function(x){
  z <- sapply(strsplit(x, " "), as.numeric)
  z[1, ] + z[2, ]/60 + z[3, ]/3600
}
mcfcd.stations$lat <- dms2dec(mcfcd.stations$lat.dms)
mcfcd.stations$lon <- dms2dec(mcfcd.stations$lon.dms)
mcfcd.stations$lon <- (-1) * mcfcd.stations$lon # arizona is west of prime meridian so longitude should be negative

mcfcd.data2 <- merge(mcfcd.data, mcfcd.stations, by = "station.id")

table(mcfcd.data2$sensor.type)

# combine all data into one object
# columns: datasource, station.id, station.name, temp.f, temp.c. ap.temp.f, ap.temp.c, 

class(mcfcd.data$station.id)
class(mcfcd.stations$station.id)


# save final data objec, reload, & check data unchanged 
#saveRDS(azmet.data, here("data/AZMET", "all_binded.rds"))
#azmet.data2 <- readRDS(here("data/AZMET", "all_binded.rds"))
#all.equal(azmet.data,azmet.data2)

# Add celsuis in TEMP and DEWP
#data$TEMPC <- (ncei.data$TEMP - 32) / 1.8
#data$DEWPC <- (ncei.data$DEWP - 32) / 1.8

# Caluclate Apparent Temperature using Steadman eqn. Assume no wind correction factor (for now).
#data$AT <- -2.653 + (0.994 * ncei.data$TEMPC) + (0.0153 *  ncei.data$DEWPC * ncei.data$DEWPC)  