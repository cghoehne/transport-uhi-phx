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
ncei.data <- ncei.data[,c("USAF","WBAN","DateTime","TEMP","DEWP","TEMPC","DEWPC","AT","YrMth","SPD")]

#*#*#
# import Maricopa County Flood Control District data
files <- list.files(here(path = "data/MCFCD/2017/Temp"), pattern="txt$", full.names = T) # full file path names
names <- gsub(".txt", "", list.files(here(path = "data/MCFCD/2017/Temp"), pattern = "txt$", full.names = F)) # names (stations) of files
mcfcd.data <- rbindlist(lapply(files, fread), idcol = "station.id") # load all station data 
mcfcd.data[, station.id := factor(station.id, labels = basename(names))] # add station names to column 'station'
colnames(mcfcd.data) <- c("station","date","time","temp") # add rest of column names
rm(names,files) 
mcfcd.stations <- fread(here(path = "data/MCFCD/stations.csv")) # load station data
mcfcd.stations <- mcfcd.stations[!is.na(station.id)]

# Format date + time in new column as YYYY-MM-DD HH:MM:SS
mcfcd.data$DateTime  <- ISOdatetime(format(strptime(mcfcd.data$date, format="%m/%d/%Y"), format="%Y"),  # Year
                                    format(strptime(mcfcd.data$date, format="%m/%d/%Y"), format="%m"),  # Month
                                    format(strptime(mcfcd.data$date, format="%m/%d/%Y"), format="%d"),  # Day
                                    format(strptime(mcfcd.data$time, format="%H:%M:%S"), format="%H"),  # Hour
                                    format(strptime(mcfcd.data$time, format="%H:%M:%S"), format="%M"),  # Hour
                                    format(strptime(mcfcd.data$time, format="%H:%M:%S"), format="%S"),  # Sec
                                    tz = "US/Arizona") # MCFCD data is all local)



# convert station deg-min-sec to decimal lat/long
dms2dec <- function(x){
  z <- sapply(strsplit(x, " "), as.numeric)
  z[1, ] + z[2, ]/60 + z[3, ]/3600
}
mcfcd.stations$lat <- dms2dec(mcfcd.stations$lat.dms)
mcfcd.stations$lon <- dms2dec(mcfcd.stations$lon.dms)
mcfcd.stations$lon <- (-1) * mcfcd.stations$lon # arizona is west of prime meridian so longitude should be negative

mcfcd.data2 <- merge(mcfcd.data, mcfcd.stations, by = "station.id")

# combine all data into one object
# columns: datasource, station.id, station.name, temp.f, temp.c. ap.temp.f, ap.temp.c, 



# save final data objec, reload, & check data unchanged 
#saveRDS(azmet.data, here("data/AZMET", "all_binded.rds"))
#azmet.data2 <- readRDS(here("data/AZMET", "all_binded.rds"))
#all.equal(azmet.data,azmet.data2)

# Add celsuis in TEMP and DEWP
#data$TEMPC <- (ncei.data$TEMP - 32) / 1.8
#data$DEWPC <- (ncei.data$DEWP - 32) / 1.8

# Caluclate Apparent Temperature using Steadman eqn. Assume no wind correction factor (for now).
#data$AT <- -2.653 + (0.994 * ncei.data$TEMPC) + (0.0153 *  ncei.data$DEWPC * ncei.data$DEWPC)  