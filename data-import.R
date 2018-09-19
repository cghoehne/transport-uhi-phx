# import and format 2016 Phoenix, AZ weather data from various sources

library(data.table)
library(lubridate)
library(here)

#-#-#
# import AZMET DATA
encanto <- do.call(rbind, lapply(list.files(here(path = "data/AZMET/Encanto/raw"),recursive = T, pattern="txt$", full.names= T), read.csv, header = F))
desert.ridge <- do.call(rbind, lapply(list.files(here(path = "data/AZMET/Desert Ridge/raw"), recursive = T, pattern="txt$", full.names= T), read.csv, header = F))
mesa <- do.call(rbind, lapply(list.files(here(path = "data/AZMET/Mesa/raw"), recursive = T, pattern="txt$", full.names= T), read.csv, header = F))
greenway <- do.call(rbind, lapply(list.files(here(path = "data/AZMET/Greenway/raw"), recursive = T, pattern="txt$", full.names= T), read.csv, header = F))

# add station ID to each dataset
encanto$station <- "Encanto"
desert.ridge$station <- "Desert Ridge"
mesa$station <- "Mesa"
greenway$station <- "Greenway"

# bind all together, remove individual dfs
azmet.phx <- rbind(encanto,desert.ridge,mesa,greenway)
rm(encanto,desert.ridge,mesa,greenway)

# assign column names (see data dictionary)
colnames(azmet.phx) <- c("year","day.year","hour","temp","rh","vap","sol","precip","soil.4.in","soil.20.in","wind.spd","wind.mag","wind.dir","wind.dir.dev","max.wind.spd","ETo","act.vap","dew","station")

# add date column
azmet.phx <- as.data.table(azmet.phx)
azmet.phx$date <- as.Date(azmet.phx$day.year-1, origin = paste0(azmet.phx$year,"-01-01"))
azmet.stations <- unique(azmet.phx$station)

# fix temp = 999 to NA
azmet.phx$temp <- ifelse(azmet.phx$temp == 999, NA, azmet.phx$temp)

#~#~#
# load NCEI data
ncei.phx <- read.fwf(here(path = "data/NCEI/2016_data.txt"), skip = 1,
               widths=(c(6,-1,5,-1,12,-1,3,-1,3,-1,3,-1,3,-1,3,-1,1,-1,1,-1,1,-1,4,-1,2,-1,2,-1,2,-1,2,-1,2,-1,2,-1,2,-1,2,-1,1,-1,4,-1,4,-1,6,-1,5,-1,6,-1,3,-1,3,-1,5,-1,5,-1,5,-1,5,-1,2)))

# assign column names (see data dictionary for details)
colnames(ncei.phx) <- c("USAF","WBAN","DATE","DIR","SPD","GUS","CLG","SKC","L","M","H","VSB","MW1","MW2","MW3","MW4","AW1","AW2","AW3","AW4","W","TEMP","DEWP","SLP","ALT","STP","MAX","MIN","PCP01","PCP06","PCP24","PCPXX","SD")

# Convert TEMP and DEWP to numeric 
ncei.phx$TEMP <- as.numeric(as.character(ncei.phx$TEMP))
ncei.phx$DEWP <- as.numeric(as.character(ncei.phx$DEWP))

# Remove NAs in TEMP and DEWP
ncei.phx <- ncei.phx[(!is.na(ncei.phx$TEMP) & !is.na(ncei.phx$DEWP)),]

# Add celsuis in TEMP and DEWP
ncei.phx$TEMPC <- (ncei.phx$TEMP - 32) / 1.8
ncei.phx$DEWPC <- (ncei.phx$DEWP - 32) / 1.8

# Caluclate Apparent Temperature using Steadman eqn. Assume no wind correction factor (for now).
ncei.phx$AT <- -2.653 + (0.994 * ncei.phx$TEMPC) + (0.0153 *  ncei.phx$DEWPC * ncei.phx$DEWPC)  

# Format date + time in new column as YYYY-MM-DD HH:MM:SS
ncei.phx$DateTime  <- ISOdatetime(format(strptime(ncei.phx$DATE, format="%Y%m%d%H%M"), format="%Y"),  # Year
                            format(strptime(ncei.phx$DATE, format="%Y%m%d%H%M"), format="%m"),  # Month
                            format(strptime(ncei.phx$DATE, format="%Y%m%d%H%M"), format="%d"),  # Day
                            format(strptime(ncei.phx$DATE, format="%Y%m%d%H%M"), format="%H"),  # Hour
                            format(strptime(ncei.phx$DATE, format="%Y%m%d%H%M"), format="%M"),  # Min
                            "00",                                                         # Sec (=":00")
                            tz = "UTC") # NCEI data is all in GMT/UTC (see data dictionary)
# Timezone
ncei.phx$DateTime <- with_tz(ncei.phx$DateTime, tz = "US/Arizona") # change timezone to local (AZ)

# Create Year-Month column
ncei.phx$YrMth <- format(strptime(ncei.phx$DATE, format="%Y%m%d%H%M"), format="%Y-%m")  # add year-month column

# Keep relevant columns only
ncei.phx <- ncei.phx[,c("USAF","WBAN","DateTime","TEMP","DEWP","TEMPC","DEWPC","AT","YrMth","SPD")]

#*#*#
# import Maricopa County Flood Control District data

#temp <- do.call(rbind, lapply(list.files(here(path = "data/MCFCD/2017/Temp"), recursive = T, pattern="txt$", full.names= T), read.csv, header = F))

# test read in one file
read.fwf(here(path = "data/MCFCD/2017/Temp/1002.txt"), widths=(c(10,-1,8,-9,2)))


# save final data objec, reload, & check data unchanged 
#saveRDS(azmet.phx, here("data/AZMET", "all_binded.rds"))
#azmet.phx2 <- readRDS(here("data/AZMET", "all_binded.rds"))
#all.equal(azmet.phx,azmet.phx2)