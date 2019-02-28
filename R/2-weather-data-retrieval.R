## MESO WEST DATA RETREIVAL VIA API

# clear space and allocate memory
gc()
memory.limit(size = 56000) 
t.start <- Sys.time() # start script timestamp

# first make sure checkpoint is installed locally
# this is the only package that is ok to not use a 'checkpointed' (i.e. archived version of a package)
# checkpoint does not archive itself and it should not create dependency issues
if (!require("checkpoint")){
  install.packages("checkpoint")
  library(checkpoint, quietly = T)
}

# load all other dependant packages from the local repo
lib.path <- paste0(getwd(),"/.checkpoint/2019-01-01/lib/x86_64-w64-mingw32/3.5.1")
library(httr, lib.loc = lib.path, quietly = T)
library(jsonlite, lib.loc = lib.path, quietly = T)
library(lubridate, lib.loc = lib.path, quietly = T)
library(data.table, lib.loc = lib.path, quietly = T)
library(here, lib.loc = lib.path, quietly = T)

# archive/update snapshot of packages at checkpoint date
checkpoint("2019-01-01", # archive date for all used packages (besides checkpoint itself!)
           R.version = "3.5.1", # will only work if using the same version of R
           checkpointLocation = here(), # calls here package
           verbose = F) 

# station metadata link w/o token
s.link <- "http://api.mesowest.net/v2/stations/metadata?state=AZ&county=Maricopa&status=ACTIVE&token="

# load personal api token (note you'll need your own, local-token.txt is git ignored)
token <- as.character(fread(here("local-token.txt"), header = F)[1])

# define time period you want to pull, max in one loop should be a 
# single year of all stations to avoid giant files
# times range of ASTER data I used:  "2000-04-12 11:30:40 MST"  to   "2019-01-02 11:21:40 MST"
years <- 2000:2018

# store partial weather data link strings (front half and variables list). time is local
w.link.f <- "http://api.mesowest.net/v2/stations/timeseries?&stid="
w.link.vars <- "&vars=air_temp,dew_point_temperature,solar_radiation,wind_speed,wind_direction&obtimezone=local"

# loop over each year and store separate .rds of each year of data for all stations
# and a station metadata .rds file (list of all station info)
for(year in years){
  
  # queary for a list of all stations' metadata (as JSON)
  meso.metadata <- fromJSON(paste0(s.link,token))
  meso.s.data <- as.data.table(meso.metadata$STATION) # make station data.table
  
  # define local start and end times as formated strings for API query
  # to change format, include in earlier link:  timeformat=%b%20%d%20%Y%20-%20%H:%M   
  # yielding format: "Jun 22 2017 - 17:06"
  start <- paste0(year,"01010000") # start time in YYYYMMDDhhmm
  end <- paste0(year + 1, "01010000")   # end time in YYYYMMDDhhmm
  
  # loop through each station and retrieve json data
  meso.w.data <- list()
  for(i in 1:nrow(meso.s.data)){ #
    
    # retieve json data for station 'i' based on start and end time, custom api token, and desired variables
    j <- fromJSON(paste0(w.link.f,meso.s.data$STID[i],"&start=",start,"&end=",end,"&token=",token,w.link.vars))
    
    # if there is data at the location, get it
    if(length(j$STATION) != 0){
      
      # coerce to data.table in list (need to unlist columns)
      meso.w.data[[i]] <- as.data.table(sapply(j$STATION$OBSERVATIONS, unlist))
      
      # if vars exists, rename it for consistency w/ other data
      if(is.null(meso.w.data[[i]]$date_time) == F){setnames(meso.w.data[[i]], "date_time", "date.time")}
      if(is.null(meso.w.data[[i]]$dew_point_temperature_set_1d) == F){setnames(meso.w.data[[i]], "dew_point_temperature_set_1d", "dewpt.c")}
      if(is.null(meso.w.data[[i]]$wind_speed_set_1) == F){setnames(meso.w.data[[i]], "wind_speed_set_1", "winspd")}
      if(is.null(meso.w.data[[i]]$wind_direction_set_1) == F){setnames(meso.w.data[[i]], "wind_direction_set_1", "windir")}
      if(is.null(meso.w.data[[i]]$air_temp_set_1) == F){setnames(meso.w.data[[i]], "air_temp_set_1", "temp.c")}
      if(is.null(meso.w.data[[i]]$solar_radiation_set_1) == F){setnames(meso.w.data[[i]], "solar_radiation_set_1", "solar")}
      
      # remove extra dewpoint column if it appears
      if(is.null(meso.w.data[[i]]$dew_point_temperature_set_1) == F){meso.w.data[[i]]$dew_point_temperature_set_1 <- NULL}
      
      # create station column name and list index name based on the station id (STID) for refrencing
      names(meso.w.data)[i] <- j$STATION$STID
      meso.w.data[[i]]$id <- j$STATION$STID
      meso.w.data[[i]]$station.name <- j$STATION$NAME
      meso.w.data[[i]]$qcflag <- j$STATION$QC_FLAGGED
    }
  }
  
  # create combined data option (all data in one data.table). this is more compact 
  all.meso.w.data <- rbindlist(meso.w.data, use.names = T, fill = T)
  
  # format date.time correctly (time is local so call AZ timezone). ignore "T" (for: time) in middle and -0700 at end (for: GMT/UTC +0700)
  all.meso.w.data[, date.time := ymd_hms(paste(substr(date.time, 1, 10), substr(date.time, 12, 19)), tz = "US/Arizona")]
  
  # force data columns to numeric
  all.meso.w.data[, dewpt.c := as.numeric(dewpt.c)][, winspd := as.numeric(winspd)][, windir := as.numeric(windir)][, temp.c := as.numeric(temp.c)][, solar := as.numeric(solar)]
  #all.meso.w.data[,2:6] <- lapply(all.meso.w.data[,2:6], as.numeric) # alt
  
  # create some vars
  all.meso.w.data[, temp.f := signif((temp.c * 1.8) + 32, digits = 4)]
  
  # rename columns for consistency
  setnames(meso.s.data, "NAME", "station.name")
  setnames(meso.s.data, "STID", "id") # could use "ID" column here instead
  setnames(meso.s.data, "ELEVATION", "elevation")
  setnames(meso.s.data, "LONGITUDE", "lon")
  setnames(meso.s.data, "LATITUDE", "lat")
  
  # keep relevant columns only in station data
  meso.s.data <- meso.s.data[, .(station.name,id,elevation,lat,lon)]
  
  # coerce lat, lon, & elev to numeric
  meso.s.data[, lat := as.numeric(lat)][, lon := as.numeric(lon)][, elevation := round(as.numeric(elevation), digits = 0)]
  
  # for id == NA, make them the station.name
  meso.s.data[is.na(id), id := station.name]
  all.meso.w.data[is.na(id), id := station.name]
  
  # remove all obs with NA temps because they won't really be useable (and if temp is missing, usually other vars are too)
  all.meso.w.data <- all.meso.w.data[!is.na(temp.c),]
  
  # save final R objects
  dir.create(here("data/mesowest"), showWarnings = FALSE) # creates output folder if it doesn't already exist
  saveRDS(all.meso.w.data, here(paste0("data/mesowest/", year,"-meso-weather-data.rds"))) # all houlry weather data
  saveRDS(meso.s.data, here(paste0("data/mesowest/", year,"-meso-station-data.rds"))) # all station data
  
}

# add some other variables 
#all.meso.w.data[, month := as.factor(lubridate::month(date.time, label = T))]
#all.meso.w.data[, week := as.factor(lubridate::week(date.time))]
#all.meso.w.data[, day := as.factor(lubridate::day(date.time))]
#all.meso.w.data[, wday := as.factor(lubridate::wday(date.time, label = T))]
#all.meso.w.data[, hour := lubridate::hour(date.time)]

# create rounded data.time to more easily filter by observations on/near the hour
#all.meso.w.data[, date.time.round := as.POSIXct(round.POSIXt(date.time, "hours"))]
