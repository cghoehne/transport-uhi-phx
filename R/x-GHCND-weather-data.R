#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#
# Retrieve Global Historical Climatology Network daily (GHCND) weather data #
#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#

# list of all dependant packages
list.of.packages <- c("lubridate",
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

# note this is the data set is not hourly, it is only daily summaries (e.g. min/max temps)

# get global list of all ghcnd stations
if(file_test("-f", here("data/outputs/temp/ghcnd-station-data.rds")) == T){ # if the file has been previously retrieved
  ghcnd.stations <- readRDS(here("data/outputs/temp/ghcnd-station-data.rds")) # load previous retervial to save time
} else {
  ghcnd.stations <- as.data.table(ghcnd_stations()) # otherwise retrieve raw station data
  saveRDS(ghcnd.stations, here("data/outputs/temp/ghcnd-station-data.rds")) # and save
}

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

# convert elevation into ft
my.stations$phx$elevation <- my.stations$phx$elevation * 3.28084
my.stations$la$elevation <- my.stations$la$elevation * 3.28084


saveRDS(my.stations, here("data/outputs/temp/2017-ghcnd-weather-stations.rds")) # daily summary (non-hourly) weather data from ghcnd
saveRDS(phx.ghcnd.data, here("data/outputs/temp/2017-ghcnd-weather-data.rds")) # daily summary (non-hourly) weather data from ghcnd



