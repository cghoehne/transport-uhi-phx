library(sirad, lib.loc = lib.path, quietly = T)

### ADD CUSTOM SOLAR RADIATION BASED ON LAT AND JULIAN DAY
#stations.raw <- rbindlist(lapply(here(paste0("data/mesowest/", my.years, "-meso-station-data.rds")), readRDS))
#weather.raw <- merge(weather.raw, unique(stations.raw[,.(station.name,lat)]), by = "station.name", all.x = T, allow.cartesian = T) # merge latitude from station data for solar rad calc

# find times to extract solar radiation since it is missing from MesoWest right now
#test.yrs <- valid.dates[, date(date.time)]

# calculate solar radiation (convert from MJ/m2/h to W/m2)
#extrat(test[1, yday(date.time)], test[1, radians(lat)])$ExtraTerrestrialSolarRadiationHourly * 277.777778

#sol.rad <- weather.old[date(date.time) %in% test.yrs, ]
#weather.raw <- merge(weather.raw,)
#####