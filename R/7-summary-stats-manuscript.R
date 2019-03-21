# PAPER SUMMARY STATS




# WEATHER
#import data
valid.dates <- readRDS(here("data/aster/my-aster-data.rds")) # remote sensed temps at valiation sites on specified dates
my.years <- unique(valid.dates[, year(date.time)]) # store all unique years to reterive weather data for those years
weather.raw <- rbindlist(lapply(here(paste0("data/mesowest/", my.years, "-meso-weather-data.rds")), readRDS), fill = T) # bind all weather data for the selected years

colnames(weather.raw)
weather.raw[, mean(winspd, na.rm = T), by = hour(date.time)][order(hour)]
weather.raw[, quantile(winspd, probs = 0.5, na.rm = T), by = hour(date.time)][order(hour)]
weather.raw[, quantile(winspd, probs = 0.5, na.rm = T)]

weather.raw[, mean(temp.c, na.rm = T), by = hour(date.time)][order(hour)]


# PAVEMENT SURFACE
# import data
folder <- paste0(here("data/outputs"),"/run_metadata_20190320_192039/")
all.surface.data <- readRDS(paste0(folder, "all_pave_surface_data.rds"))
colnames(all.surface.data)

all.surface.data[, daytime := factor(ifelse(hour(date.time) %in% c(7:18), "Day", "Night"), levels = c("Day","Night"))]

# flux comparisons by type
all.surface.data[, mean(q.rad + q.cnv), by = c("pave.name", "batch.name", "albedo")][order(V1)]

# convection
all.surface.data[, quantile(q.cnv, probs = 0.5, na.rm = T), by = c("pave.name", "batch.name", "albedo")][order(V1)]
all.surface.data[, max(q.cnv, na.rm = T), by = c("pave.name", "batch.name", "albedo")][order(V1)]

all.surface.data[, max(q.cnv, na.rm = T), by = c("daytime")][order(V1)]
all.surface.data[, min(q.cnv, na.rm = T), by = c("daytime")][order(V1)]

# IR
all.surface.data[, max(q.rad, na.rm = T), by = c("daytime")][order(V1)]
all.surface.data[, min(q.rad, na.rm = T), by = c("daytime")][order(V1)]

# which max out.flux
all.surface.data[,max(q.rad), by = c("pave.name", "batch.name", "albedo")][order(V1)]
