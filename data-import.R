library(here)
library(data.table)

# import AZMET DATA
encanto <- do.call(rbind, lapply(list.files(here(path = "data/AZMET/Encanto/raw"),recursive = T, pattern="txt$", full.names= T), read.csv, header = F))
desert.ridge <- do.call(rbind, lapply(list.files(here(path = "data/AZMET/Desert Ridge/raw"), recursive = T, pattern="txt$", full.names= T), read.csv, header = F))
mesa <- do.call(rbind, lapply(list.files(here(path = "data/AZMET/Mesa/raw"), recursive = T, pattern="txt$", full.names= T), read.csv, header = F))
phx.greenway <- do.call(rbind, lapply(list.files(here(path = "data/AZMET/Phoenix Greenway/raw"), recursive = T, pattern="txt$", full.names= T), read.csv, header = F))

# add station ID to each dataset
encanto$station <- "Encanto"
desert.ridge$station <- "Desert Ridge"
mesa$station <- "Mesa"
phx.greenway$station <- "Phoenix Greenway"

# bind all together
azmet.phx <- rbind(encanto,desert.ridge,mesa,phx.greenway)
rm(encanto,desert.ridge,mesa,phx.greenway)

# assign column names (see data dictionary)
colnames(azmet.phx) <- c("year","day.year","hour","temp","rh","vap","sol","precip","soil.4.in","soil.20.in","wind.spd","wind.mag","wind.dir","wind.dir.dev","max.wind.spd","ETo","act.vap","dew","station")

# add date column
azmet.phx <- as.data.table(azmet.phx)
azmet.phx$date <- as.Date(azmet.phx$day.year-1, origin = paste0(azmet.phx$year,"-01-01"))
azmet.stations <- unique(azmet.phx$station)

# fix temp = 999 to NA
azmet.phx$temp <- ifelse(azmet.phx$temp == 999, NA, azmet.phx$temp)

# save AZMENT data object
saveRDS(azmet.phx, here("data/AZMET", "all_binded.rds"))

# reload
azmet.phx2 <- readRDS(here("data/AZMET", "all_binded.rds"))

# check data equal
all.equal(azmet.phx,azmet.phx2)
