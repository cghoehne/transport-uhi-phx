################################################################
## PLOT STATIONS LOCATIONS for 2017 PHOENIX, AZ WEATHER DATA ##
##############################################################

library(data.table)
library(tidyverse)
library(lubridate)
library(raster)
library(tmap)
library(here)


#-#-#-#-#-#-#-#
# Import data #
#-#-#-#-#-#-#-#

# cleaned weather station data
w.data <- readRDS(here("data/2016-all-data.rds"))
w.stations <- readRDS(here("data/2016-all-stations.rds"))

# various shapefiles for plotting
# load city labels shpfile
phx.labels <- shapefile(here("data/shapefiles/phx_metro_labels.shp"))

# load UZA border
uza.border <- shapefile("Projects/Maricopa Parking/Minimum Parking estimates/20180409_final_results/Maricopa_urbanized_area.shp")

# load 2017 travel ways
hways <- shapefile("Projects/Maricopa Parking/Minimum Parking estimates/20180507_final_results_post_valid/20180905_phx_metro_clipped_hways3.shp")
lrt <- shapefile("Projects/Maricopa Parking/Minimum Parking estimates/20180411_final_results_w_urbanized/20180411_valley_metro_lrt_route.shp")