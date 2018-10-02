# install missing packages
list.of.packages <- c("data.table","tidyverse","lubridate","weathermetrics","here")  # a list of the dependant packages  
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load fonts
extrafont::loadfonts(device = "win") # load fonts