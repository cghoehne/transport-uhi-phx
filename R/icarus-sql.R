#############################################################
## ACCESS ICARUS SQL DATABASE TO PULL VEHICLE TRAVEL DATA  ##
#############################################################

# ** It is recommended to use Microsoft Open R (v3.5.1) for improved performance without compromsing compatibility **

## SCRIPT PREPERATION

# clear space and allocate memory
gc()
memory.limit(size = 50000) 
script.start <- Sys.time() # start script timestamp

# first make sure checkpoint is installed locally
# this is the only package that is ok to not use a 'checkpointed' (i.e. archived version of a package)
# checkpoint does not archive itself and it should not create dependency issues
if (!require("checkpoint")){
  install.packages("checkpoint")
  library(checkpoint, quietly = T)
}

# load all other dependant packages from the local repo
.libPaths(paste0(getwd(),"/.checkpoint/2019-01-01/lib/x86_64-w64-mingw32/3.5.1"))
library(DBI)
library(RMariaDB)
#library(RMySQL)
#library(odbc)
library(data.table)
library(here)

# archive/update snapshot of packages at checkpoint date
checkpoint("2019-01-01", # Sys.Date() - 1  this calls the MRAN snapshot from yestersday
           R.version = "3.5.1", # will only work if using the same version of R
           checkpointLocation = here(), # calls here package
           verbose = F) 

my.server <- as.character(fread(here("data/icarus/server.txt"), header = F)[1])
my.pass <- as.character(fread(here("data/icarus/server.txt"), header = F)[2])

con <- dbConnect(MariaDB(), 
                 dbname = "icarus",
                 host = my.server,
                 username = "amichne",
                 password = my.pass)


