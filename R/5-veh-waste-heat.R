
# Load files
per <- read_sas("NHTS 2009/FullGeoDated/DOT/pp_dotv2.sas7bdat")   # person file
day <- read_sas("NHTS 2009/FullGeoDated/DOT/dd_dotv2.sas7bdat")   # day (trip) file
hh <- read_sas("NHTS 2009/FullGeoDated/DOT/hh_dotv2.sas7bdat")    # household file
veh <- read_sas("NHTS 2009/FullGeoDated/DOT/vv_dotv2.sas7bdat")   # vehicle file

hhct <- read_sas("NHTS 2009/FullGeoDated/CT/hhct.sas7bdat")       # household census tract file
wkct <- read_sas("NHTS 2009/FullGeoDated/CT/workct.sas7bdat")     # workplace census tract file

hhwt <- read_sas("NHTS 2009/FullGeoDated/DOT/hh50wt.sas7bdat")    # household replicate weights
perwt <- read_sas("NHTS 2009/FullGeoDated/DOT/per50wt.sas7bdat")  # person replicate weights

## R Workspace saved here with loaded data ##


# MERGE FILES TO CONSOLIDATE DATA INTO THREE META FILES (Vehicles, Persons, Trips)
veh.hh <- merge(veh, hh) # Merge vehicle & hh file
per.hh <- merge(per, hh) # Merge person & hh file
day.per.hh <- merge(day, per.hh) # Merge person-hh to trip file

vehicles <- merge(veh.hh, hhct)
persons.temp <- merge(per.hh, hhct)
persons <- merge(persons.temp, wkct)
trips.temp <- merge(day.per.hh, hhct)

rm(veh.hh,per.hh,day.per.hh,persons.temp) # remove excess variables
gc() # clear up memory for last merge (biggest)

trips <- merge(trips.temp, wkct)

rm(trips.temp)
gc()


# ERROR CALCS

# list of vehicles that had valid report of fuel consumption in Maricopa County
veh.phx <- vehicles[vehicles$HHCNTYFP == "013" & vehicles$HHSTFIPS == "04" & vehicles$GSYRGAL > 0,]

# add rep weights
veh.phx <- merge(veh.phx, hhwt)

veh.phx$yrmiles <- veh.phx$EIADMPG * veh.phx$GSYRGAL # yearly miles estimate
veh.phx$adj_mpg <- veh.phx$EPATMPG  - veh.phx$EIADMPG # adjustment for EPA rating MPG and actual estaimted MPG

# adjust the gal/yr consumption by imporving vehicle MPG for certain model years.
veh.phx.2 <- veh.phx # create new data frame for estimating adjustment
veh.phx.2$EPATMPG <- ifelse(veh.phx.2$VEHYEAR < 2000, 36.4, veh.phx.2$EPATMPG) # repalce MPG to new '16 MPG for < 2000 yr cars
veh.phx.2$EIADMPG <- veh.phx.2$EPATMPG - veh.phx.2$adj_mpg  # adjust actual on-road MPG based on previous data
veh.phx.2$GSYRGAL <- veh.phx.2$yrmiles / veh.phx.2$EIADMPG  # calc new gallon use

# adjust the gal/yr consumption by imporving vehicle MPG for certain model years.
veh.phx.3 <- veh.phx # create new data frame for estimating adjustment
veh.phx.3$EPATMPG <- 36.4 # repalce MPG to new '16 MPG for < 2000 yr cars
veh.phx.3$EIADMPG <- veh.phx.3$EPATMPG - veh.phx.3$adj_mpg  # adjust actual on-road MPG based on previous data
veh.phx.3$GSYRGAL <- veh.phx.3$yrmiles / veh.phx.3$EIADMPG  # calc new gallon use

# mean weighted est of fuel consumption in eq. gal for Maricopa County
x.1 <- (veh.phx[,c('GSYRGAL')] %*% veh.phx[,c('WTHHFIN')]) 
x.2 <- (veh.phx.2[,c('GSYRGAL')] %*% veh.phx.2[,c('WTHHFIN')]) 
x.3 <- (veh.phx.3[,c('GSYRGAL')] %*% veh.phx.3[,c('WTHHFIN')]) 

# full error = sum_i[ (REP(i)-x)^2 ] 
#    where x is sample estimate from final weight and REP(i) is the sample estimate for replicate weight i.

idx <- which(colnames(veh.phx)=="HHWGT1") # index location of first replicate weight
REP.1 <- vector(mode="numeric", length=99)
REP.2 <- vector(mode="numeric", length=99)
REP.3 <- vector(mode="numeric", length=99)
   
for(i in 1:100){
  j <- i + idx - 1 # replicate weights start at this column index
  REP.1[i] <- (((veh.phx[,c('GSYRGAL')] %*% veh.phx[,j]) - x.1)^2) # calculate inside of sumation
  REP.2[i] <- (((veh.phx.2[,c('GSYRGAL')] %*% veh.phx.2[,j]) - x.2)^2) # calculate inside of sumation
  REP.3[i] <- (((veh.phx.3[,c('GSYRGAL')] %*% veh.phx.3[,j]) - x.3)^2) # calculate inside of sumation
}

# standard error on yearly gas consumption in Maricopa County
sqrt((99/100)*sum(REP.1))
sqrt((99/100)*sum(REP.2))
sqrt((99/100)*sum(REP.3))

# total gas consupmtion in Maricopa County
paste0(x.1[1,1]," +/- ",sqrt((99/100)*sum(REP.1))," gallons gas eq in 2009")
paste0(x.2[1,1]," +/- ",sqrt((99/100)*sum(REP.2))," gallons gas eq in 2009 with all < '00 replaced with new '16")
paste0(x.3[1,1]," +/- ",sqrt((99/100)*sum(REP.3))," gallons gas eq in 2009 with all vehicles improved to 36.4 MPG")


veh.years <- as.data.frame(table(veh.phx$VEHYEAR))

nrow(veh.phx[veh.phx$VEHYEAR < 2000,]) / nrow(veh.phx)
mean(veh.phx$EIADMPG[veh.phx$VEHYEAR < 2000])

# Weighted MPG on-road
(veh.phx[,c('EIADMPG')] %*% veh.phx[,c('WTHHFIN')]) / sum(veh.phx$WTHHFIN)

# Weighted MPG label
(veh.phx[,c('EPATMPG')] %*% veh.phx[,c('WTHHFIN')]) / sum(veh.phx$WTHHFIN)



# total gas consupmtion in Maricopa County from imporvement
x.new <- (veh.phx.new[,c('GSYRGAL')] %*% veh.phx.new[,c('WTHHFIN')]) 
paste0(x.new[1,1]," +/- ",0," gallons gas eq in 2009 with new MPG")

# replace every vehilce with 2016 MPG
veh.phx.new <- veh.phx # create new data frame for estimating adjustment
veh.phx.new$EPATMPG <- 36.4
veh.phx.new$EIADMPG <- veh.phx.new$EPATMPG - veh.phx.new$adj_mpg
veh.phx.new$GSYRGAL <- veh.phx.new$yrmiles / veh.phx.new$EIADMPG
# total gas consupmtion in Maricopa County from imporvement
x.new <- (veh.phx.new[,c('GSYRGAL')] %*% veh.phx.new[,c('WTHHFIN')]) 
paste0(x.new[1,1]," +/- ",0," gallons gas eq in 2009 with new MPG")



# TRIP DISTRIBUTION OVER TIME IN PHX

# modes that we car about (car, van, truck, RV, bus, taxi, etc..)
modes <- c(1,2,3,4,5,6,7,9,10,11,12,13,14,19)

# recode mode to numeric for matching
trips$TRPTRANS <- as.numeric(trips$TRPTRANS)
trips.phx <- trips[trips$HHCNTYFP == "013" & trips$HHSTFIPS == "04" & trips$TRPTRANS %in% modes,]

# create dataframe to count every trip occurring at every minute of day
time.counts <- as.data.frame(cbind(format(seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), by = "1 min"),"%H%M", tz="GMT"), vector(mode="numeric", length=1441)))
time.counts <- time.counts[-c(1441),]
time.counts$V1 <- as.character(time.counts$V1)
time.counts$V2 <- as.numeric(time.counts$V2)
colnames(time.counts)[1] <- "time"
colnames(time.counts)[2] <- "counts"
time.counts$counts <- 0
time.counts$weighted_counts <- 0

# iterate through list of trips and determine if the trip is occuring during a specific minute of day, if so count it and weighted value
for(a in 1:nrow(time.counts)){
  for(b in 1:nrow(trips.phx)){
    if( (trips.phx$STRTTIME[b] <= time.counts$time[a]) & (time.counts$time[a] <= trips.phx$ENDTIME[b]) ){
      time.counts$counts[a] <- time.counts$counts[a] + 1  
      time.counts$weighted_counts[a] <- time.counts$weighted_counts[a] + trips.phx$WTPERFIN[b]
    }
  }
}

time.counts$time <- format(strptime(time.counts$time, format="%H%M"), format="%H:%M")
time.counts$weighted_counts_n <- time.counts$weighted_counts / sum(time.counts$weighted_counts) * sum(time.counts$counts)


(ggplot(data = time.counts, aes(x = time, y = counts)) 
  + geom_point()
  + scale_y_continuous(limits = c(0,1300)))

if( (trips.phx$STRTTIME[1] <= time.counts[770]) & (time.counts[770] <= trips.phx$ENDTIME[1]) ){"yes"}


nrow(trips.phx[as.numeric(trips.phx$STRTTIME) >= 0 & as.numeric(trips.phx$ENDTIME) >= 0,])

write.csv(time.counts,"time_counts.csv")


# check to see that all the travel mintues are counted in the time counts
sum(trips.phx$TRVL_MIN) + length(trips.phx$TRVL_MIN)
sum(time.counts$counts)

# weighted trip time
trips.phx[,c('TRVL_MIN')] %*% trips.phx[,c('WTPERFIN')] / sum(trips.phx$WTPERFIN)



# ADOT AADT - Match Maricopa sections to AADT, know truck amounts
AADT <- read.csv("2016_ADOT_AADT_raw.csv")
AADT.loc <- read.csv("ADOT_mileposts_and_sections.csv")

AADT.mag <- AADT[AADT$Loc.ID %in% AADT.loc$LOCATION.ID,]
AADT.mag$length <- AADT.mag$EMP - AADT.mag$BMP 

# Determine how much of the AADT records are given for highways
OSM.mag.roads <- read.csv("maricopa_roads_no_res.csv")
(sum(OSM.mag.roads$length_con[OSM.mag.roads$fclass == "motorway"]) * 0.000621371) - sum(AADT.mag$length[!is.na(AADT.mag$length)])  # lenght of highway in Maricopa County in Miles
# 33 miles of missing data. ~ 4.1% (33.08114/805.0911)

# Table 4-23: Average Fuel Efficiency of U.S. Light Duty Vehicles
# average light duty vehicle MPG 2009: onroad = 21.7
# average light duty vehicle MPG 2015: onroad = 22.0, new = 36.4

# Table 4-13: Single-Unit 2-Axle 6-Tire or More Truck Fuel Consumption and Travel(a)
# average Average MPGe 2009/15: onroad = 7.4/7.4, hasn't changed much in years

# Table 4-14: Combination Truck Fuel Consumption and Travel
# average Average MPGe 2009/15: onroad = 6.0/5.9, hasn't changed much in decades 

# 1,405 lane miles as of 2005

# percent lost to waste heat
p <- 0.5


AADT.mag$daily_gal <- (
  (AADT.mag$length * AADT.mag$AADT.Single.Trucks / 7.4) +  # single unit trucks gallons gas eq per day per segment
  (AADT.mag$length * AADT.mag$AADT.Combo.Trucks / 6.0) +  # combo unit trucks gallons gas eq per day per segment
  (AADT.mag$length * (AADT.mag$AADT.2016 - AADT.mag$AADT.Combo.Trucks - AADT.mag$AADT.Single.Trucks) / 21.7)) # light duty veh gas/day
    
AADT.mag$daily_gal_trucks <- (
  (AADT.mag$length * AADT.mag$AADT.Single.Trucks / 7.4) +  # single unit trucks gallons gas eq per day per segment
    (AADT.mag$length * AADT.mag$AADT.Combo.Trucks / 6.0))  # combo unit trucks gallons gas eq per day per segment

sum(AADT.mag$daily_gal_trucks[!is.na(AADT.mag$daily_gal_trucks)]) * 365 # total yearly gallons frieght from AADT/ADOT

AADT.mag$daily_Wh <- AADT.mag$daily_gal * p * 33.41 * 1000  # gallons/day * effiency * kWh/gal * Wh/kWh
AADT.mag$watts <- AADT.mag$daily_Wh / 24
AADT.mag$heatflux <- AADT.mag$watts / (AADT.mag$length * 1609.34 * 60)  # watts per sq meter; 1609.34 meters per mile, 60 meter (200ft width of highways)


AADT.mag.agg <- aggregate(cbind(watts,length,AADT.2016) ~ Route, data = AADT.mag, FUN = sum)
AADT.mag.agg$heatflux <- AADT.mag.agg$watts / (AADT.mag.agg$length * 1609.34 * 60) 

# length of I 17 in Maricopa
(sum(OSM.mag.roads$length_con[OSM.mag.roads$ref == "I 17"]) * 0.000621371)

write.csv(AADT.mag,"aadt_mag_all_maricopa_adot.csv")
