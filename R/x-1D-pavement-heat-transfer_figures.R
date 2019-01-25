# clear space and allocate memory
gc()
memory.limit(size = 56000) 
t.start <- Sys.time() # start script timestamp

# in case we need to revert or packages are missing
#if (!require("here")) install.packages("here")
#if (!require("checkpoint")) install.packages("checkpoint")

# load checkpoint package to insure you call local package dependcies
library(checkpoint)
#checkpoint("2019-01-17") # static checkpoint

# archive/update snapshot of packages at checkpoint date
checkpoint("2019-01-01", # Sys.Date() - 1  this calls the MRAN snapshot from yestersday
           R.version = "3.5.1", # will only work if using the same version of R
           checkpointLocation = here::here(), # calls here package
           verbose = T) 
#checkpointRemove(Sys.Date() - 1, allUntilSnapshot = TRUE, here::here()) # this removes all previous checkpoints before today

# load dependant packages
library(ggplot2)
library(zoo)
library(lubridate)
library(data.table)
library(here)

# load windows fonts and store as string
windowsFonts(Century=windowsFont("TT Century Gothic"))
windowsFonts(Times=windowsFont("TT Times New Roman"))
my.font <- "Century"


# read in sample weather data 
weather <- readRDS(here("data/outputs/temp/sample-weather-data.rds")) # 3 day period of weather data at 15 min intervals
#weather <- readRDS(here("data/outputs/temp/2017-weather-data.rds"))
#weather <- weather[station.name == "City of Glendale" & source == "MCFCD" & month == "Jun" & # choose station for desired period of time
#                     !is.na(solar) & !is.na(temp.c) & !is.na(dewpt.c) & !is.na(windir),] # make sure only to select obs with no NA of desired vars
#weather <- weather[date.time <= (min(date.time) + days(12))] # trim to 12 days long

# read in model simulation metadata
model.runs <- fread(here("data/outputs/1D-heat-model-runs/20190121/20190121_model_runs_metadata.csv")) # 
runs <- model.runs[run.n == 1 | RMSE != 0, run.n] # store all runs that are archived

# create summary variable for max temp by day for each day in sample weather data
n.days <- ceiling(difftime(max(weather$date.time),min(weather$date.time), units = "days"))
model.runs[, paste0(rep(paste0("Tmax"),n.days),1:n.days) := 0]
model.runs[, paste0(rep(paste0("Tmin"),n.days),1:n.days) := 0] 

# loop through loading simulated pavement temperature data for run 
# and summaring/ploting as necessary
should.plot <- "no" # "yes" or "no"

for(run in runs){
  tryCatch({  # catch and print errors, avoids stopping model run 
  
  # read simulation data
  pave.time <- readRDS(here(paste0("data/outputs/1D-heat-model-runs/20190121/run_",run,"_output.rds"))) # 20190121/
  
  # record max temp at surface for every day
  for(d in 1:n.days){
    model.runs[run.n == run ,eval(paste0("Tmax",d)) := max(pave.time[node == 0 & as.Date(date.time) == as.Date(min(date.time) + days(d-1)), T.degC])]
    model.runs[run.n == run ,eval(paste0("Tmin",d)) := min(pave.time[node == 0 & as.Date(date.time) == as.Date(min(date.time) + days(d-1)), T.degC])]
  }
  
  # record Range and IQR for key depths
  model.runs[run.n == run, T.degC_Range_L1 := max(pave.time[layer == "surface", T.degC], na.rm = T) - min(pave.time[, T.degC], na.rm = T)]
  model.runs[run.n == run, T.degC_IQR_L1 := IQR(pave.time[layer == "surface", T.degC], na.rm = T)]
  model.runs[run.n == run, T.degC_Range_L2 := max(pave.time[layer == "base", T.degC], na.rm = T) - min(pave.time[, T.degC], na.rm = T)]
  model.runs[run.n == run, T.degC_IQR_L2 := IQR(pave.time[layer == "base", T.degC], na.rm = T)]
  model.runs[run.n == run, T.degC_Range_L3 := max(pave.time[layer == "subgrade", T.degC], na.rm = T) - min(pave.time[, T.degC], na.rm = T)]
  model.runs[run.n == run, T.degC_IQR_L3 := IQR(pave.time[layer == "subgrade", T.degC], na.rm = T)]
  
  if(should.plot == "yes"){
    ## replicate Gui et al. Fig. 2 
    # depth temp
    my.node <- quantile(pave.time$node, probs = .9)
    p1.data <- pave.time[node == my.node]
    min.x <- min(p1.data$date.time, na.rm = T)
    max.x <- ceiling_date(max(weather$date.time, na.rm = T), unit = "hours")
    min.y <- 20 #signif(min(p1.data[, T.degC]) - (0.1 * diff(range(p1.data[, T.degC]))),4)
    max.y <- 80 #ceiling(signif(max(p1.data[, T.degC]) + (0.1 * diff(range(p1.data[, T.degC]))),4))
    
    p1 <- (ggplot(data = p1.data) # weather
           + geom_segment(aes(x = min.x, y = min.y, xend = max.x, yend = min.y))   # x border (x,y) (xend,yend)
           + geom_segment(aes(x = min.x, y = min.y, xend = min.x, yend = max.y))  # y border (x,y) (xend,yend)
           + geom_point(aes(y = T.degC, x = date.time)) #date.time
           + geom_point(aes(y = T.degC, x = date.time), data = p1.data[date.time %in% weather$date.time], color = "red")
           #+ geom_point(aes(y = T.sky - 273.15, x = time.s), data = weather, color = "blue") # points of sky temp
           #+ geom_line(aes(y = T.degC, x = time.s), color = "grey50", linetype = 2, size = 0.75) # line of temp
           + scale_x_datetime(expand = c(0,0), limits = c(min.x,max.x), date_breaks = "2 days")
           + scale_y_continuous(expand = c(0,0), limits = c(min.y,max.y), breaks = seq(min.y,max.y,5))
           + ggtitle(paste0("Modeled ", p1.data[, depth.m][1]*1000,"mm Pavement Temperature"))
           + labs(x = "Time of Day", y = "Temperature (deg C)") # "Time of Day"
           + theme_minimal()
           + theme(text = element_text(family = my.font, size = 12),
                   plot.margin = margin(t = 10, r = 20, b = 10, l = 40, unit = "pt"),
                   axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
                   axis.title.x = element_text(vjust = 0.3),
                   plot.title = element_text(hjust = 0.75)))
    
    # save plot
    ggsave(paste0("run_",model.runs$run.n[run],"_1D-modeled-", round(p1.data[, depth.m][1]*1000,0),"mm-pave-temp.png"), p1, 
           device = "png", path = here("figures/1D-heat-model-runs"), scale = 1, width = 6.5, height = 5, dpi = 300, units = "in") # /20190121
    
    # Deviation (boxplot) of single scenario by nodes
    p.node.box <- (ggplot(data = pave.time[depth.m < 0.3],
                          aes(x = depth.m, y = T.degC, group = factor(depth.m)))
                   + geom_boxplot()
                   + geom_vline(xintercept = unique(pave.time[layer == "boundary" & depth.m != 0, depth.m]), linetype = "dotted")
                   + annotate("text", label = "Layer 1: Surface", x = 0.05, y = 67.5)
                   + annotate("text", label = "Layer 2: Base", x = 0.15, y = 67.5)
                   + annotate("text", label = "Layer 3: Subbase", x = 0.25, y = 67.5)
                   + labs(x = "Pavement Depth (m)", y = "Temperature (deg C)")
                   + theme_light()
                   + theme(text = element_text(family = my.font, size = 12),
                           plot.margin = margin(t = 10, r = 20, b = 10, l = 40, unit = "pt"),
                           axis.title.x = element_text(vjust = -2),
                           axis.title.y = element_text(vjust = 5))
    )
    
    # save plot
    ggsave(paste0("run_",model.runs$run.n[run],"_1D-modeled-pave-temp-box_0-0.3m.png"), p.node.box, 
           device = "png", path = here("figures/1D-heat-model-runs"), scale = 1, width = 6.5, height = 5, dpi = 300, units = "in") # /20190121
    
  } # end optional plotting
  }, error = function(e){cat("ERROR:",conditionMessage(e), "\n")}) # print error message if model run had error
}



# check pavement temperature quantiles at depths
depths <- c(0,0.05,0.1,0.2,0.45)
for(depth in depths){
  print(paste0("At ",depth, " meters, T.degC quantiles are:"))
  print(pave.time[depth.m == depth, quantile(T.degC, probs = c(0,0.1,0.25,0.5,0.75,0.9,1))])
  cat("\n") # Enter
}


# tmax surface diff with mean starting temp
model.runs[, paste0(rep(paste0("Tmax.dif"),n.days),1:n.days) := 0] 
model.runs[, avg.i.T := (i.top.temp + i.bot.temp)/2]

# melt all temp max into data.table for easier plotting
model.runs.tmax <- melt(model.runs[, .SD, .SDcols = c("run.n",paste0("Tmax",1:n.days))] , id.vars = "run.n")
model.runs.tmin <- melt(model.runs[, .SD, .SDcols = c("run.n",paste0("Tmin",1:n.days))] , id.vars = "run.n")
model.runs.tmax <- merge(model.runs.tmax, model.runs[, .(run.n, avg.i.T, pave.depth)], by = "run.n")
model.runs.tmin <- merge(model.runs.tmin, model.runs[, .(run.n, avg.i.T, pave.depth)], by = "run.n")

p.all <- (ggplot(data = model.runs.tmax, aes(x = variable, y = value, col = factor(avg.i.T), shape = factor(pave.depth)))
          + geom_point()
          + labs(x = "Day", y = "Max Simulated Surface Temperature", col = "Mean \nInitialized \nPavement \nTemperature \n(deg C)", shape = "Pavement \nDepth \n(m)")
          + scale_color_manual(values=c('red','blue'))
          + theme_minimal()
          + theme(text = element_text(family = my.font, size = 12),
                  plot.margin = margin(t = 10, r = 20, b = 10, l = 40, unit = "pt"),
                  axis.title.x = element_text(vjust = 0.3))
          )
p.all


################
# BACKUP PLOTS #
################

# SURFACE TEMP PLOT

# for including air temps, instead use:
#min.y <- pmin(signif(min(p0.data[, T.degC], na.rm = T) - (0.1 * diff(range(p0.data[, T.degC], na.rm = T))),4), min(weather$temp.c), na.rm = T) 
#max.y <- signif(max(p0.data[, T.degC], na.rm = T) + (0.1 * diff(range(p0.data[, T.degC], na.rm = T))),4) 

p0.data <- pave.time[node == 0]
min.x <- min(p0.data$date.time, na.rm = T)
max.x <- ceiling_date(max(weather$date.time, na.rm = T), unit = "hours")
min.y <- signif(min(p0.data[, T.degC], na.rm = T) - (0.1 * diff(range(p0.data[, T.degC], na.rm = T))),4)
max.y <- ceiling(signif(max(p0.data[, T.degC], na.rm = T) + (0.1 * diff(range(p0.data[, T.degC], na.rm = T))),4))

p0 <- (ggplot(data = p0.data) 
       + geom_segment(aes(x = min.x, y = min.y, xend = max.x, yend = min.y))   # x border: (x,y) (xend,yend)
       + geom_segment(aes(x = min.x, y = min.y, xend = min.x, yend = max.y))  # y border: (x,y) (xend,yend)
       + geom_point(aes(y = T.degC, x = date.time))
       + geom_point(aes(y = T.degC, x = date.time), data = p0.data[date.time %in% weather$date.time], color = "red")
       #+ geom_point(aes(y = temp.c, x = date.time), data = weather, color = "blue") # for inlcuding air temp on plot
       + scale_x_datetime(expand = c(0,0), limits = c(min.x,max.x), date_breaks = "6 hours")
       + scale_y_continuous(expand = c(0,0), limits = c(min.y,max.y))
       + ggtitle("Modeled Surface Pavement Temperature")
       + labs(x = "Time of Day", y = "Temperature (deg C)") # "Time of Day"
       + theme_minimal()
       + theme(text = element_text(family = my.font, size = 12),
               plot.margin = margin(t = 10, r = 20, b = 10, l = 40, unit = "pt"),
               axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
               axis.title.x = element_text(vjust = 0.3),
               plot.title = element_text(hjust = 0.75)))

p0
ggsave("1D-modeled-surface-pave-temp_new.png", p0, 
       device = "png", path = here("figures"), scale = 1, width = 6.5, height = 5, dpi = 300, units = "in")



# depth temps
min.x <- 20 #signif(min(pave.time[, T.degC]) - (0.1 * diff(range(pave.time[, T.degC]))),4)
max.x <- 80 #signif(max(pave.time[, T.degC]) + (0.1 * diff(range(pave.time[, T.degC]))),4)
min.y <- 0 # 0 depth
max.y <- max(pave.time[, depth.m])

my.date <- date(max(pave.time$date.time))

p.depth <- (ggplot(data = pave.time) # weather
            + geom_segment(aes(x = min.x, y = min.y, xend = max.x, yend = min.y))   # x border (x,y) (xend,yend)
            + geom_segment(aes(x = min.x, y = min.y, xend = min.x, yend = max.y))  # y border (x,y) (xend,yend)
            + geom_line(aes(y = depth.m, x = T.degC, color = "1pm"), data = pave.time[date.time == paste(my.date,"13:00:00")], size = 1.5) # 1pm temperatures at depth
            + geom_line(aes(y = depth.m, x = T.degC, color = "5pm"), data = pave.time[date.time == paste(my.date, "17:00:00")], size = 1.5) # 5pm temperatures at depth
            #+ geom_line(aes(y = depth.m, x = T.degC, color = "9pm"), data = pave.time[date.time == paste(my.date, "21:00:00")], size = 1.5) # 9pm temperatures at depth
            #+ geom_point(aes(y = depth.m, x = T.degC), data = pave.time[date.time == paste(my.date,"13:00:00")], name = "1pm", shape = 1) # 1pm temperatures at depth
            #+ geom_point(aes(y = depth.m, x = T.degC), data = pave.time[date.time == paste(my.date, "17:00:00")], name = "5pm", shape = 2) # 5pm temperatures at depth
            #+ geom_point(aes(y = depth.m, x = T.degC), data = pave.time[date.time == paste(my.date, "21:00:00")], name = "9pm", shape = 3) # 9pm temperatures at depth
            + geom_hline(yintercept = p.data[layer == "boundary" & x != 0, x], linetype = "dotted")
            #+ scale_x_continuous(expand = c(0,0), limits = c(min.x,max.x))
            + scale_x_continuous(expand = c(0,0), limits = c(min.x,max.x), position = "top")
            + scale_y_reverse(expand = c(0,0), limits = c(max.y,min.y))
            + ggtitle(paste0("Modeled Pavement Temperature at Depth"))
            + labs(x = "Temperature (deg C)", y = "Depth (m)") # "Time of Day"
            + theme_minimal()
            + theme(text = element_text(family = my.font, size = 12),
                    plot.margin = margin(t = 10, r = 20, b = 10, l = 10, unit = "pt"),
                    axis.title.x = element_text(vjust = 0.3)))
p.depth