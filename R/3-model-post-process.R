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
library(zoo, lib.loc = lib.path, quietly = T)
library(lubridate, lib.loc = lib.path, quietly = T)
library(ggplot2, lib.loc = lib.path, quietly = T)
library(data.table, lib.loc = lib.path, quietly = T)
library(here, lib.loc = lib.path, quietly = T)

# archive/update snapshot of packages at checkpoint date
checkpoint("2019-01-01", # archive date for all used packages (besides checkpoint itself!)
           R.version = "3.5.1", # will only work if using the same version of R
           checkpointLocation = here(), # calls here package
           verbose = F) 

# load windows fonts and store as string
windowsFonts(Century=windowsFont("TT Century Gothic"))
windowsFonts(Times=windowsFont("TT Times New Roman"))
my.font <- "Century"

# load model simulation metadata
model.runs <- fread(here("data/outputs/1D-heat-model-runs/model_runs_metadata.csv")) # 20190121/20190121_

# load weather data
#weather <- readRDS(here("data/outputs/temp/sample-weather-data.rds")) # sample 3 day period of weather data
#weather <- readRDS(here("data/outputs/temp/2017-weather-data.rds")) # all weather data from cleaning script
weather.raw <- rbindlist(list(readRDS(here("data/outputs/2017-weather-data-1.rds")), # all weather data saved to repo
                          readRDS(here("data/outputs/2017-weather-data-2.rds")),
                          readRDS(here("data/outputs/2017-weather-data-3.rds"))))
                          
# filter weather data
weather.raw <- weather.raw[!is.na(solar) & !is.na(temp.c) & !is.na(dewpt.c) ] #& !is.na(winspd),] # make sure only to select obs with no NA of desired vars
weather.raw[is.na(winspd), winspd := 0] # set NA windspeeds to zero becuase this is the most likely to be missing weather variable
weather.raw <- weather.raw[station.name == "City of Glendale" & source == "MCFCD",] # choose station 


# layer profile data
layer.profiles <- readRDS(here("data/outputs/1D-heat-model-runs/layer_profiles.rds"))

# loop through loading simulated pavement temperature data for run 
# and summaring/ploting as necessary
should.plot <- "yes" # "yes" or "no"

for(run in 1:max(model.runs$run.n)){
  tryCatch({  # catch and print errors, avoids stopping model run 
  
  # read simulation data
  pave.time <- readRDS(here(paste0("data/outputs/1D-heat-model-runs/run_",run,"_output.rds"))) # 20190121/
  
  # trim weather data to number of days specified
  my.date <- as.POSIXct(model.runs$start.day[run])
  weather <- weather.raw[date.time >= my.date & # date.time starts at start.day 
                           date.time <= (my.date + days(model.runs$n.days[run]))] # ends n days later
  
  # record avg max temp and final day max temp at surface
  n.days <- model.runs$n.days[run]

  model.runs[run.n == run, T.degC_Avg_Day_Max_0m := pave.time[node == 0, max(T.degC, na.rm = T), by = as.Date(date.time)][, mean(V1, na.rm = T)]]
  model.runs[run.n == run, T.degC_Fnl_Day_Max_0m := pave.time[node == 0, max(T.degC, na.rm = T), by = as.Date(date.time)][.N - 1, V1]]
  
  # record Range and IQR for key depths
  model.runs[run.n == run, T.degC_Range_L1 := max(pave.time[layer == "surface", T.degC], na.rm = T) - min(pave.time[, T.degC], na.rm = T)]
  model.runs[run.n == run, T.degC_IQR_L1 := IQR(pave.time[layer == "surface", T.degC], na.rm = T)]
  model.runs[run.n == run, T.degC_Range_L2 := max(pave.time[layer == "base", T.degC], na.rm = T) - min(pave.time[, T.degC], na.rm = T)]
  model.runs[run.n == run, T.degC_IQR_L2 := IQR(pave.time[layer == "base", T.degC], na.rm = T)]
  model.runs[run.n == run, T.degC_Range_L3 := max(pave.time[layer == "subgrade", T.degC], na.rm = T) - min(pave.time[, T.degC], na.rm = T)]
  model.runs[run.n == run, T.degC_IQR_L3 := IQR(pave.time[layer == "subgrade", T.degC], na.rm = T)]
  
  # custom ASTER temp validation
  if(run == 1){valids <- pave.time[0]} # create empty data.frame to bind to # exists("pave.time") == F
  valid.all <- pave.time[which(abs(difftime(pave.time[,date.time],"2017-08-22 22:35:36")) == 
                                 min(abs(difftime(pave.time[,date.time],"2017-08-22 22:35:36"))))]
  valids <- rbind(valids,valid.all[node == 0], fill = T) # should be between 30 C (bare ground) and 40 C (asphalt))
  valids[run, run.n := run]
  
  which.min(difftime(pave.time$date.time, as.POSIXct("2017-08-22 22:35:36")))
  
  if(should.plot == "yes"){
    ## replicate Gui et al. Fig. 2 
    # depth temp
    my.node <- 0 #quantile(pave.time$node, probs = .9)
    p1.data <- pave.time[node == my.node]
    min.x <- min(p1.data$date.time, na.rm = T)
    max.x <- ceiling_date(max(p1.data[!is.na(T.degC), date.time]), unit = "hours")
    #min.y <- round(signif(min(p1.data[, T.degC]) - (0.1 * diff(range(p1.data[, T.degC]))),4), -1)
    min.y <- 0 #round(signif(min(weather[, temp.c]) - (0.1 * diff(range(weather[, temp.c]))),4), -1)
    max.y <- round(signif(max(weather[, solar / 10]) + (0.1 * diff(range(p1.data[, T.degC]))),4), -1)
    
    p1 <- (ggplot(data = p1.data) 
           + geom_segment(aes(x = min.x, y = min.y, xend = max.x, yend = min.y))   # x border (x,y) (xend,yend)
           + geom_segment(aes(x = min.x, y = min.y, xend = min.x, yend = max.y))  # y border (x,y) (xend,yend)
           + geom_point(aes(y = T.degC, x = date.time)) #date.time
           + geom_point(aes(y = T.degC, x = date.time), data = p1.data[date.time %in% weather$date.time], color = "gray")
           + geom_line(data = weather, aes(y = temp.c, x = date.time), color = "blue")
           + geom_point(data = weather, aes(y = temp.c, x = date.time), color = "blue", shape = 4)
           + geom_line(data = weather, aes(y = solar/10, x = date.time), color = "red")
           + geom_point(data = weather, aes(y = solar/10, x = date.time), color = "red", shape = 2)
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
    dir.create(here("figures/1D-heat-model-runs/"), showWarnings = FALSE) # creates output folder if it doesn't already exist
    ggsave(paste0("run_",model.runs$run.n[run],"_1D-modeled-", round(p1.data[, depth.m][1]*1000,0),"mm-pave-temp.png"), p1, 
           device = "png", path = here("figures/1D-heat-model-runs"), scale = 1, width = 6.5, height = 5, dpi = 300, units = "in") # /20190121
    
    # Deviation (boxplot) of single scenario by nodes
    min.x <- min(p1.data$date.time, na.rm = T)
    max.x <- ceiling_date(max(p1.data[!is.na(T.degC), date.time]), unit = "hours")
    min.y <- round(signif(min(pave.time[, T.degC]) - (0.1 * diff(range(pave.time[, T.degC]))),4), -1)
    max.y <- round(signif(max(pave.time[, T.degC]) + (0.1 * diff(range(pave.time[, T.degC]))),4), -1)
    
    # only will show first three layers with the third layer partially shown if very deep
    boundary.nodes <- pave.time[layer == "boundary" & time.s == 0, node] # to mark the boundaries
    max.depth <- (sum(layer.profiles[[model.runs$layer.profile[run]]]$thickness[1:2])) * 3 #pave.time[, max(depth.m)]
    layers <- nrow(layer.profiles[[model.runs$layer.profile[run]]])
    p.node.box <- (ggplot(data = pave.time[depth.m < max.depth],
                          aes(x = depth.m, y = T.degC, group = factor(depth.m)))
                   + geom_boxplot(outlier.alpha = 0.1, outlier.size = 0.7)
                   + geom_vline(xintercept = unique(pave.time[node %in% boundary.nodes & depth.m != 0, depth.m]), linetype = "dotted")
                   + labs(x = "Pavement Depth (m)", y = "Temperature (deg C)")
                   + theme_light()
                   + theme(text = element_text(family = my.font, size = 12),
                           plot.margin = margin(t = 10, r = 20, b = 10, l = 40, unit = "pt"),
                           axis.title.x = element_text(vjust = -2),
                           axis.title.y = element_text(vjust = 5))
    )
    for(l in 1:layers){ # add text annotating each layer
      an.x <- ifelse(l == max(layers), # if last layer, different label x position
                     max.depth - (0.5 * (max.depth - sum(layer.profiles[[model.runs$layer.profile[run]]]$thickness[1:(l-1)]))),
                     sum(layer.profiles[[model.runs$layer.profile[run]]]$thickness[1:l],-0.5*layer.profiles[[model.runs$layer.profile[run]]]$thickness[l]))
      
      p.node.box <- p.node.box + annotate("text", label = layer.profiles[[model.runs$layer.profile[run]]]$layer[l],
                                          x = an.x,
                                          y = 0.9 * max.y, family = my.font)
    }
    #+ annotate("text", label = "Layer 2: \nBase", x = model.runs$L1.depth[run] + (model.runs$L2.depth[run] * 0.5), y =  0.9 * max.y, family = my.font)
    #+ annotate("text", label = "Layer 3: \nSubbase", x = max.depth - (max.depth - model.runs$L1.depth[run] - model.runs$L2.depth[run])/2, y = 0.9 * max.y, family = my.font)
    
    # save plot
    ggsave(paste0("run_",model.runs$run.n[run],"_1D-modeled-pave-temp-box_0-0.3m.png"), p.node.box, 
           device = "png", path = here("figures/1D-heat-model-runs"), scale = 1, width = 6.5, height = 5, dpi = 300, units = "in") # /20190121
    
  } # end optional plotting
  }, error = function(e){cat("ERROR:",conditionMessage(e), "\n")}) # print error message if model run had error
}

write.csv(model.runs, here("data/outputs/1D-heat-model-runs/model_runs_metadata_stats.csv"), row.names = F) # output model run metadata



# check pavement temperature quantiles at depths
depths <- c(0,0.05,0.1,0.2,0.45)
for(depth in depths){
  print(paste0("At ",depth, " meters, T.degC quantiles are:"))
  print(pave.time[depth.m == depth, quantile(T.degC, probs = c(0,0.1,0.25,0.5,0.75,0.9,1))])
  cat("\n") # Enter
}


# min mean max by depth & date
pave.time[, .(min.T = min(T.degC, na.rm = T),
                 avg.T = mean(T.degC, na.rm = T),
                 max.T = max(T.degC, na.rm = T)), by = depth.m]

pave.time[, .(min.T = min(T.degC, na.rm = T),
              avg.T = mean(T.degC, na.rm = T),
              max.T = max(T.degC, na.rm = T)), by = as.Date(date.time)]
              

################
# BACKUP PLOTS #
################

# melt all temp max into data.table for easier plotting
#model.runs.tmax <- melt(model.runs[, .SD, .SDcols = c("run.n",paste0("Tmax",1:n.days))] , id.vars = "run.n")
#model.runs.tmin <- melt(model.runs[, .SD, .SDcols = c("run.n",paste0("Tmin",1:n.days))] , id.vars = "run.n")
#model.runs.tmax <- merge(model.runs.tmax, model.runs[, .(run.n, avg.i.T, pave.depth)], by = "run.n")
#model.runs.tmin <- merge(model.runs.tmin, model.runs[, .(run.n, avg.i.T, pave.depth)], by = "run.n")

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
       #+ geom_point(aes(y = T.degC, x = date.time), data = p0.data[date.time %in% weather$date.time], color = "red")
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
min.x <- 30 #signif(min(pave.time[, T.degC]) - (0.1 * diff(range(pave.time[, T.degC]))),4)
max.x <- 90 #signif(max(pave.time[, T.degC]) + (0.1 * diff(range(pave.time[, T.degC]))),4)
min.y <- 0 # 0 depth
max.y <- max(pave.time[, depth.m])

my.date <- date(max(pave.time$date.time) - days(1))

p.depth <- (ggplot(data = pave.time) # weather
            + geom_segment(aes(x = min.x, y = min.y, xend = max.x, yend = min.y))   # x border (x,y) (xend,yend)
            + geom_segment(aes(x = min.x, y = min.y, xend = min.x, yend = max.y))  # y border (x,y) (xend,yend)
            + geom_line(aes(y = depth.m, x = T.degC, color = "9am"), data = pave.time[date.time == paste(my.date,"9:00:00")], size = 1.5) # 9am temperatures at depth
            + geom_line(aes(y = depth.m, x = T.degC, color = "1pm"), data = pave.time[date.time == paste(my.date,"13:00:00")], size = 1.5) # 1pm temperatures at depth
            + geom_line(aes(y = depth.m, x = T.degC, color = "5pm"), data = pave.time[date.time == paste(my.date, "17:00:00")], size = 1.5) # 5pm temperatures at depth
            + geom_line(aes(y = depth.m, x = T.degC, color = "9pm"), data = pave.time[date.time == paste(my.date, "21:00:00")], size = 1.5) # 9pm temperatures at depth
            #+ geom_point(aes(y = depth.m, x = T.degC), data = pave.time[date.time == paste(my.date,"13:00:00")], name = "1pm", shape = 1) # 1pm temperatures at depth
            #+ geom_point(aes(y = depth.m, x = T.degC), data = pave.time[date.time == paste(my.date, "17:00:00")], name = "5pm", shape = 2) # 5pm temperatures at depth
            #+ geom_point(aes(y = depth.m, x = T.degC), data = pave.time[date.time == paste(my.date, "21:00:00")], name = "9pm", shape = 3) # 9pm temperatures at depth
            + geom_hline(yintercept = unique(pave.time[node %in% boundary.nodes & depth.m != 0, depth.m]), linetype = "dotted")
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


p1 <- (ggplot(data = weather) 
       + geom_segment(aes(x = min.x, y = min.y, xend = max.x, yend = min.y))   # x border (x,y) (xend,yend)
       + geom_segment(aes(x = min.x, y = min.y, xend = min.x, yend = max.y))  # y border (x,y) (xend,yend)
       + geom_line(aes(y = solar/10, x = date.time), color = "red")
       #+ geom_point( aes(y = solar, x = date.time), color = "red", shape = 2)
       + scale_x_datetime(expand = c(0,0), limits = c(min.x,max.x), date_breaks = "3 hours")
       + scale_y_continuous(expand = c(0,0), limits = c(min.y,max.y), breaks = seq(min.y,max.y,10))
       + ggtitle(paste0("Modeled ", p1.data[, depth.m][1]*1000,"mm Pavement Temperature"))
       + labs(x = "Time of Day", y = "Temperature (deg C)") # "Time of Day"
       + theme_minimal()
       + theme(text = element_text(family = my.font, size = 12),
               plot.margin = margin(t = 10, r = 20, b = 10, l = 40, unit = "pt"),
               axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
               axis.title.x = element_text(vjust = 0.3),
               plot.title = element_text(hjust = 0.75)))

p1
