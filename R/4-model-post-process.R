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

# validation dates
valid.dates <- readRDS(here("data/best-aster-dates.rds"))

# load weather data
my.years <- unique(valid.dates[, year(date.time)])
weather.raw <- rbindlist(lapply(here(paste0("data/mesowest/", my.years, "-meso-weather-data.rds")), readRDS))
weather.raw <- weather.raw[!is.na(solar) & !is.na(temp.c) & !is.na(dewpt.c) ] #& !is.na(winspd),] # make sure only to select obs with no NA of desired vars
weather.raw[is.na(winspd), winspd := 0] # set NA windspeeds to zero becuase this is the most likely to be missing weather variable
weather.raw <- weather.raw[station.name == "City of Glendale",] # choose station for desired period of time

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
  my.date <- date(model.runs$end.day[run])
  weather <- weather.raw[date.time >= (my.date - days(model.runs$n.days[run])) & # date.time ends on day of end.day 
                           date(date.time) <= my.date] # ends n days later
  
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
  
  # minute and second variable to filter for weather obs
  pave.time[, mins := minute(date.time)][, secs := second(date.time)] 
  
  # air temp in deg c
  pave.time[, air.temp.c := T.inf - 273.15]
  
  # custom ASTER temp validation
  if(run == 1){valids <- pave.time[0]} # create empty data.frame to bind to # exists("pave.time") == F
  for(i in 1:nrow(valid.dates)){
    my.date <- valid.dates$date.time[i]
    year(my.date) <- 2017 ###** TEMP FIX
    valid.all <- pave.time[which(abs(difftime(pave.time[,date.time], my.date)) == 
                                   min(abs(difftime(pave.time[,date.time], my.date))))]
    valids <- rbind(valids,valid.all[node == 0], fill = T) # should be between 30 C (bare ground) and 40 C (asphalt))
    valids[run, run.n := run]
    
  }


  if(should.plot == "yes"){
    
    # SURFACE TEMPS PLOT
    # specify plot info 
    p1.data <- pave.time[node == 0]
    min.x <- min(p1.data$date.time, na.rm = T)
    max.x <- ceiling_date(max(p1.data[!is.na(T.degC), date.time]), unit = "hours")
    min.y <- 0 # solar rad is always 0 at night
    max.y <- round(signif(max(p1.data[, solar / 10]) + (0.1 * diff(range(p1.data[, T.degC]))),4), -1)
    max.y <- round(max(p1.data[, solar/10], na.rm = T), - 1) + 5
    surf.col <- c("Modeled Pavement \nSurface Temperature" = "#0D1B1E", "Observed Air Temperature" = "#10316B", "Observed Solar Radiation" = "#BF1C3D")
    surf.shp <- c("Modeled Pavement \nSurface Temperature" = 32, "Observed Air Temperature" = 4, "Observed Solar Radiation" = 2)
    surf.siz <- c("Modeled Pavement \nSurface Temperature" = 1.25, "Observed Air Temperature" = 0.75, "Observed Solar Radiation" = 0.75)
      
    p.surf <- (ggplot(data = p1.data) 
               
           # custom border
           + geom_segment(aes(x = min.x, y = min.y, xend = max.x, yend = min.y))   # x border (x,y) (xend,yend)
           + geom_segment(aes(x = min.x, y = min.y, xend = min.x, yend = max.y))  # y border (x,y) (xend,yend)
           
           # Observederved air temperature
           + geom_line(aes(y = air.temp.c, x = date.time, color = "Observed Air Temperature", size = "Observed Air Temperature"))
           + geom_point(aes(y = air.temp.c, x = date.time, color = "Observed Air Temperature", shape = "Observed Air Temperature"), data = p1.data[mins %in% c(0,30) & secs == 0,])
           
           # Observederved solar radiation
           + geom_line(aes(y = solar/10, x = date.time, color = "Observed Solar Radiation", size = "Observed Solar Radiation"))
           + geom_point(aes(y = solar/10, x = date.time, color = "Observed Solar Radiation", shape = "Observed Solar Radiation"), data = p1.data[mins %in% c(0,30) & secs == 0,])
           
           # modeled pavement surface temperature
           + geom_line(aes(y = T.degC, x = date.time, color = "Modeled Pavement \nSurface Temperature", size = "Modeled Pavement \nSurface Temperature"))
           + geom_point(aes(y = T.degC, x = date.time, color = "Modeled Pavement \nSurface Temperature", shape = "Modeled Pavement \nSurface Temperature"))
           
           # plot/axis titles & second axis for solar rad units
           #+ ggtitle("Modeled Surface Pavement Temperature")
           + labs(x = "Time of Day", y = "Temperature (deg C)")
           + scale_color_manual(name = "", values = surf.col, labels = c("Modeled Pavement \nSurface Temperature", "Observed Air Temperature", "Observed Solar Radiation"))
           + scale_shape_manual(name = "", values = surf.shp, labels = c("Modeled Pavement \nSurface Temperature", "Observed Air Temperature", "Observed Solar Radiation"))
           + scale_size_manual(name = "", values = surf.siz, labels = c("Modeled Pavement \nSurface Temperature", "Observed Air Temperature", "Observed Solar Radiation"))
           
           # scales
           + scale_x_datetime(expand = c(0,0), limits = c(min.x,max.x), date_breaks = "6 hours")
           + scale_y_continuous(expand = c(0,0), limits = c(min.y,max.y), breaks = seq(min.y,max.y,20),
                                sec.axis = sec_axis(~.*10, breaks = seq(min.y*10, max.y*10, 200), name = bquote('Solar Rad ('*W/m^2*')'))) # solar radiation axis
          
           # theme and formatting
           + theme_minimal()
           + theme(text = element_text(family = my.font, size = 12),
                   plot.margin = margin(t = 10, r = 20, b = 10, l = 40, unit = "pt"),
                   plot.title = element_text(hjust = 0.75),
                   axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
                   axis.title.x = element_text(vjust = 0.3),
                   legend.position = c(.8,.93),
                   legend.background = element_blank())
           )
    
    # save plot
    dir.create(here("figures/1D-heat-model-runs/"), showWarnings = FALSE) # creates output folder if it doesn't already exist
    ggsave(paste0("run_",model.runs$run.n[run],"_1D-modeled-surface-temp.png"), p.surf, 
           device = "png", path = here("figures/1D-heat-model-runs"), scale = 1, width = 8, height = 6, dpi = 300, units = "in") # /20190121
    
    # DEPTH TEMPS PLOT
    # specify plot info
    my.node <- c(0,4,8)
    p1.data <- pave.time[node %in% my.node]
    min.x <- min(p1.data$date.time, na.rm = T)
    max.x <- ceiling_date(max(p1.data[!is.na(T.degC), date.time]), unit = "hours")
    min.y <- round(min(weather[, temp.c]), - 1) - 5
    max.y <- round(signif(max(p1.data[, T.degC], na.rm = T) + (0.1 * diff(range(p1.data[, T.degC], na.rm = T))),4), -1)
    
    # create different legend charateristics for plotting
    depth.names <- factor(paste(unique(signif(p1.data$depth.m, 2) * 1000), "mm"), ordered = T)
    depth.col <- c("#67000D", "#D42020", "#FC7050")
    #depth.col <- c("#220901","#621708","#941B0C", "#BC3908", "#F6AA1C")
    depth.shp <- c(0:2)
    depth.siz <- c(0.6, 0.6, 0.6)

    p.depth <- (ggplot(data = p1.data) 
               
               # custom border
               + geom_segment(aes(x = min.x, y = min.y, xend = max.x, yend = min.y))   # x border (x,y) (xend,yend)
               + geom_segment(aes(x = min.x, y = min.y, xend = min.x, yend = max.y))  # y border (x,y) (xend,yend)
               
               # fifth depth
               #+ geom_line(aes(y = T.degC, x = date.time, color = depth.names[5], size = depth.names[5]), data = p1.data[node == my.node[5]])
               #+ geom_point(aes(y = T.degC, x = date.time, color = depth.names[5], shape = depth.names[5]), data = p1.data[node == my.node[5] & mins %in% c(0,30) & secs == 0,])
               
               # fourth depth
               #+ geom_line(aes(y = T.degC, x = date.time, color = depth.names[4], size = depth.names[4]), data = p1.data[node == my.node[4]])
               #+ geom_point(aes(y = T.degC, x = date.time, color = depth.names[4], shape = depth.names[4]), data = p1.data[node == my.node[4] & mins %in% c(0,30) & secs == 0,])
               
               # third depth
               + geom_line(aes(y = T.degC, x = date.time, color = depth.names[3], size = depth.names[3]), data = p1.data[node == my.node[3]])
               + geom_point(aes(y = T.degC, x = date.time, color = depth.names[3], shape = depth.names[3]), data = p1.data[node == my.node[3] & mins %in% c(0,30) & secs == 0,])
               
               # second depth
               + geom_line(aes(y = T.degC, x = date.time, color = depth.names[2], size = depth.names[2]), data = p1.data[node == my.node[2]])
               + geom_point(aes(y = T.degC, x = date.time, color = depth.names[2], shape = depth.names[2]), data = p1.data[node == my.node[2] & mins %in% c(0,30) & secs == 0,])
               
               # first depth
               + geom_line(aes(y = T.degC, x = date.time, color = depth.names[1], size = depth.names[1]), data = p1.data[node == my.node[1]])
               + geom_point(aes(y = T.degC, x = date.time, color = depth.names[1], shape = depth.names[1]), data = p1.data[node == my.node[1] & mins %in% c(0,30) & secs == 0,])
               
               # plot/axis titles & second axis for solar rad units
               #+ ggtitle("Modeled Surface Pavement Temperature")
               + labs(x = "Time of Day", y = "Temperature (deg C)")
               + scale_color_manual(name = "Pavement Depth", values = depth.col, labels = depth.names)
               + scale_shape_manual(name = "Pavement Depth", values = depth.shp, labels = depth.names)
               + scale_size_manual(name = "Pavement Depth", values = depth.siz, labels = depth.names)
               
               # scales
               + scale_x_datetime(expand = c(0,0), limits = c(min.x,max.x), date_breaks = "6 hours")
               + scale_y_continuous(expand = c(0,0), limits = c(min.y,max.y), breaks = seq(min.y,max.y,5))
               
               # theme and formatting
               + theme_minimal()
               + theme(text = element_text(family = my.font, size = 12),
                       plot.margin = margin(t = 10, r = 20, b = 10, l = 40, unit = "pt"),
                       plot.title = element_text(hjust = 0.75),
                       axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
                       axis.title.x = element_text(vjust = 0.3),
                       legend.title = element_text(size = 10),
                       legend.text = element_text(size = 9),
                       legend.position = c(.75,.85),
                       legend.background = element_blank())
    )
    
    

    
    # save plot
    dir.create(here("figures/1D-heat-model-runs/"), showWarnings = FALSE) # creates output folder if it doesn't already exist
    ggsave(paste0("run_",model.runs$run.n[run],"_1D-modeled-depth-temps.png"), p.depth, 
           device = "png", path = here("figures/1D-heat-model-runs"), scale = 1, width = 6.5, height = 5, dpi = 300, units = "in") # /20190121
    
    
    
    # Deviation (boxplot) of single scenario by nodes
    min.x <- min(p1.data$date.time, na.rm = T)
    max.x <- ceiling_date(max(p1.data[!is.na(T.degC), date.time]), unit = "hours")
    min.y <- round(signif(min(pave.time[, T.degC], na.rm = T) - (0.1 * diff(range(pave.time[, T.degC], na.rm = T))),4), -1)
    max.y <- round(signif(max(pave.time[, T.degC], na.rm = T) + (0.1 * diff(range(pave.time[, T.degC], na.rm = T))),4), -1)
    
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
write.csv(valids, here("data/outputs/1D-heat-model-runs/validation.csv"), row.names = F)


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
