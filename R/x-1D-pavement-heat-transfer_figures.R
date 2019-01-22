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

# load simulated pavement temperature data for run 
run <- 1
pave.time <- readRDS(here(paste0("data/outputs/1D-heat-model-runs/run_",run,"_output.rds")))

# read in sample weather data 
weather <- readRDS(here("data/outputs/temp/sample-weather-data.rds")) # 3 day period of weather data at 15 min intervals

# recreate time step vector
t.step <- seq(from = 0, # from time zero
              to = as.numeric(difftime(max(weather$date.time), min(weather$date.time), units = "secs")) + pave.time[2,time.s], 
              by = pave.time[2,time.s]) 

## replicate Gui et al. Fig. 2 

# depth temp
my.node <- 1
p1.data <- pave.time[node == my.node & time.s <= t.step[p]]
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
       + scale_x_datetime(expand = c(0,0), limits = c(min.x,max.x), date_breaks = "6 hours")
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
       device = "png", path = here("figures/1D-heat-model-runs"), scale = 1, width = 6.5, height = 5, dpi = 300, units = "in")


# SURFACE TEMP PLOT

# for including air temps, instead use:
#min.y <- pmin(signif(min(p0.data[, T.degC], na.rm = T) - (0.1 * diff(range(p0.data[, T.degC], na.rm = T))),4), min(weather$temp.c), na.rm = T) 
#max.y <- signif(max(p0.data[, T.degC], na.rm = T) + (0.1 * diff(range(p0.data[, T.degC], na.rm = T))),4) 

p0.data <- pave.time[node == 0 & time.s <= t.step[p]]
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