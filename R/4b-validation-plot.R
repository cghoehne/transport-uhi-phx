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
library(grid, lib.loc = lib.path, quietly = T)
library(gridExtra, lib.loc = lib.path, quietly = T)
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

# create RMSE function
RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}

# IMPORT MODEL DATA
model.runs <- readRDS(here("data/outputs/all_model_run_metadata_stats_20190316_135855.rds"))

###################
# create predicted vs modeled plots

min.x <- 0  # round(min(valids[,.(T.degC, T.degC.sat)] - 5), - 1)  
max.x <- round(max(model.runs[,.(Modeled,Observed)], na.rm = T) + 5, - 1)  #valids.long[, temp]
min.y <- min.x
max.y <- max.x

# create different legend charateristics for plotting
m.o.names <- levels(model.runs[, day.sea])
m.o.shp <- c(rep(16, 4),rep(18, 4))
m.o.col <- c("#0062DB", "#1E964E", "#CC0E2A", "#602E00", "#0062DB", "#1E964E", "#CC0E2A", "#602E00")
names(m.o.shp) <- m.o.names
names(m.o.col) <- m.o.names

p.name <- model.runs[ , batch.name]

p.mod_obs <- (ggplot(data = model.runs[!is.na(p.err)]) 
              
              # custom border
              + geom_segment(aes(x = min.x, y = min.y, xend = max.x, yend = min.y))   # x border (x,y) (xend,yend)
              + geom_segment(aes(x = min.x, y = min.y, xend = min.x, yend = max.y))  # y border (x,y) (xend,yend)
              
              # points for modeled vs observed + ref line
              + geom_point(aes(y = Modeled, x = Observed, color = day.sea, shape = day.sea), size = 2)
              + geom_abline(intercept = 0, slope = 1)
              
              # plot/axis titles & second axis for solar rad units
              + labs(x = "Observed Surface Temperature (deg C)", y = "Modeled Surface Temperature (deg C)")
              + annotate("text", label = paste("RMSE =", signif(RMSE(model.runs[!is.na(Observed), Modeled], 
                                                                     model.runs[!is.na(Observed), Observed])
                                                                , 3)), x = max.x * 0.85, y = 5, family = my.font)
              + scale_color_manual(name = "Season & Time of Day", values = m.o.col)
              + scale_shape_manual(name = "Season & Time of Day", values = m.o.shp)
              
              # scales
              + scale_x_continuous(expand = c(0,0), limits = c(min.x,max.x), breaks = seq(min.x,max.x,5))
              + scale_y_continuous(expand = c(0,0), limits = c(min.y,max.y), breaks = seq(min.y,max.y,5))
              
              # theme and formatting
              + theme_minimal()
              + theme(text = element_text(family = my.font, size = 12, colour = "black"),
                      axis.text = element_text(colour = "black"),
                      plot.margin = margin(t = 10, r = 10, b = 15, l = 10, unit = "pt"),
                      plot.title = element_text(hjust = 0.75),
                      axis.text.x = element_text(vjust = 1, hjust = 1),
                      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                      axis.ticks.x = element_line(color = "black", size = 0.25),
                      axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                      legend.position = c(0.85, 0.35), 
                      #legend.direction ="horizontal",
                      legend.background = element_blank())
)
p.mod_obs

p.mod_obs <- arrangeGrob(p.mod_obs, bottom = textGrob(paste0("Material Type: ", p.name), 
                                                      gp = gpar(fontfamily = my.font))) # add pave name
ggsave(paste0(gsub(" ", "-", gsub(" / ", "-", tolower(p.name))), ".png"), p.mod_obs, # save plot
       device = "png", path = paste0(out.folder,"/figures"), 
       scale = 1, width = 6, height = 5, dpi = 300, units = "in") 

# check rankings of RMSE to see under what circumstances temps were most accurate
model.runs[!is.na(p.err), RMSE(Modeled, Observed), by = c("end.day")][order(V1)]
model.runs[!is.na(p.err), RMSE(Modeled, Observed), by = c("valid.site")][order(V1)]
model.runs[!is.na(p.err), RMSE(Modeled, Observed), by = c("layer.profile")][order(V1)]
model.runs[!is.na(p.err), RMSE(Modeled, Observed), by = c("station.name")][order(V1)]

