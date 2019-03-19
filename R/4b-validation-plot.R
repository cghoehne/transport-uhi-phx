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
all.model.runs <- readRDS(here("data/outputs/stats_all_model_runs.rds"))

# filter if necessary / as desired
model.runs <- all.model.runs[p.err <= 0.35]

# check rankings of RMSE to see under what circumstances temps were most accurate
model.runs[!is.na(p.err), RMSE(Modeled, Observed), by = c("end.day")][order(V1)]
model.runs[!is.na(p.err), RMSE(Modeled, Observed), by = c("valid.site")][order(V1)]
model.runs[!is.na(p.err), RMSE(Modeled, Observed), by = c("layer.profile", "batch.name")][order(V1)]
model.runs[!is.na(p.err), RMSE(Modeled, Observed), by = c("station.name")][order(V1)]

# check rankings of RMSE to see under what circumstances temps were most accurate
model.runs[!is.na(p.err), mean(p.err), by = c("end.day")][order(V1)]
model.runs[!is.na(p.err), mean(p.err), by = c("valid.site")][order(V1)]
model.runs[!is.na(p.err), mean(p.err), by = c("layer.profile", "batch.name")][order(V1)]
model.runs[!is.na(p.err), mean(p.err), by = c("station.name")][order(V1)]

###################
# create predicted vs modeled plots

for(p.name in unique(model.runs[, batch.name])){
  
  p.runs <- model.runs[!is.na(p.err) & batch.name == p.name,]
  
  min.x <- 0  # round(min(valids[,.(T.degC, T.degC.sat)] - 5), - 1)  
  max.x <- round(max(p.runs[,.(Modeled,Observed)], na.rm = T) + 5, - 1)  #valids.long[, temp]
  min.y <- min.x
  max.y <- max.x
  p.letter <- paste0("(", chartr("1234", "abcd", which(p.name == unique(model.runs[, batch.name]))), ")")
  
  # create different legend charateristics for plotting
  m.o.names <- levels(p.runs[, day.sea])
  m.o.shp <- c(rep(16, 4),rep(18, 4))
  m.o.col <- c("#0062DB", "#1E964E", "#CC0E2A", "#602E00", "#0062DB", "#1E964E", "#CC0E2A", "#602E00")
  names(m.o.shp) <- m.o.names
  names(m.o.col) <- m.o.names

  my.plot <- (ggplot(data = p.runs) 
              
              # custom border
              + geom_segment(aes(x = min.x, y = min.y, xend = max.x, yend = min.y))   # x border (x,y) (xend,yend)
              + geom_segment(aes(x = min.x, y = min.y, xend = min.x, yend = max.y))  # y border (x,y) (xend,yend)
              
              # points for modeled vs observed + ref line
              + geom_point(aes(y = Modeled, x = Observed, color = day.sea, shape = day.sea), size = 2.5)
              + geom_abline(intercept = 0, slope = 1)
              
              # plot/axis titles & second axis for solar rad units
              + labs(x = "Observed Surface Temperature (deg C)", y = "Modeled Surface Temperature (deg C)")
              + annotate("text", label = paste("RMSE =", signif(RMSE(p.runs[!is.na(Observed), Modeled], 
                                                                     p.runs[!is.na(Observed), Observed]), 3)), 
                         x = max.x * 0.85, y = max.y / 12, family = my.font)
              + annotate("text", label = paste0("MAPE = ", signif(mean(p.runs[, p.err]), 3) * 100, "%"), 
                         x = max.x * 0.85, y = max.y / 6, family = my.font)
              + scale_color_manual(name = "Season & Time of Day", values = m.o.col)
              + scale_shape_manual(name = "Season & Time of Day", values = m.o.shp)
              
              # scales
              + scale_x_continuous(expand = c(0,0), limits = c(min.x,max.x), breaks = seq(min.x,max.x,10))
              + scale_y_continuous(expand = c(0,0), limits = c(min.y,max.y), breaks = seq(min.y,max.y,10))
              
              # theme and formatting
              + theme_minimal()
              + theme(text = element_text(family = my.font, size = 10, colour = "black"),
                      axis.text = element_text(colour = "black"),
                      plot.margin = margin(t = 10, r = 10, b = 15, l = 10, unit = "pt"),
                      plot.title = element_text(hjust = 0.75),
                      axis.title.x = element_text(margin = margin(t = 4, r = 0, b = 0, l = 0)),
                      axis.ticks.x = element_line(color = "black", size = 0.25),
                      axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                      legend.position = "none")
  )
  
  my.title <- textGrob(label = paste(p.letter, p.name),
                       x = unit(4, "mm"), y = unit(2, "mm"), hjust = 0, vjust = 0, # right and bottom align inside grob
                       gp = gpar(fontfamily = my.font, fontsize = 10, fontface = "bold"))
  my.plot <- arrangeGrob(my.plot, top = my.title) # title
  assign(paste0("my.plot.", unique(model.runs[batch.name == p.name, batch.id])), my.plot)
}

# legend
my.legend <- legendGrob(labels = m.o.names, nrow = 2, pch = m.o.shp, 
                        vgap = unit(2, "mm"),
                        gp = gpar(col = m.o.col, fill = m.o.col, size = 0.5, fontfamily = my.font, fontsize = 10))

# combine into multi-plot
my.plots <- lapply(unique(model.runs[, batch.id]), function(.x) get(paste0("my.plot.", .x)))
meta.plot <- grid.arrange(layout_matrix = rbind(c(1,2), c(3,4)), grobs = my.plots)
meta.plot <- arrangeGrob(meta.plot, bottom = my.legend)
ggsave("modeled-observed.png", meta.plot, # save plot
       device = "png", path = "figures", 
       scale = 1.5, width = 6, height = 5, dpi = 300, units = "in")

