#######################
### Check HP ranges ###
#######################
rm(list = ls())

library(ggplot2)
library(gridExtra)
library(data.table)
library(RColorBrewer)
data.name <- "Liver"
file.prefix <- NULL

source("../../Scripts/Set_file_paths.R")
source("../../Scripts/Load_all_functions.R")

file.names <- list.files(paste0(data.path, "Eval/"))
file.names <- file.names[grep("eval_dfs", file.names)]

load(file = paste0(data.path,"exp_data.RData"))
load(file = paste0(data.path,"exp_params.RData"))



#### Check out hps ####
method.index <- 8
for (method.index in 1:length(file.names)){
  file.name <- file.names[method.index]
  load(file = paste0(data.path, "Eval/", file.name))
  
  
  outer.all <- eval.res$outer
  outer.all <- outer.all[, -which(colnames(outer.all) %in% c("wh.fold.data", "wh.init", "inner.fold", "init.index"))]
  
  non.crit <- c("method", "K.init", "alpha", "lambda", "delta",
                "outer.fold", "data.index", "rep.index", "ground.truth")
  group.by <- non.crit[-which(non.crit == "outer.fold")] # mean over all outer folds for a given group
  means.outer <- GetStatsMultCrit(eval.df = outer.all,
                                  non.crit = non.crit,
                                  group.by = group.by)
  method.name <- unique(means.outer$method)
  
  
  perf.data <- means.outer
  x.var <- "s"
  y.var <- "MSEP"
  facet.vars <- c("K.init", "data.index")
  facet.rows <- 3
  

  if (method.name %in% c("CEN", "SRR")){
    
    col.factor <- "lambda"
    col.factor2 <- "delta"
    
    plot.list <- PlotPerfByHP(perf.data, x.var, y.var, col.factor, 
                              col.factor2, facet.vars, facet.rows)
    
  }
  
  if (length(grep("CRL", method.name)) > 0){
    col.factor <- NULL
    col.factor2 <- "delta"
    
    plot.list <- PlotPerfByHP(perf.data, x.var, y.var, col.factor, 
                              col.factor2, facet.vars, facet.rows)
  }
  
  if (length(grep("PCR", method.name)) > 0){
    if (length(grep("Identity", method.name)) > 0){
      col.factor <- NULL
    } else {
      col.factor <- "delta"
    }
    
    col.factor2 <- "lambda"
    
    plot.list <- PlotPerfByHP(perf.data, x.var, y.var, col.factor, 
                              col.factor2, facet.vars, facet.rows)
    
  }
  
  plot.file.name <- paste0("../../Plots/", data.name, "/Calibration/", file.prefix, method.name, ".pdf")
  pdf(file = plot.file.name, width = 10, height = 10)
  print(plot.list$p1)
  print(plot.list$p2)
  dev.off()
  
}
