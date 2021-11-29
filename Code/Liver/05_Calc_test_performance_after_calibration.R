#############################
### Calc test performance ###
#############################

rm(list = ls())

library(ggplot2)
library(gridExtra)
library(data.table)
library(RColorBrewer)
data.name <- "Liver"

# Names of hps
hps <- c("alpha", "delta", "lambda", "K.init")
# Name of hp that is fixed for each method (in order of file names)
# CEN, CRL-hclust, CRL-hclustCC, CRL-kmeans, SRR, VC-PCR-Identity, VC-PCR-Lasso, VC-PCR-Ridge
fixed.hps <- c(c("delta"), c("delta"), c("delta"), c("delta"), c("delta"), c("lambda"), c("lambda"), c("lambda"))
# Crit used to calibrate hps
calib.crit <- c("MSEP")
# Should the crit be minimized?
minimize <- c(T)
# Colnames that are not numerical criteria
non.crit <- c("method", "K.init", "alpha", "lambda", "delta", "inner.fold",
              "outer.fold", "data.index", "rep.index", "init.index", "wh.init")

source("../../Scripts/Set_file_paths.R")
source("../../Scripts/Load_all_functions.R")

file.names <- list.files(paste0(data.path, "Eval/"))
file.names <- file.names[grep("eval_dfs", file.names)]

data.files <- list.files(data.path)
data.files <- data.files[grep("exp_data", data.files)]



  
  load(file = paste0(data.path, "exp_data.RData"))
  load(file = paste0(data.path, "exp_params.RData"))
  
  
  
  #### Evaluate each method separately ####
  
  outer.means <- list() # OOS Mean performance across outer folds (separate values for each data.index, rep.index, wh.init)
  outer.rep.means <- list() # OOS Mean performance across outer folds and reps (separate values for each data.index, wh.init)
  outer.init.stats <- list()
  
  method.index <- 8
  for (method.index in 1:length(file.names)){
    
    file.name <- file.names[method.index]
    load(file = paste0(data.path, "Eval/", file.name))
    
    inner.all <- eval.res$inner
    outer.all <- eval.res$outer
    
    #### INNER LOOP ####
    
    ## Calculate mean validation performance for hps for each outer fold, data type, repetition and initialization = mean of each inner fold
    inner.all <- inner.all[, -which(colnames(inner.all) %in% c("wh.fold.data", "ground.truth"))]
    group.by <- non.crit[-which(non.crit == "inner.fold")] # mean over all inner folds for a given group
    means.inner <- GetStatsMultCrit(eval.df = inner.all,
                                    non.crit = non.crit,
                                    group.by = group.by,
                                    stat = "mean")
    
    # K.init <- 4
    # delta <- unique(inner.all$delta)[4]
    # lambda <- unique(inner.all$lambda)[2]
    # outer.fold <- 5
    # data.index <- 2
    # rep.index <- 1
    # wh.init <- 5
    # 
    # data <- inner.all
    # wh <- which(data$K.init == K.init &
    #               data$delta == delta &
    #               data$lambda == lambda &
    #               data$outer.fold == outer.fold &
    #               data$data.index == data.index &
    #               data$rep.index == rep.index &
    #               data$wh.init == wh.init)
    # data <- means.inner
    # wh2 <- which(data$K.init == K.init &
    #                data$delta == delta &
    #                data$lambda == lambda &
    #                data$outer.fold == outer.fold &
    #                data$data.index == data.index &
    #                data$rep.index == rep.index &
    #                data$wh.init == wh.init)
    # mean(inner.all[wh, "MSEP"])
    # means.inner[wh2, "MSEP"]
    # 
    #### OUTER LOOP ####
    
    ## Get outer fold means for the best hps for each inner fold ##
    
    # Add hp index column to match inner loop hps with outer loop hps
    outer.all <- outer.all[, colnames(means.inner)]
    inner.and.outer <- rbind.data.frame(cbind.data.frame(outer.all, outer = T), 
                            cbind.data.frame(means.inner, outer = F))
    
    hps.method <- hps[!(hps %in% fixed.hps[[method.index]])]
    hp.list <- lapply(hps.method, function(x) paste0(inner.and.outer[, x]))
    hp.indexes <- tapply(inner.and.outer[, "s"], INDEX = hp.list, FUN = NULL)
    inner.and.outer$hp.index <- hp.indexes
    
    
    inner.calib.list <- GetBestHPForFixedHP(fixed.hp = fixed.hps[[method.index]],
                                 mean.data = inner.and.outer[inner.and.outer$outer == F, , drop = F],
                                 group.by = c("method", "data.index", "rep.index", "outer.fold", "wh.init"), 
                                 calib.crit = calib.crit,
                                 minimize = minimize)
    
    # Out of sample performance for selected hps for all levels of data.index, rep.index, outer.fold, wh.init, fixed hp
    outer.res.list <- lapply(1:length(inner.calib.list), function(x) {
      if (sum(inner.calib.list[[x]]$is.best.hp) > 0){
        GetOuterFoldResGivenHP(eval.outer = inner.and.outer[inner.and.outer$outer == T, , drop = F],
                               eval.inner = inner.calib.list[[x]][which(inner.calib.list[[x]]$is.best.hp), ],
                               group.by = c("method", "data.index", "rep.index", "outer.fold", "wh.init", fixed.hps[[method.index]])) 
      }
    })
    
    names(outer.res.list) <- names(inner.calib.list)
    
    
    ## Mean over outer.fold (fixed data.index, rep.index, wh.init)
    eval.res <- outer.res.list
    non.crit2 <- c(colnames(eval.res[[1]])[which(colnames(eval.res[[1]]) %in% non.crit)], "outer", "group.index", "hp.index")
    group.by <- c("method", "data.index", "rep.index", "wh.init", fixed.hps[[method.index]])
    
    outer.mean.list <- lapply(1:length(eval.res), function(x) {
      if (length(eval.res[[x]]) > 0){
        GetStatsMultCrit(eval.df = eval.res[[x]], 
                         non.crit = non.crit2, 
                         group.by = group.by,
                         stat = "mean")
      }
    })
    names(outer.mean.list) <- names(eval.res)
    outer.sd.list <- lapply(1:length(eval.res), function(x) {
      if (length(eval.res[[x]]) > 0){
        GetStatsMultCrit(eval.df = eval.res[[x]], 
                         non.crit = non.crit2, 
                         group.by = group.by,
                         stat = "sd")
      }
    })
    names(outer.sd.list) <- names(eval.res)
    outer.means[[method.index]] <- outer.mean.list
    
    ## Mean over rep.index (fixed data.index, wh.init)
    eval.res <- outer.mean.list
    non.crit3 <- c(colnames(eval.res[[1]])[which(colnames(eval.res[[1]]) %in% non.crit)])
    group.by <- c("method", "data.index", "wh.init", fixed.hps[[method.index]])
    
    outer.rep.mean.list <- lapply(1:length(eval.res), function(x) {
      if (length(eval.res[[x]]) > 0){
        GetStatsMultCrit(eval.df = eval.res[[x]], 
                         non.crit = non.crit3, 
                         group.by = group.by,
                         stat = "mean")
      }
    })
    names(outer.rep.mean.list) <- names(eval.res)
    outer.rep.means[[method.index]] <- outer.rep.mean.list
    
    eval.res <- outer.sd.list
    outer.rep.sd.list <- lapply(1:length(eval.res), function(x) {
      if (length(eval.res[[x]]) > 0){
        GetStatsMultCrit(eval.df = eval.res[[x]], 
                         non.crit = non.crit3, 
                         group.by = group.by,
                         stat = "mean")
      }
    })
    names(outer.rep.sd.list) <- names(eval.res)
    
    
    ## Summary stats for each data.index (quantify variation related to wh.init)
    group.by <- c("method", "data.index", fixed.hps[[method.index]])
    col.names <- colnames(outer.rep.mean.list[[1]])
    non.crit4 <- c(col.names[which(col.names %in% group.by)], "wh.init")
    crits <- col.names[-which(col.names %in% non.crit4)]
    
    outer.init.stats.list <- lapply(1:length(outer.rep.mean.list), function(x) 
      cbind.data.frame(MultCritSummaryStatsByGroup(data = outer.rep.mean.list[[x]],
                                  crits = crits,
                                  group.by = group.by), crit = names(inner.calib.list)[x]))
    outer.init.stats.list.sd <- lapply(1:length(outer.rep.sd.list), function(x) {
      temp <- cbind.data.frame(MultCritSummaryStatsByGroup(data = outer.rep.sd.list[[x]],
                                                   crits = crits,
                                                   group.by = group.by), 
                               crit = names(inner.calib.list)[x])
      
     temp[, "crit"] <- paste0("sd.", temp[, "crit"])
      return(temp)
    })
      
    
    outer.init.stats.df <- do.call(rbind.data.frame, outer.init.stats.list)
    outer.init.stats.df.sd <- do.call(rbind.data.frame, outer.init.stats.list.sd)
    outer.init.stats.df <- rbind.data.frame(outer.init.stats.df, outer.init.stats.df.sd)
    
    # Make it so that each method has the same colnames and order
    
      
      hp.mat <- as.data.frame(matrix(NA, nrow = nrow(outer.init.stats.df), ncol = length(hps)))
      colnames(hp.mat) <- hps
      hp.mat[, fixed.hps[[method.index]]] <- outer.init.stats.df[, fixed.hps[[method.index]]]
      wh.hp.cols <- which(colnames(outer.init.stats.df) %in%
                          hps)
      outer.init.stats.df <- outer.init.stats.df[, -wh.hp.cols, drop = F]
      
      outer.init.stats.df <- cbind.data.frame(outer.init.stats.df, hp.mat)
   
    
    
    outer.init.stats[[method.index]] <- outer.init.stats.df[order(outer.init.stats.df$data.index), ]
    
    
  }
  
  plot.data <- do.call(rbind.data.frame, outer.init.stats)
  save(plot.data, file = paste0(data.path, "Eval_stats/", "plot_data_all.RData"))


