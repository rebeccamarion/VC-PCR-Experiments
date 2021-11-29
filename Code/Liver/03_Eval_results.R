######################
#### Eval results ####
######################

rm(list = ls())

data.name <- "Liver"
real.data <- T
bin.y <- F
clust.stats <- F

source("../../Scripts/Set_file_paths.R")
source("../../Scripts/Load_all_functions.R")

library(foreach)
library(doParallel)
nb.cores <- 5

load(file = paste0(data.path,"exp_data.RData"))
load(file = paste0(data.path,"exp_params.RData"))


#### Load results ####

file.names <- list.files(paste0(data.path, "Results/"))
file.names <- file.names[grep("res_cv_list", file.names)]
out.file.names <- paste0("eval_dfs", sapply(strsplit(file.names, "res_cv_list"), function(x) x[2]))

#### Evaluate each method separately ####

method.index <- 8
#for (method.index in 1:length(file.names)){
for (method.index in c(6)){
  
  beg <- Sys.time()
  file.name <- file.names[method.index]
  load(file = paste0(data.path, "Results/", file.name))
  res.list <- res.cv.list$inner
  fold.data.list <- res.cv.list$fold.data.inner$fold.data.list
  
  if (real.data){
    source(paste0(script.path, "Eval_results_real_data.R"))
  } else {
    source(paste0(script.path, "Eval_results.R"))
  }
  
  inner.eval <- eval.df
  res.list <- res.cv.list$outer
  fold.data.list <- res.cv.list$fold.data.outer$fold.data.list
  
  if (real.data){
    source(paste0(script.path, "Eval_results_real_data.R"))
  } else {
    source(paste0(script.path, "Eval_results.R"))
  }
  
  outer.eval <- eval.df
  eval.res <- list(inner = inner.eval, outer = outer.eval)
  save(eval.res, file = paste0(data.path, "Eval/", out.file.names[method.index]))
  rm(list = c("res.cv.list", "inner.eval", "outer.eval", "fold.data.list"))
  end <- Sys.time()
  print(end - beg)
  print(paste0("finished for method ", method.index, " of ", length(file.names)))

}
