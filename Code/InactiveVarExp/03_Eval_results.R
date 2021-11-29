####################
### Eval results ###
####################
rm(list = ls())

data.name <- "InactiveVarExp"
real.data <- F
bin.y <- F
clust.stats <- T

source("../../Scripts/Set_file_paths.R")
source("../../Scripts/Load_all_functions.R")

library(foreach)
library(doParallel)
nb.cores <- 5

file.names <- list.files(paste0(data.path, "Results/"))
file.names <- file.names[grep("res_cv_list", file.names)]

data.files <- list.files(data.path)
data.files <- data.files[grep("exp_data", data.files)]
file.prefixes <- unlist(strsplit(data.files, "exp_data.RData"))

settings.index <- 4
for (settings.index in 1:(length(data.files))){
  
  file.prefix <- file.prefixes[settings.index]
  
  # Load data
  load(file = paste0(data.path, file.prefix, "exp_data.RData"))
  load(file = paste0(data.path, file.prefix, "exp_params.RData"))
  
  
  file.names.sub <- file.names[grep(file.prefix, file.names)]
  out.file.names <- paste0("eval_dfs_", sapply(strsplit(file.names.sub, 
                                                       "res_cv_list_"), function(x) paste0(x, collapse = "")))
  


#### Evaluate each method separately ####

method.index <- 1
for (method.index in 1:length(file.names.sub)){

  
  beg <- Sys.time()
  file.name <- file.names.sub[method.index]
  load(file = paste0(data.path, "Results/", file.name))
  
  # Inner loop
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
  rm("res.cv.list")
  end <- Sys.time()
  print(end - beg)
  print(paste0("finished for method ", method.index, " of ", length(file.names.sub), " for setting ", settings.index))
  
}
}