GetBestHP <- function(mean.data, group.by, var.hp, minimize = T){
  
  groups <- group.by
  
  group.list <- lapply(groups, function(group.name) paste0(mean.data[, group.name]))
  group.indexes <- tapply(mean.data[, var.hp[1]],
                          INDEX = group.list,
                          FUN = NULL)
  
  calib.list <- list()
  for (calib.index in 1:length(var.hp)){
    
    mean.data$is.best.hp <- rep(F, nrow(mean.data))
    for (group.index in unique(group.indexes)){
      
      wh <- which(group.indexes == group.index)
      
      if (minimize[calib.index]){
        wh.best <- which.min(mean.data[wh, var.hp[calib.index]])
      } else {
        wh.best <- which.max(mean.data[wh, var.hp[calib.index]])
      }
      
      mean.data$is.best.hp[wh[wh.best]] <- T
      
      
    }
    
    calib.list[[calib.index]] <- mean.data
  }
  
  names(calib.list) <- var.hp
  return(calib.list)
  
}