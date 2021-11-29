GetGroupStats <- function(data, col.names, group.by, stat = "mean", drop = T, save.cols = NULL){
  
  
  has.NA <- sapply(group.by, function(x) mean(is.na(unique(data[, x]))))
  save.cols <- NULL
  if (sum(has.NA == 1) > 0){
    save.cols <- group.by[has.NA == 1]
  }
  group.by <- group.by[which(has.NA != 1)]
  
  stats.list <- lapply(1:length(col.names), function(col.index) {
    aggregate(data[, col.names[col.index]],
              by = data[, group.by, drop = F], 
              FUN = stat,
              drop = drop,
              na.rm = T)
  })
  
  group.info <- stats.list[[1]][, 1:length(group.by)]
  stat.vals <- do.call(cbind.data.frame, lapply(stats.list, function(x) x[, length(group.by) + 1]))
  colnames(stat.vals) <- col.names
  stats <- cbind(group.info, stat.vals)
  
  
  if (!is.null(save.cols)){
    
    extra <- do.call(cbind.data.frame, lapply(1:length(save.cols), function(col.index) 
      aggregate(data[, save.cols[col.index]],
                       by = data[, group.by, drop = F], 
                       FUN = function(x) x[1],
                       drop = drop)$x))
    
    colnames(extra) <- save.cols
    stats <- cbind.data.frame(stats, extra)
    
  }
  
  return(stats)
}