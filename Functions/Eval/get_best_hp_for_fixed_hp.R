GetBestHPForFixedHP <- function(fixed.hp, mean.data, group.by, calib.crit, minimize){
  require(data.table)
  if (sum(is.na(fixed.hp)) > 0 | is.null(fixed.hp)){
    out <-  GetBestHP(mean.data = mean.data,
                           group.by = group.by,
                           var.hp = calib.crit,
                           minimize = minimize)
  } else {
  
  fixed.hp.vals <- as.matrix(unique(mean.data[, fixed.hp, drop = F]))
  mean.data <- as.data.table(mean.data)
  setkeyv(mean.data, fixed.hp)
  
  best <- lapply(1:nrow(fixed.hp.vals), function(hp.index) {
    temp.dt <- as.data.table(fixed.hp.vals[hp.index, , drop = F])
    setkeyv(temp.dt, fixed.hp)
    mean.sub <- mean.data[temp.dt] # get rows that correspond to fixed.hp.vals for hp.index 
    mean.sub <- as.data.frame(mean.sub)
    return(GetBestHP(mean.data = mean.sub,
              group.by = group.by,
              var.hp = calib.crit,
              minimize = minimize))
  })
  best.new <- RevertList(best)
  out <- lapply(best.new, function(x) do.call(rbind.data.frame, lapply(x, function(y) y[which(y$is.best.hp == T), ])))
  }
  return(out)
}