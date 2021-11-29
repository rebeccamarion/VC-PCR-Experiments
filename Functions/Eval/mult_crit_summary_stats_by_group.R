MultCritSummaryStatsByGroup <- function(data, crits, group.by){
  
  
  summary.data <- lapply(crits, function(crit) 
    SummaryStatsByGroup(data = data,
                     measurevar = crit,
                     groupvars = group.by,
                     na.rm = T,
                     keep.cols = c(crit, "sd", "max", "min")))
  just.crits <- do.call(cbind.data.frame, lapply(summary.data, function(x) x[, -which(colnames(x) %in% group.by)]))
  data.info <- summary.data[[1]][, group.by]
  out <- cbind.data.frame(data.info, just.crits)
  return(out)
}