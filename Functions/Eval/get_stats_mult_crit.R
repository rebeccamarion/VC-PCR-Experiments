GetStatsMultCrit <- function(eval.df, non.crit, group.by, stat = "mean"){
  
  crits <- colnames(eval.df)[-which(colnames(eval.df) %in% non.crit)]
  eval <- GetGroupStats(data = eval.df,
                        col.names = crits,
                        group.by = group.by,
                        stat = stat,
                        drop = T)
  non.crit <- colnames(eval)[which(colnames(eval) %in% non.crit)]
  eval <- eval[, c(non.crit, crits)]
  
  return(eval)
}