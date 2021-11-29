GetOuterFoldResGivenHP <- function(eval.outer, eval.inner, group.by){
  
  
    all.eval <- rbind.data.frame(cbind.data.frame(eval.outer[, c(group.by)], type = "outer", row.index = 1:nrow(eval.outer)),
                                 cbind.data.frame(eval.inner[, c(group.by)], type = "inner", row.index = 1:nrow(eval.inner)))
    group.list <- lapply(group.by, function(x) all.eval[, x])
    group.indexes <- tapply(all.eval[, "row.index"], group.list, FUN = NULL)
    
    eval.outer <- cbind.data.frame(eval.outer, group.index = group.indexes[which(all.eval$type == "outer")])
    eval.inner <- cbind.data.frame(eval.inner, group.index = group.indexes[which(all.eval$type == "inner")])
    
    
    res.subset <- do.call(rbind.data.frame, lapply(1:nrow(eval.inner), function(index.row) {
      wh <- which(eval.outer$group.index == eval.inner$group.index[index.row] &
                    eval.outer$hp.index == eval.inner$hp.index[index.row])
      sub <- eval.outer[wh, ]
      return(sub)
    }))
 
  
  return(res.subset)
  
}
