cvPrepareLoopInputs <- function(exp.data, wh.rep, init.type, 
                                n.starts, alpha.vals, delta.vals, lambda.vals,
                                folds.outer, folds.inner, method.name){
  
  loop.list <- lapply(1:length(exp.data), function(data.index) 
    lapply(wh.rep, function(rep.index) 
      cvRepLoopInputs(exp.data = exp.data,
                      data.index = data.index,
                      rep.index = rep.index,
                      init.type = init.type,
                      n.starts = n.starts,
                      alpha.vals = alpha.vals,
                      delta.vals = delta.vals,
                      lambda.vals = lambda.vals,
                      folds.outer = folds.outer,
                      folds.inner = folds.inner,
                      method.name = method.name)))
  
  
  return(loop.list)
  
}