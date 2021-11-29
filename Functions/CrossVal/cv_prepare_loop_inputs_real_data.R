cvPrepareLoopInputsRealData <- function(sim.data, wh.rep, init.type, 
                                n.starts, alpha.vals, delta.vals, lambda.vals,
                                folds.outer, folds.inner, method.name, K.vals){
  
  loop.list <- lapply(1:length(sim.data), function(data.index) 
    lapply(wh.rep, function(rep.index) 
      cvRepLoopInputsRealData(sim.data = sim.data,
                      data.index = data.index,
                      init.type = init.type,
                      n.starts = n.starts,
                      alpha.vals = alpha.vals,
                      delta.vals = delta.vals,
                      lambda.vals = lambda.vals,
                      folds.outer = folds.outer,
                      folds.inner = folds.inner,
                      method.name = method.name,
                      K.vals = K.vals)))
  
  
  return(loop.list)
  
}