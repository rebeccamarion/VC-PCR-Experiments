cvPrepareLoopInputsLasso <- function(sim.data, wh.rep, lambda.vals,
                                folds.outer, folds.inner, method.name){
  
  loop.list <- lapply(1:length(sim.data), function(data.index) 
    lapply(wh.rep, function(rep.index) 
      cvRepLoopInputsLasso(sim.data = sim.data,
                      data.index = data.index,
                      rep.index = rep.index,
                      
                      lambda.vals = lambda.vals,
                      folds.outer = folds.outer,
                      folds.inner = folds.inner,
                      method.name = method.name)))
  
  
  return(loop.list)
  
}