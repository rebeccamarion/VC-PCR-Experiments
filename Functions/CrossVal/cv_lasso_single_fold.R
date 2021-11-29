cvLassoSingleFold <- function(fold.data, 
                               param.vals,
                               scale.X, 
                               hps, 
                               support.true,
                            
                               method.name,
                               bin.y = F){
  
 
#  profvis({
  
  all.hp <- cvLasso(fold.data = fold.data, hps =hps, bin.y = bin.y)
  res <- all.hp
  rownames(param.vals) <- NULL
  res.params <- cbind.data.frame(method = method.name,
                                 hps,
                                 param.vals)
  
  out <- list(res = res,
              res.params = res.params,
              support.true = support.true)
  return(out)
  
}