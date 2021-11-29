ExtractLassoOutputs <- function(fit, index, X.test, y.test){
  
  betas <- fit$beta[, index]
  lambda <- fit$lambda[index]
  y.pred <- GetPredictionsLasso(fit = fit, lambda = lambda, 
                                X.test = X.test)
  return(list(betas = betas, y.pred = y.pred, y.true = y.test))
  
}
