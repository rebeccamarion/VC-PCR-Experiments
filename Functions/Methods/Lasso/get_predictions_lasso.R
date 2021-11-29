GetPredictionsLasso <- function(fit, lambda, X.test){
  
  
  y.pred <- predict(fit, newx = X.test, s = lambda)
  return(y.pred)
}