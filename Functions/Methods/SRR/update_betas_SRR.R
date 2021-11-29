UpdateBetasSRR <- function(X, y, lambda, delta, betas.prev){
  
  betas <- betas.prev
  for (k in 1:ncol(betas)){
    
    betas[, k] <- UpdateBetasByClust(X = X,
                                     y = y,
                                     lambda = lambda, 
                                     delta = delta, 
                                     betas.prev = betas, 
                                     k = k)
    
  }
  
  return(betas)
}