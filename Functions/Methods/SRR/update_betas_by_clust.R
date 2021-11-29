UpdateBetasByClust <- function(X, y, lambda, delta, betas.prev, k){
  
  
  betas <- betas.prev[, k]
  Xbeta <- X%*%betas
  sums.betas <- apply(abs(betas.prev[, -k, drop = F]), 1, sum)
  n <- nrow(X)
  for (j in 1:length(betas)){
    
    Xj <- X[,j]
    oldcoef <- betas[j]
    # residual vector
    ytildej <- y - (Xbeta - Xj*oldcoef)
    sum.beta.j <- sums.betas[j]
    thresh <- delta + (lambda*sum.beta.j)
    newcoef <- SoftThreshSRR((1/n)*sum(Xj*(ytildej)), 
                             thresh)
    # beta update
    betas[j] <- newcoef
    # new predicted vals: Xbeta - Xj*old.coef + Xj*newcoef
    # Xbeta - Xj*old.coef = X_-j*Beta_-j
    Xbeta <- Xbeta + Xj*(newcoef-oldcoef)
    
  }
  
  return(betas)
}