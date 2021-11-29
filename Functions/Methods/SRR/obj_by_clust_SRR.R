ObjByClustSRR <- function(X, y, lambda, delta, betas, k, n){
  betas.k <- betas[, k]
  LS <- (1/(2*n))*sum((y - X%*%betas.k)^2)
  pen1 <- delta*sum(abs(betas.k))
  pen2 <- (lambda/2)*sum(sapply(1:nrow(betas), function(j) 
    sapply((1:ncol(betas))[-k], function(ell) 
      abs(betas.k[j])*abs(betas[j, ell]))))
  obj <- LS + pen1 + pen2
  return(obj)
}