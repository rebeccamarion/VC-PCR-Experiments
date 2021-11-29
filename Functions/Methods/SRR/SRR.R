SRR <- function(fold.data, lambda, delta, K, max.iter = 50){
  
  X <- fold.data$X.norm
  y <- fold.data$y.norm

  obj <- NULL; counter <- 0; n <- nrow(X);
  
  betas <- matrix(0, nrow = ncol(X), ncol = K)
  
  diff.betas <- Inf
  # iterate a maximum of max.iter times or diff.betas is smaller than a certain threshold 
  while(counter<max.iter && diff.betas > 1e-5){
    
    counter <- counter + 1
    betas.prev <- betas 
    betas <- UpdateBetasSRR(X, y, lambda, delta, betas.prev)
    
    av.betas <- apply(betas, 1, mean)
    av.betas.prev <- apply(betas.prev, 1, mean)
    diff.betas <- max((av.betas - av.betas.prev)^2)
    
    obj <- c(obj, 
             sum(sapply(1:ncol(betas), function(k) ObjByClustSRR(X, y, lambda, delta, betas, k = k, n))))
    
    
  }
  
  if (sum(betas != 0) == 0){
    Cs <- rep(1, ncol(X))
  } else {
    Cs <- apply(abs(betas), 1, which.max)
  }
  
  y.pred <- fold.data$X.test%*%av.betas
  
  return(list(betas=av.betas,betas.all=betas,Cs=Cs,obj = obj, y.pred = y.pred, y.true = fold.data$y.test))
  
}