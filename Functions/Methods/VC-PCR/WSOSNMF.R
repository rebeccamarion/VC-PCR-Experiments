WSOSNMF <- function(X, weights, lambda, Cs.init, X.sd){
  if (NonzeroCoeffs(weights) > 0){
    
    
    # Add weights to variables
    W <- diag(weights)
    XW <- X%*%W
    XW.n <- ScaleNonzeroL2Norm(XW)
    
    # initialize V
    V <- MakeBinaryVMatrix(Cs.init)
    
    # calculate max gamma val
    VtV.inv <- GetVtVInv(V)
    U.tilde <- Crossprod(t(XW), V)%*%VtV.inv
    U <- ScaleNonzeroVar(U.tilde, center = T)
    cor.vals <- (1/(nrow(X)-1))*(t(U)%*%XW)
    gamma.max <- max(cor.vals)
    
    
    # Optimize U and V
    res <- SOSNMF(X.tilde = XW, 
                  X.tilde.n = XW.n, 
                  X.sd = X.sd, 
                  V = V, 
                  gamma = lambda*gamma.max)
    
    
  } else {
    
    V <- matrix(rep(0, ncol(X)))
    U <- matrix(rep(0, nrow(X)))
    res <- list(U = U, V = V)
    
  }
  
  return(res)
}