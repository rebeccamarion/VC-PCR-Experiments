SOSNMF <- function(X.tilde, X.tilde.n, X.sd, V, gamma, max.iter = 500){
  
  
  iter <- 1
  
  
  VtV.inv <- GetVtVInv(V)
  U.tilde <- Crossprod(t(X.tilde), V)%*%VtV.inv
  U <- ScaleNonzeroVar(U.tilde, center = T)
  
  err.old <- Inf
  err <- ObjectiveSOSNMF(X.tilde, U, V, X.sd, gamma)
  
  
  while (iter < max.iter && (abs(err.old - err)/err) > 1e-4){
    
    # Update V
    V <- SparseOrthogNNLS(M = X.tilde, U = U, Mn = X.tilde.n, gamma = gamma, M.sd = X.sd)
    
    # Update U
    VtV.inv <- GetVtVInv(V)
    U.tilde <- Crossprod(t(X.tilde), V)%*%VtV.inv
    U <- ScaleNonzeroVar(U.tilde, center = T)
    
    
    err.old <- err
    err <- ObjectiveSOSNMF(X.tilde, U, V, X.sd, gamma)
    iter <- iter + 1
  }
 
  return(list(U = U, V = V))
}