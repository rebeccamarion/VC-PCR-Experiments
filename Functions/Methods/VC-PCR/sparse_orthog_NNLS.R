SparseOrthogNNLS <- function(M, U, Mn, gamma, M.sd){

  #[(1/(n-1))*u_k^t m_j - \gamma]_+

  n <- nrow(M) - 1
 
  A <- Crossprod((M), U) # Covariance of M and U
  wh.max <- apply(A, 1, which.max)
  R <- matrix(0, nrow = nrow(A), ncol = ncol(A))
  for (r in unique(wh.max)){
    wh.r <- which(wh.max == r)
    
    R[wh.r, r] <- pmax((((1/(n))*Crossprod(M[, wh.r, drop = F], U[, r, drop = F])) - as.matrix((gamma)*M.sd[wh.r])), 0)
  }

  wh.zero <- which(R < (.Machine)$double.eps)
  R[wh.zero] <- 0
  Rn <- R
  return(Rn)
}