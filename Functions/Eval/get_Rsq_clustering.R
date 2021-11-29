GetRsqClustering <- function(X, Cs){
  V <- MakeBinaryVMatrix(Cs)
  M <- X%*%V%*%solve(t(V)%*%V)%*%t(V)
  Xbar <- apply(X, 1, mean)
  SSW <- apply((X - M)^2, 1, sum)
  SSE <- sapply(1:nrow(X), function(i) sum((X[i, ] - Xbar[i])^2))
  SSB <- sapply(1:nrow(X), function(i) sum((M[i, ] - Xbar[i])^2))
  Rsq <- pmax(1 - (SSW/SSE), 0)
  
  return(mean(Rsq))
}