ObjectiveSOSNMF <- function(X.tilde, U, V, X.sd, gamma){
  
  n <- nrow(X.tilde)
  diff.PCA <- X.tilde - U%*%t(V)
  LS.PCA <- (1/(2*(n-1)))*sum(diff.PCA^2)
  lasso.term.clust <- sum(abs(t(V) %*% diag(gamma*X.sd)))
  obj <- LS.PCA + lasso.term.clust
  
  return(obj)
}