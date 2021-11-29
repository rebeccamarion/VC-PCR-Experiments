ScaleNonzeroL2Norm <- function(X, center = F){
  scale.vec <- apply(X, 2, L2Norm, nonzero = T)
  X.norm <- scale(X, center = center, scale = scale.vec)
  return(X.norm)
}