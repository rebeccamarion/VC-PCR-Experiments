ScaleNonzeroVar <- function(X, center = T){
  scale.vec <- apply(X, 2, sd)
  scale.vec[scale.vec == 0] <- 1
  X.norm <- scale(X, center = center, scale = scale.vec)
  return(X.norm)
}