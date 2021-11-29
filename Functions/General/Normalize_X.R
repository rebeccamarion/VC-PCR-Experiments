NormalizeX <- function(X, scale.vars = F){
  
  X.norm <- scale(X, center = T, scale = F)
  if (scale.vars == T){
    X.norm <- scale(X.norm, center = F, scale = apply(X, 2, sd))
  } 
  
  return(X.norm)
}