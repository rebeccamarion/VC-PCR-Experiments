L2Norm <- function(x, nonzero = F) {
  
  norm.val <- sqrt(sum(x*x))
  
  if (nonzero == T){
    norm.val <- ifelse(norm.val == 0, 1, norm.val)
  }
  
  return(norm.val)
  
}