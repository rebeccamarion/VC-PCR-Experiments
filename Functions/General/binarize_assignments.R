BinarizeAssignments <- function(clusters){
  
  
  S <- MakeBinaryVMatrix(as.numeric(factor(clusters)))
  SSt <- S%*%t(S)
  
  return(SSt[lower.tri(SSt, diag = F)])
 
}