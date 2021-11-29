MakeBinaryVMatrix <- function(clusters){
 
    nonzero <- clusters != 0
    
    if (sum(nonzero) == 0){
      V <- matrix(0, nrow = length(clusters), ncol = 1)
    } else {
      clusters[nonzero] <- as.numeric(factor(clusters[nonzero]))
      clust.indexes <- cbind(1:length(clusters), clusters)[nonzero, ]
      V <- matrix(0, nrow = length(clusters), ncol = max(clusters[nonzero]))
      V[clust.indexes] <- 1
    }
    
   
  return(V)
  
}