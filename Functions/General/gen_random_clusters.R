GenRandomClusters <- function(K, p){
  
  cand <- 1:K
  clusters <- sample(cand, size = p, replace = T)
  num.unique <- length(unique(clusters))
  iter <- 0
  while(num.unique != K & iter < 200){
    iter <- iter + 1
    clusters <- sample(cand, size = p, replace = T)
    num.unique <- n.unique(clusters)
  }
  
  if (iter == 200){
    clusters <- rep(NA, p)
    rand.indexes <- sample(1:p, size = K, replace = F)
    clusters[rand.indexes] <- 1:K
    clusters[-rand.indexes] <- sample(cand, size = p - K, replace = T)
  }
  
  return(clusters)
}