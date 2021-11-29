CRLAllHP <- function(fold.data, hps, Cs.init, clust.type = "kmeans", bin.y = F){
  
  Cs <- CRLClusteringStep(X = fold.data$X.norm,
                          Cs.init = Cs.init,
                          clust.type = clust.type)
  
  all.hp <- lapply(1:nrow(hps), function(hp.index) 
    CRLRegressionStep(fold.data = fold.data,
          delta = hps$delta[hp.index],
          Cs = Cs,
          bin.y = bin.y))
  
 
  return(all.hp)
}