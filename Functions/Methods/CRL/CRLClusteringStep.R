CRLClusteringStep <- function(X, Cs.init, clust.type = "kmeans"){
  
  
  require(ClustOfVar)
  # Cluster Representative Lasso
  
  X.tilde <- X
  sds <- apply(X.tilde, 2, sd)
  sds[sds == 0] <- 1
  X.norm <- scale(X.tilde, scale = sds, center = T) # scale matrix unlike for other methods
  
  
  # Step 1: find clusters
  if (clust.type == "kmeans"){
    V <- MakeBinaryVMatrix(Cs.init)
    V <- scale(V, center = F, scale = apply(V, 2, sum))
    centers <- t(X.norm%*%V)
    Cs <- kmeans(t(X.norm), centers = unique(centers),
                 algorithm = "Lloyd", iter.max = 100)$cluster
  }
  if (clust.type == "hclust"){
    K <- n.unique(Cs.init)
    clust.tree <- hclust(dist(t(X.norm)), method = "ward.D2")
    Cs <- cutree(clust.tree, K)
  }
  if (clust.type == "hclustCC"){
    K <- n.unique(Cs.init)
    clust.tree <- ClustOfVar::hclustvar(X.quanti = X.norm)
    Cs <- ClustOfVar::cutreevar(clust.tree, k = K)$cluster
  }
  
  return(Cs)
  
}