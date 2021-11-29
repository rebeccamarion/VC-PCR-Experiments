# update clusters with betas fixed
UpdateCs <- function(Xb, K, Cs.init, nstart=1){ 
  V <- MakeBinaryVMatrix(Cs.init)
  V <- scale(V, center = F, scale = apply(V, 2, sum))
  centers <- t(Xb%*%V)
  twonorm <- sqrt(apply(Xb^2,2,sum))
  if(sum(twonorm!=0)==0) {
    Cs <- rep(1,ncol(Xb))
  } else {
    wcss.old <- GetWCSS(Xb, Cs.init)  
    km.out <- kmeans(t(Xb), centers = unique(centers), algorithm = "Lloyd", iter.max = 100)
    
    if(sum(km.out$withinss) < wcss.old) {
      Cs <- km.out$cluster
    } else {
      Cs <- Cs.init
    }
    
  }  
  
  return(Cs)
}
