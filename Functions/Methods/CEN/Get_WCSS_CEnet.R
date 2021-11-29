# Function gives the clustering objective function value
GetWCSS <- function(Xb, Cs){
  
  tot <- sum(sapply(unique(Cs), function(k) sum((Xb[,Cs==k,drop=FALSE]-apply(Xb[,Cs==k,drop=FALSE],1,mean))^2)))
  
  return(tot)
}