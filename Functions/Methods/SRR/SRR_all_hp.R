SRRAllHP <- function(fold.data, hps, Cs.init){
  
  K <- n.unique(Cs.init)
  all.hp <- lapply(1:nrow(hps), function(hp.index) 
    SRR(fold.data = fold.data,
        K = K, 
        lambda = hps$lambda[hp.index], 
        delta = hps$delta[hp.index]))
  
 
  return(all.hp)
  
}