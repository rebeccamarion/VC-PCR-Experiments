VCPCRAllHP <- function(fold.data, hps, Cs.init, bin.y = F){
  
  
 
  X.sd <- rep(1, ncol(fold.data$X.norm))

  
  all.hp <- lapply(1:nrow(hps), function(hp.index) 
    VCPCR(fold.data = fold.data,
          lambda = hps$lambda[hp.index], 
          delta = hps$delta[hp.index],
          X.sd = X.sd,
          Cs.init = Cs.init,
          alpha = hps$alpha[hp.index],
          bin.y = bin.y))
  

  return(all.hp)
}