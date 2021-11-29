CENAllHP <- function(fold.data, hps, Cs.init){
  
  
  order.hps <- order(hps$lambda, decreasing = T)

  all.hp <- list()
  for (order.index in 1:length(order.hps)){
    hp.index <- order.hps[order.index]
    if (order.index == 1){
      warm.betas <- rep(0, ncol(fold.data$X.norm))
    } else {
      warm.betas <- all.hp[[order.hps[order.index - 1]]]$betas
    }
    all.hp[[hp.index]] <- CEN(fold.data = fold.data,
                              K = n.unique(Cs.init),
                              lambda = hps$lambda[hp.index],
                              delta = hps$delta[hp.index],
                              Cs.init = Cs.init,
                              warm.betas = warm.betas)
    
  }
  
  return(all.hp)
}