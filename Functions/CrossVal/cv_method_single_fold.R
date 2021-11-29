cvMethodSingleFold <- function(fold.data, 
                               param.vals,
                               scale.X, 
                               hps, 
                               Cs.true,
                               support.true,
                               Cs.init,
                               method.name,
                               bin.y = F){
  
 
  if (method.name == "CEN"){
    all.hp <- CENAllHP(fold.data = fold.data,
                       hps = hps, 
                       Cs.init = Cs.init)
  }
  
  
  
  if (length(grep("VC-PCR", method.name)) > 0){
    all.hp <- VCPCRAllHP(fold.data = fold.data,
                         hps = hps,
                         Cs.init = Cs.init,
                         bin.y = bin.y)
  }
  
  
  
  
  if (length(grep("CRL", method.name)) > 0){
    
    clust.type <- unlist(strsplit(method.name,  "-"))[2]
    all.hp <- CRLAllHP(fold.data = fold.data,
                        hps = hps,
                       Cs.init = Cs.init,
                        clust.type = clust.type,
                       bin.y = bin.y)
    
  }
  
 
  if (method.name == "SRR"){
    
    all.hp <- SRRAllHP(fold.data = fold.data,
                       hps = hps, 
                       Cs.init = Cs.init)
  }
  
 
  

  res <- all.hp
  rownames(param.vals) <- NULL
  res.params <- cbind.data.frame(method = method.name,
                                 hps,
                                 param.vals)
  
  out <- list(res = res,
              res.params = res.params,
              Cs.true = Cs.true,
              support.true = support.true)
  return(out)
  
}