EvalStats <- function(res, fold.data, Cs.true.bin, support.true, method.name, real.data = F, bin.y = F, clust.stats = T){
  
  
  #profvis({
  if (clust.stats){
    Cs <- as.numeric(factor(res$Cs))
  }
  
  betas <- as.numeric(res$betas)
  y.pred <- res$y.pred
  y.true <- res$y.true
  
    
  # Prediction error
  if (bin.y){
      
    log.loss <- LogLoss(y = y.true, probs = y.pred$y.probs)
        class.error <- cbind.data.frame(ClassificationRates(est.bin = y.pred$y.pred, 
                                                            true.bin = y.true))
      
    } else {
        MSEP <- (1/length(y.pred))*sum((y.pred - y.true)^2)
    }
  
  if (length(grep("CRL", method.name)) > 0 | length(grep("PCR", method.name)) > 0){
    
    support <- res$V%*%betas
    
    
  } else {
      
    support <- betas

  }
  
  # Number of variables used overall
  s <- NonzeroCoeffs(support)
  # Number of nonzero coefficients estimated
  s.betas <- NonzeroCoeffs(betas)
  if (bin.y == F){
    # Regression R-squared
    Rsq <- GetRsqRegression(y = fold.data$y.test,
                            n = nrow(fold.data$X.test),
                            MSEP = MSEP)
    
  }
  if (clust.stats){
    # Clustering R-squared
    Rsq.clust <- GetRsqClustering(X = fold.data$X.test, Cs = Cs)
    if (bin.y == F){
      
      # Mean of regression and clustering R-squared
      hmean.Rsq <- HarmonicMean(c(Rsq, Rsq.clust))
      mean.Rsq <- mean(c(Rsq, Rsq.clust))
    }
  }
 
  
  
  
  
  if (bin.y){
    out <- cbind.data.frame(class.error,
                            log.loss = log.loss,
                            
                            s = s,
                            s.betas = s.betas)
    if (clust.stats){
      out <- cbind.data.frame(out, Rsq.clust = Rsq.clust)
    }
  } else {
    
    if (clust.stats){
      out <- cbind.data.frame(MSEP = MSEP,
                              Rsq = Rsq,
                              Rsq.clust = Rsq.clust,
                              hmean.Rsq = hmean.Rsq,
                              mean.Rsq = mean.Rsq,
                              s = s,
                              s.betas = s.betas)
    } else {
      out <- cbind.data.frame(MSEP = MSEP,
                              Rsq = Rsq,
                              
                              s = s,
                              s.betas = s.betas)
    }
    
  }
  
  
  
  if (real.data == F){
    
    # If true support and true clusters known:
    
    # Support recovery
    eval.supp <- cbind.data.frame(GetSupportStats(support = support,
                                                  support.true = support.true))
    colnames(eval.supp) <- paste0(colnames(eval.supp), ".support")
    
    if (clust.stats){
      # Clustering quality
      eval.clust <- cbind.data.frame(GetClustStats(clusters = Cs,
                                                   true.bin = Cs.true.bin))
      out <- cbind.data.frame(out, eval.clust, eval.supp)
      
    } else {
      out <- cbind.data.frame(out,  eval.supp)
    }
    
    
    
    
    
  }
 
  
  #})
  
  return(out)
}
