CRLRegressionStep <- function(fold.data, delta, Cs, bin.y = F){
  
  X <- fold.data$X.norm
  y <- fold.data$y.norm
  
  var.y <- var(y)
  lambda.lasso <- delta*sqrt(var.y)
  
  # Calculate representatives
  V <- MakeBinaryVMatrix(Cs)
  V <- scale(V, center = F, scale = apply(V, 2, sum))
  M <- X%*%V
  #M <- ScaleNonzeroVar(M.tilde, center = T)
  
  # Lasso regression
  res <- GetBetasStep2(M = M, V = V, y = y, is.lasso = T, lambda = lambda.lasso, bin.y = bin.y)
  
  Cs <- apply(res$V, 1, which.max)
  wh.non.zero <- which(apply(res$V, 1, sum) != 0)
  Cs[-wh.non.zero] <- max(Cs) + 1
  
  y.pred <- GetPredictionsTwoStep(V = res$V, Cs = Cs, betas = res$betas, X.train = fold.data$X.norm, X.test = fold.data$X.test, fit = res$fit, bin.y = bin.y)
  

 
  return(list(V = res$V, U = res$M, Cs = Cs, betas = res$betas, y.pred = y.pred, y.true = fold.data$y.test))
}

