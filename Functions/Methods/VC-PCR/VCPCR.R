VCPCR <- function(fold.data, lambda, delta, Cs.init, X.sd = NULL, alpha = NA, bin.y = F){
  
  
  X <- fold.data$X.norm
  y <- fold.data$y.norm
  var.y <- var(y)
  
  if (!is.na(alpha)){
    if (alpha == 1){
      if (bin.y){
        lambda.lasso <- max( abs(t(y - mean(y)*(1-mean(y))) %*% X ) )/ ( nrow(X))
      } else {
        lambda.lasso <- max((1/nrow(X))*abs(t(X)%*%y), na.rm = T)
      }
      
    } else {
      lambda.lasso <- 1
    }
  }

  
  if (is.null(X.sd)){
    X.sd <- rep(1, ncol(X))
  }
  
  if (bin.y){
    fam <- "binomial"
  } else {
    fam <- "gaussian"
  }
  
  if (is.na(delta)){
    weights <- rep(1, ncol(X))
  } else {
    weights <- glmnet(x = X, y = y, family = fam,
                      alpha = alpha, lambda = delta*lambda.lasso, standardize = F)$beta[, 1]
  }
  
  
  
  # Step 1: W-SOS-NMF
  
  res <- WSOSNMF(X = X, weights = weights, lambda = lambda, Cs.init = Cs.init, X.sd = X.sd)
  
  # Step 2: Prediction
  
  V <- res$V
  M <- X%*%diag(X.sd)%*%V
  
  if (sum(abs(V)) > 0){
    temp <- GetBetasStep2(M = M, V = V, y = y, is.lasso = F, lambda = 0, bin.y = bin.y)
    U <- temp$M
    V <- temp$V
    betas <- temp$betas
    fit <- temp$fit
    Cs <- apply(V, 1, which.max)
    wh.non.zero <- which(apply(V, 1, sum) != 0)
    Cs[-wh.non.zero] <- max(Cs) + 1
  } else {
    U <- as.matrix(rep(0, nrow(X)))
    V <- V[, 1, drop = F]
    Cs <- rep(1, ncol(X))
    betas <- 0
    fit <- NULL
  }
  
  y.pred <- GetPredictionsTwoStep(V = V, Cs = Cs, betas = betas, X.train = X, 
                                X.test = fold.data$X.test, fit = fit, bin.y = bin.y)
  
  return(list(V = V, U = U, Cs = Cs, betas = betas, weights = weights, y.pred = y.pred, y.true = fold.data$y.test))
  
}