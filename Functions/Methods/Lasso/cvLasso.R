cvLasso <- function(fold.data, hps, bin.y = F){
  
  
  X <- fold.data$X.norm
  y <- fold.data$y.norm
  y.test <- fold.data$y.test
  X.test <- fold.data$X.test
  
  delta <- hps$lambda
  
  if (bin.y){
    lambda.lasso <- delta*max( abs(t(y - mean(y)*(1-mean(y))) %*% X ) )/ ( nrow(X))
    fam <- "binomial"
  } else {
    lambda.lasso <- delta*max(abs((1/nrow(X))*t(X)%*%y))
    fam <- "gaussian"
  }
  ord.lambda <- order(lambda.lasso, decreasing = T)
  
  fit <- glmnet(x = X, y = y, family = fam, 
                lambda = lambda.lasso[ord.lambda], standardize = F)
  
  
  all.hp <- lapply(1:length(delta), function(x) ExtractLassoOutputs(fit, index = which(ord.lambda == x), X.test, y.test))
  return(all.hp)
}