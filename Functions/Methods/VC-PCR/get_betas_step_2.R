GetBetasStep2 <- function(M, V, y, is.lasso = T, lambda, bin.y = F){
  nonzero.var <- which(apply(M, 2, sd) != 0)
  if (length(nonzero.var) == 0){
    
    betas <- rep(0, ncol(M))
    fit <- NULL
    
  } else {
    M <- M[, nonzero.var, drop = F]
    V <- V[, nonzero.var, drop = F]
    
      if (bin.y){
        fam <- "binomial"
      } else {
        fam <- "gaussian"
        fit <- NULL
      }
    
    added.col <- F
    if (ncol(M) == 1){
      M <- cbind(M, 0)
      added.col <- T
    }
    
   
    if (bin.y & lambda == 0){
     
      fit <- glm(y ~ ., data = cbind.data.frame(M, y = y), family=binomial(link = "logit"),
                 method = "brglmFit",
                 type = "AS_mean", maxit = 1000)
      fit$betas <- fit$coefficients[-1]
     
    } else {
      fit <- glmnet(x = M, y = y, family = fam,
                    alpha = as.numeric(is.lasso), lambda = lambda, standardize = F)
    }
    
    
    
    if (added.col){
      M <- M[, -ncol(M), drop = F]
    }
    
    betas <- as.matrix(fit$beta)
    betas <- betas[, ncol(betas)]
  }
  return(list(M = M, V= V, betas = betas, fit = fit))
}