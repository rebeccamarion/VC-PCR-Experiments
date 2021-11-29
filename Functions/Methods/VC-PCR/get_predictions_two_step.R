GetPredictionsTwoStep <- function(V, Cs, betas, X.train, X.test, fit, bin.y = F){
  require(glmnet)
  require(brglm2)
  if (is.null(fit)){
    y.pred <- rep(0, nrow(X.test))
    if (bin.y){
    y.pred <- cbind.data.frame(y.pred = y.pred, y.probs = y.pred)
    }
    
  } else {
    
    M.test <- as.matrix(X.test%*%V)
    if (ncol(M.test) < length(betas)){
      M.test <- cbind(M.test, 
                      matrix(0, 
                             nrow = nrow(M.test), 
                             ncol = length(betas) - ncol(M.test)))
    }
    if (bin.y){
      # preds = log(1-p)/log(p)
      # if p > 0.5 --> 1 
      # preds > 1 if p > 0.5
      # https://glmnet.stanford.edu/articles/glmnet.html#logistic-regression-family-binomial-
      if (class(fit)[1] == "glm"){
        preds <- as.matrix(predict.glm(object = fit, newdata = cbind.data.frame(M.test, y = rep(1, nrow(M.test))), type = "response"))
        
      } 
      if (class(fit)[1] == "brglmFit"){
        preds <- as.matrix(predict.glm(object = fit, newdata = cbind.data.frame(M.test, y = rep(1, nrow(M.test))), type = "response"))
        
      }
      if ("glmnet" %in% class(fit)){
        preds <- as.matrix(predict(fit, newx = M.test, type = "response", family = "binomial"))
        
      }
    
        
      preds <- preds[, ncol(preds)]
      y.pred <- as.numeric(preds > 0.5)
      y.pred <- cbind.data.frame(y.pred = y.pred, y.probs = preds)
      
    } else {
      
      y.pred <- as.numeric(M.test%*%betas)
      
    }
  }
  
  return(y.pred)
}