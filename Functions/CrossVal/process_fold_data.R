ProcessFoldData <- function(X, y, test.id, scale.X = T, scale.y = T, bin.y = F){
  
  if (is.null(test.id)){
    train.id <- 1:nrow(X)
    test.id <- 1:nrow(X)
  } else {
    train.id <- (1:nrow(X))[-test.id]
  }
  
  y.test <- y[test.id]
  X.test <- X[test.id, ]
  
  # Get mean and sd of features in training set
  
  X.mean <- apply(X[train.id, ], 2, mean) 
  X.sd <- apply(X[train.id, ], 2, sd)
  X.sd.all <- X.sd
  wh.zero.sd <- which(X.sd == 0)
  
  if (length(wh.zero.sd) > 0){
    X.sd[wh.zero.sd] <- 1 # replace with 1 to avoid dividing by 0
  }
  
  if (scale.X == F){
    X.sd <- rep(1, ncol(X))
  }
  
  # Get mean of y in training set
  y.mean <- mean(y[train.id])
  y.sd <- sd(y[train.id])
  
  # Scale and center training set
  X.norm <- scale(X[train.id, ], center = X.mean, scale = X.sd)
  if (bin.y){
    y.norm <- y[train.id]
    y.test <- y.test
  } else {
    y.norm <- scale(y[train.id], center = y.mean, scale = ifelse(scale.y, y.sd, F))[, 1]
    y.test <- scale(y.test, center = y.mean, scale = ifelse(scale.y, y.sd, F))[, 1]
  }
  
  
  # Scale and center test set using means and sds from training set
  X.test <- scale(X.test, center = X.mean, scale = X.sd)
  
  
  return(list(X.norm = X.norm, y.norm = y.norm, 
              X.test = X.test, y.test = y.test,
              X.sd = X.sd.all,
              y.sd = y.sd))
}