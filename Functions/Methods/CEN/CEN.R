# tuning params = K, lambda, delta
# X = n x m
# Based on code provided by Daniela Witten
CEN <- function(fold.data, K, lambda, delta, Cs.init = NULL, warm.betas = NULL){
  
  X <- fold.data$X.norm
  y <- fold.data$y.norm
  # check to make sure features and y are centered
  #if(abs(mean(y))>(1e-4) || max(abs(apply(X,2,sum)))>(1e-4)) stop("features not centered properly.")
  obj <- NULL; counter <- 0; 

  # initialize betas as random normal values
  set.seed(10974)
  Cs <- Cs.init
  
  if (is.null(warm.betas)){
    betas <- rep(0, ncol(X))
  } else {
    betas <- warm.betas
  } 
  Xb <- scale(X, center=FALSE, scale=(1/betas))  
  
  # iterate a maximum of 10 times or until the norm of the change in betas is smaller than a certain threshold 
  change <- Inf
  while(counter<10 && change > 1e-5){
    #print(table(Cs))
    counter <- counter+1
    betas.prev <- betas 
    betas <- UpdateBetasCEnet(X, y, lambda, delta, betas, Cs)$beta # uses the betas from the last iteration
    # store the objective function value in a vector
    obj <- c(obj, ObjectiveCEN(X, y, lambda, delta, betas, Cs, Xb))
    Xb <- scale(X, center=FALSE, scale=(1/betas))    
    Cs <- UpdateCs(Xb, K, Cs.init=Cs)
    
    obj <- c(obj, ObjectiveCEN(X, y, lambda, delta, betas, Cs, Xb))
    change <- sum((betas.prev-betas)^2)/(1e-10+sum(betas^2)) 
  }
  
  
  y.pred <- fold.data$X.test%*%betas
 
  if(max(diff(obj))>1e-3) warning("Oh my...... objective increased :(")
  return(list(betas=betas,Cs=Cs,obj=obj, Cs.init = Cs.init, y.pred = y.pred, y.true = fold.data$y.test))
}