# update betas with clusters fixed
# Modified version of code provided by Daniela Witten
UpdateBetasCEnet <- function(X, y, lambda, delta, betas, 
                             Cs, maxiter=50){
  obj <- NULL
  obj <- 1e10; obj.prev <- 1e9; counter <- 0
  objs <- NULL # vector of objective function values for all iterations
  Xbeta <- X%*%betas
  
  unique.Cs <- unique(Cs)
  Ck.all <- sapply(1:length(unique.Cs), function(x)
    length(which(Cs == unique.Cs[x]))) # Number of variables in each cluster; order determined by order of unique(Cs)
 
  # see page 9 of article
  denom <- (1+(lambda*((Ck.all-1)/Ck.all))) # Vector of denominators
  inactive <- NULL
  # until 50 iterations or the change is too small
  while(counter<maxiter && (abs(obj-obj.prev)/abs(obj)) > 1e-5){ 
    # print counter number every 5 iterations 
    #if(counter/5 == floor(counter/5)) cat(counter,fill=FALSE)    
    counter <- counter+1
    obj.prev <- obj
    
    # stops if the objective function output increases instead of decreasing
    if(obj>obj.prev) stop(paste("Objective increase in UpdateBetas function!! Was ", obj.prev, " and now it's ", obj, sep=""))
    # For each cluster k
    for(k in 1:length(unique.Cs)){
      wh <- (Cs==unique.Cs[k]) 
      wh[wh %in% inactive] <- F
      Ck <- Ck.all[k]
      denom.k <- denom[k]
      # predicted values for variables in cluster k
      Xbeta.wh <- X[,wh,drop=FALSE]%*%(betas[wh,drop=FALSE])
      
      # for all variables in cluster k
      for(j in which(wh)){
        Xj <- X[,j]
        Xj2 <- sum(Xj^2) # rjj from page 9 of article (should be 1 if scaled and centered)
        oldcoef <- betas[j]
        # residual vector
        ytildej <- y - (Xbeta - Xj*oldcoef)
        # argument for soft thresholding function (see page 9 of article)
        numer <- sum(Xj*(ytildej)) + (lambda/Ck)*(sum(Xj*Xbeta.wh)-oldcoef*Xj2)  
        newcoef <- Soft(numer,delta/2)/(denom.k*Xj2) 
        # beta update
        betas[j] <- newcoef
        # new predicted vals: Xbeta - Xj*old.coef + Xj*newcoef
        # Xbeta - Xj*old.coef = X_-j*Beta_-j
        Xbeta <- Xbeta + Xj*(newcoef-oldcoef)
        # update predicted vals for vars in cluster: Xbeta.wh - Xj*old.coef + Xj*newcoef
        Xbeta.wh <- Xbeta.wh + Xj*(newcoef-oldcoef)
      }
    }
    # multiply each column of X by its associated beta value
    Xb <- scale(X, center=FALSE, scale=(1/(betas)))
   
    obj.prev <- obj
    # evaluation of the objective function: noncluster part + cluster part
    obj <- sum((y-Xbeta)^2)+delta*sum(abs(betas))+lambda*GetWCSS(Xb,Cs)
    # add most recent objective function value to the vector of values
    objs <- c(objs, obj)
    inactive <- which(betas == 0)
  }
  if(max(diff(objs))> 1e-6){
    print(objs)
    stop("max diff objs increased...")
  }
  return(list(betas=betas,obj=objs))
}