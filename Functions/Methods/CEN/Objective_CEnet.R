# calculate objective function value for CEN
ObjectiveCEN <- function(X, y, lambda, delta, betas, Cs, Xb){
  tot <- sum((y-X%*%betas)^2) + delta*sum(abs(betas)) # not including clustering penalty
  return(tot+lambda*GetWCSS(Xb,Cs)) # including clustering penalty
}