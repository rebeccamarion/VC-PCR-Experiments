OLS <- function(X, y){
  betas <- solve(t(X)%*%X)%*%t(X)%*%y
  return(betas)
}