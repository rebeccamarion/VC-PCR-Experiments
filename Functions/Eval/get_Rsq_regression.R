GetRsqRegression <- function(y, n, MSEP){
  SS.res <- n*MSEP
  SS.tot <- sum((y - mean(y))^2)
  R.sq <- pmax(1 - (SS.res/SS.tot), 0)
  return(R.sq)
}