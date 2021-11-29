GetVtVInv <- function(V){
  VtV.diag <- diag(t(V)%*%V)
  VtV.diag[VtV.diag < (.Machine)$double.eps] <- 1
  VtV.inv <- diag(1/VtV.diag)
  
  return(VtV.inv)
}