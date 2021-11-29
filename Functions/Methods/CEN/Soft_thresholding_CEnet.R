Soft <- function(a, b){
  
    out <- sign(a)*pmax(abs(a)-b,0)
  
  return(out)
}