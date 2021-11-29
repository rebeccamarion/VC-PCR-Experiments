SoftThreshSRR <- function(x, thresh){
  sign(x)*pmax(abs(x) - thresh, 0)
}
