NonzeroCoeffs <- function(betas){
  s <- sum(sign(betas) != 0)
  return(s)
}