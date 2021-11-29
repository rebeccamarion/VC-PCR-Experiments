LogLoss <- function(y, probs, eps = (.Machine)$double.eps){
  
  probs.clipped <- sapply(probs, function(x) ClipProbabilities(probs = x, eps = eps))
  return(-sum((y*log(probs.clipped)) + ((1-y)*log(1-probs.clipped)))/length(y))
}