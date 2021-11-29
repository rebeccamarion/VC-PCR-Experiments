GetClustStats <- function(clusters, true.bin){
  
  est.bin <- BinarizeAssignments(clusters)
  
  out <- ClassificationRates(est.bin, true.bin)
  
  return(out)
}