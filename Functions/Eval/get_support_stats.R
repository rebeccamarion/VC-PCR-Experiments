GetSupportStats <- function(support, support.true){
  bin.supp <- abs(sign(support))
  bin.supp.true <- abs(sign(support.true))
  
  stats <- ClassificationRates(bin.supp, bin.supp.true)
  
  
  return(stats)
}