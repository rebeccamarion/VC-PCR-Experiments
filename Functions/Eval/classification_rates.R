ClassificationRates <- function(est.bin, true.bin){
  
  n <- length(est.bin)

  TP <- sum(est.bin*true.bin)
  TN <- sum((1-est.bin)*(1-true.bin))
  FN <- sum((1-est.bin)*(true.bin))
  FP <- sum((est.bin)*(1-true.bin))
  
  sensitivity <- TP/(TP + FN)
  specificity <- TN/(TN + FP)
  F1 <- HarmonicMean(c(sensitivity, 
                       specificity))
  accuracy <- (TP + TN)/n
  
  S.MCC <- (TP + FN)/n
  P.MCC <- (TP + FP)/n
  MCC.num <- ((TP/n) - (S.MCC*P.MCC))
  MCC.denom <- sqrt(P.MCC*S.MCC*(1-S.MCC)*(1-P.MCC))
  MCC.denom[MCC.denom == 0] <- 1
  MCC <- MCC.num/MCC.denom
  
  BA <-  mean(c(sensitivity, specificity))
  
  return(list(sensitivity = sensitivity,
              specificity = specificity,
              F1 = F1,
              MCC = MCC,
              accuracy = accuracy,
              BA = BA))
}