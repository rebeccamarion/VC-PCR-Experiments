SummaryStatsByGroup <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      .drop=TRUE, keep.cols = NULL) {
  library(plyr)
  
  
  
    # This does the summary. For each group's data frame, return a vector with
    # N, mean, and sd
  datac <- plyr::ddply(data, groupvars, .drop=T,
                       .fun = function(xx, col) {
                         c(q25    = quantile(xx[[col]], probs = 0.25, na.rm=na.rm),
                           mean   = mean(xx[[col]], na.rm = na.rm),
                           q75    = quantile(xx[[col]], probs = 0.75, na.rm=na.rm),
                           sd     = sd(xx[[col]], na.rm=na.rm),
                           min    = min(xx[[col]], na.rm = na.rm),
                           max    = max(xx[[col]], na.rm = na.rm)
                         )
                       },
                       measurevar
  )
   
    # Rename the "mean" and "untrans" columns   
    datac <- rename(datac, c("mean" = measurevar))
    
    
  
      qt.max <- datac[, "q75.75%"]
      qt.min <- datac[, "q25.25%"] 
      
      datac$qt.max <- qt.max
      datac$qt.min <- qt.min
      
      
      if (mean(is.na(datac[, "sd"])) == 1){
        sd.max <- datac[,measurevar]
        sd.min <- datac[,measurevar]
      } else {
        sd.max <- datac[, measurevar] + datac[, "sd"]
        sd.min <- datac[, measurevar] - datac[, "sd"]
      }
      
      
      
      datac$sd.max <- sd.max
      datac$sd.min <- sd.min
      
      datac <- rename(datac, c("max" = paste0(measurevar, ".max")))
      datac <- rename(datac, c("min" = paste0(measurevar, ".min")))
      datac <- rename(datac, c("qt.max" = paste0(measurevar, ".qt.max")))
      datac <- rename(datac, c("qt.min" = paste0(measurevar, ".qt.min")))
      datac <- rename(datac, c("sd.max" = paste0(measurevar, ".sd.max")))
      datac <- rename(datac, c("sd.min" = paste0(measurevar, ".sd.min")))
      datac <- rename(datac, c("sd" = paste0(measurevar, ".sd")))
      
      
      if (!is.null(keep.cols)){
        keep.cols <- c(groupvars, keep.cols)
        wh.cols <- unique(unlist(lapply(1:length(keep.cols), function(x) grep(keep.cols[x], colnames(datac)))))
        datac <- datac[, wh.cols]
      }
    
  return(datac)
}