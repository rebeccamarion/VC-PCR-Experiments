RevertList <- function(ls) { # @Josh O'Brien
  # get sub-elements in same order
  x <- lapply(ls, `[`, names(ls[[1]]))
  # stack and reslice
  apply(do.call(rbind, x), 2, as.list) 
}