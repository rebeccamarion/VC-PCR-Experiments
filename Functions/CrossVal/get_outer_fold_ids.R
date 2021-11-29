GetOuterFoldIDs <- function(n, K.outer){
  
  fold.ids <- suppressWarnings(split(sample(n), seq(1, n, length = K.outer)))
  
  return(fold.ids)
}