GetInnerFoldIDs <- function(test.id, n, K.inner){
  row.ids <-  (1:n)[-test.id]
  fold.ids <- suppressWarnings(split(sample(row.ids), seq(1, length(row.ids), length = K.inner)))
  return(fold.ids)
}