GetEvalDf <- function(res, fold.data.list, Cs.true.bin, support.true, res.params, ground.truth, 
                      real.data = F, bin.y = F, clust.stats = T){
  
  require(Matrix)
  
  method.name <- unique(res.params$method)
  eval <- EvalStats(res = res, 
                    fold.data = fold.data.list[[res.params$wh.fold.data[1]]],
                    Cs.true.bin = Cs.true.bin, 
                    support.true = support.true,
                    method.name = method.name,
                    real.data = real.data,
                    bin.y = bin.y,
                    clust.stats = clust.stats)
  
  eval.df <- cbind.data.frame(res.params, eval, ground.truth = ground.truth)
  return(eval.df)
}