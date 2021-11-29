cvRepLoopInputsLasso <- function(sim.data, data.index, rep.index, lambda.vals,
                            folds.outer, folds.inner, method.name, ground.truth = "def2"){
  
  X <- sim.data[[data.index]]$train[[rep.index]]$X
  y <- sim.data[[data.index]]$train[[rep.index]]$y
  support.true <- sim.data[[data.index]]$train[[1]]$betas.x
  
  set.seed(922)
  # Split data row indexes randomly into K.outer folds
  fold.ids.outer <- GetOuterFoldIDs(n = nrow(X), K.outer = folds.outer)
  if (!is.null(folds.inner)){
    # Split training data for each outer fold into K.inner folds
    fold.ids.inner <- lapply(fold.ids.outer, function(test.id) 
      GetInnerFoldIDs(test.id = test.id, 
                      n = nrow(X), 
                      K.inner = folds.inner))
  }
  
  hps <- expand.grid(lambda = lambda.vals)

  
  rep.loop.list <- list()
  rep.loop.list$X <- X
  rep.loop.list$y <- y
  rep.loop.list$support.true <- support.true
  rep.loop.list$hps <- hps
  rep.loop.list$fold.ids.outer <- fold.ids.outer
  rep.loop.list$fold.ids.inner <- fold.ids.inner
  
  return(rep.loop.list)
  
}