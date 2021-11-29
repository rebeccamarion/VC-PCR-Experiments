cvRepLoopInputs <- function(exp.data, data.index, rep.index, init.type, 
                            n.starts, alpha.vals, delta.vals, lambda.vals,
                            folds.outer, folds.inner, method.name, ground.truth = "def2"){
  
  X <- exp.data[[data.index]]$train[[rep.index]]$X
  y <- exp.data[[data.index]]$train[[rep.index]]$y
  Cs.true <- exp.data[[data.index]]$clusters
  support.true <- exp.data[[data.index]]$train[[1]]$betas.x
  K.vals <- n.unique(Cs.true[[ground.truth]])
  K.vals <- (K.vals - 1):(K.vals + 1)
 
  
  if (init.type == "true"){
    Cs.init <- Cs.true[[groud.truth]]
  } else {
    Cs.init <- NULL
  }
  if (init.type == "none"){
    Cs.init <- init.type
  }
  
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
  
  hps <- expand.grid(alpha = alpha.vals, 
                     lambda = lambda.vals,
                     delta = delta.vals)
  
  
  set.seed(8214)
  if (is.null(Cs.init)){
    
    init.params <- expand.grid(init = 1:n.starts,
                               K = K.vals)
    Cs.init.list <- lapply(1:nrow(init.params), function(x) GenRandomClusters(K = init.params$K[x],
                                                                              p = ncol(X)))
    
    
  } else {
    
    Cs.init.list <- list(Cs.init)
    
    init.params <- cbind.data.frame(init = NA,
                                    K = NA)
  }
  
  
  rep.loop.list <- list()
  rep.loop.list$X <- X
  rep.loop.list$y <- y
  rep.loop.list$Cs.true <- Cs.true
  rep.loop.list$support.true <- support.true
  rep.loop.list$hps <- hps
  rep.loop.list$Cs.init.list <- Cs.init.list
  rep.loop.list$init.params <- init.params
  rep.loop.list$fold.ids.outer <- fold.ids.outer
  rep.loop.list$fold.ids.inner <- fold.ids.inner
  
  return(rep.loop.list)
  
}