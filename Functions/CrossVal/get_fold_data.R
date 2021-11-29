
GetFoldData <- function(exp.data, wh.rep, cv.loop.input, inner.loop = F){
  fold.data.list <- list()
  fold.data.params <- NULL
  for(data.index in 1:length(exp.data)){ # datasets
    for (rep.index in wh.rep){ # repetitions of each dataset
      for (outer.fold.index in 1:length(cv.loop.input[[data.index]][[rep.index]]$fold.ids.outer)){ # outer folds
        
        if (inner.loop){
          inner.fold.indexes <- 1:length(cv.loop.input[[data.index]][[rep.index]]$fold.ids.inner[[outer.fold.index]])
        } else {
          inner.fold.indexes <- 1
        }
        
        for (inner.fold.index in inner.fold.indexes){
          
          for (init.index in 1:nrow(cv.loop.input[[data.index]][[rep.index]]$init.params)){
            
            if (init.index == 1){
              X <- cv.loop.input[[data.index]][[rep.index]]$X
              y <- cv.loop.input[[data.index]][[rep.index]]$y
              if (inner.loop == T){
                fold.ids <- cv.loop.input[[data.index]][[rep.index]]$fold.ids.inner[[outer.fold.index]]
                fold.index <- inner.fold.index
              } else {
                fold.ids <- cv.loop.input[[data.index]][[rep.index]]$fold.ids.outer
                fold.index <- outer.fold.index
              }
              
              fold.data <- ProcessFoldData(X = X, 
                                           y = y, 
                                           test.id = fold.ids[[fold.index]], 
                                           scale.X = scale.X,
                                           bin.y = bin.y)
              fold.data.list <- c(fold.data.list, list(fold.data))
            }
            
            
            params <- cbind(data.index = data.index,
                            rep.index = rep.index,
                            outer.fold = outer.fold.index,
                            inner.fold = ifelse(inner.loop, inner.fold.index, NA),
                            init.index = init.index,
                            K.init = cv.loop.input[[data.index]][[rep.index]]$init.params[init.index, "K"],
                            wh.init = cv.loop.input[[data.index]][[rep.index]]$init.params[init.index, "init"],
                            wh.fold.data = length(fold.data.list))
            fold.data.params <- rbind.data.frame(fold.data.params, params)
          }
          
        }
        
      }
    }
  }
  
  return(list(fold.data.list = fold.data.list, fold.data.params = fold.data.params))
}