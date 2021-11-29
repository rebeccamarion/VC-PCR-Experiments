# Prepare inputs for cv loops

wh.rep <- 1
scale.X <- T
if (real.data == T){
  cv.loop.input <- cvPrepareLoopInputsRealData(exp.data, wh.rep, init.type, 
                                       n.starts, alpha.vals, delta.vals, lambda.vals,
                                       folds.outer, folds.inner, method.name, K.vals)
  file.prefix <- ""
} else {
cv.loop.input <- cvPrepareLoopInputs(exp.data, wh.rep, init.type, 
                                     n.starts, alpha.vals, delta.vals, lambda.vals,
                                     folds.outer, folds.inner, method.name)

}


#### Outer loop results ####

fold.data.outer <- GetFoldData(exp.data = exp.data,
                               wh.rep = wh.rep,
                               cv.loop.input = cv.loop.input,
                               inner.loop = F)
fold.data.list <- fold.data.outer$fold.data.list
fold.data.params <- fold.data.outer$fold.data.params

beg <- Sys.time()
cl <- parallel::makePSOCKcluster(names = nb.cores)

doParallel::registerDoParallel(cl, nb.cores)

outer.cv <- foreach(data.index = 1:nrow(fold.data.params),
                    .packages = c("glmnet", "RColorBrewer", "ClustOfVar", "Rfast", "brglm2")) %dopar% {
              
              
              param.vals <- fold.data.params[data.index, , drop = F]
              print(paste(data.index, " of ", nrow(fold.data.params)))
              return(cvMethodSingleFold(fold.data = fold.data.list[[param.vals$wh.fold.data]],
                                 param.vals = param.vals,
                                 scale.X = T, 
                                 hps = cv.loop.input[[param.vals$data.index]][[param.vals$rep.index]]$hps, 
                                 Cs.true = cv.loop.input[[param.vals$data.index]][[param.vals$rep.index]]$Cs.true,
                                 support.true = cv.loop.input[[param.vals$data.index]][[param.vals$rep.index]]$support.true,
                                 
                                 Cs.init = cv.loop.input[[param.vals$data.index]][[param.vals$rep.index]]$Cs.init.list[[param.vals$init.index]],
                                 method.name = method.name,
                                 bin.y = bin.y))
              
            }
  
stopCluster(cl)
closeAllConnections()
end <- Sys.time()
end - beg


#### Inner loop results ####


fold.data.inner <- GetFoldData(exp.data = exp.data,
                               wh.rep = wh.rep,
                               cv.loop.input = cv.loop.input,
                               inner.loop = T)
fold.data.list <- fold.data.inner$fold.data.list
fold.data.params <- fold.data.inner$fold.data.params


if (!is.null(folds.inner)){
  
cl <- parallel::makePSOCKcluster(names = nb.cores)
#parallel::clusterExport(cl, funx)
doParallel::registerDoParallel(cl, nb.cores)

inner.cv <- foreach(data.index = 1:nrow(fold.data.params),
                    .packages = c("glmnet", "RColorBrewer", "ClustOfVar", "Rfast", "brglm2")) %dopar% {
                      
               
                      param.vals <- fold.data.params[data.index, , drop = F]
                      print(paste(data.index, " of ", nrow(fold.data.params)))
                    
                      return(cvMethodSingleFold(fold.data = fold.data.list[[param.vals$wh.fold.data]],
                                         param.vals = param.vals,
                                         scale.X = T, 
                                         hps = cv.loop.input[[param.vals$data.index]][[param.vals$rep.index]]$hps, 
                                         Cs.true = cv.loop.input[[param.vals$data.index]][[param.vals$rep.index]]$Cs.true,
                                         support.true = cv.loop.input[[param.vals$data.index]][[param.vals$rep.index]]$support.true,
                                         Cs.init = cv.loop.input[[param.vals$data.index]][[param.vals$rep.index]]$Cs.init.list[[param.vals$init.index]],
                                         method.name = method.name,
                                         bin.y = bin.y))
                      
                    }

stopCluster(cl)

} else {
  inner.cv <- NULL
}


#inner.cv <- cvInnerFlattenList(inner.cv)

res.cv.list <- list(inner = inner.cv,
                    fold.data.inner = fold.data.inner,
                    outer = outer.cv,
                    fold.data.outer = fold.data.outer)


file.name <- paste0(data.path,
                    "Results/",
                    file.prefix,
                    "res_cv_list_",
                    method.name,
                    "_init_",
                    init.type,
                    ".RData")
save(res.cv.list, file = file.name)
