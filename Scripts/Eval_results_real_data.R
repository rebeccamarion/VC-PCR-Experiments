# res <- inner.all[[1]]$res[[1]]
# res.params <- inner.all[[1]]$res.params
# Cs.true <- inner.all[[1]]$Cs.true
# support.true <- inner.all[[1]]$support.true
# fold.data <- inner.all[[1]]$fold.data

res.index <- 1


ground.truth <- "unknown"

# beg <- Sys.time()
cl <- parallel::makePSOCKcluster(names = nb.cores)

doParallel::registerDoParallel(cl, nb.cores)

eval.df <- foreach(res.index = 1:length(res.list),
                   #.errorhandling = "pass", 
                   .packages = c("Matrix", "clere", "glmnet"),
                   .combine = "rbind.data.frame") %dopar% {
                  
             # for (res.index in 1:length(res.list)){
             #   for (hp.index in 1:length(res.list[[res.index]]$res)){
             #     Cs.true.bin <- NULL
             #     support.true <- NULL
             #     foo <-  GetEvalDf(res = res.list[[res.index]]$res[[hp.index]],
             #                       fold.data = res.list[[res.index]]$fold.data,
             #                       Cs.true.bin =  Cs.true.bin,
             #                       support.true = support.true,
             #                       res.params = res.list[[res.index]]$res.params[hp.index, , drop = F],
             #                       ground.truth = ground.truth,
             #                       real.data = T,
             #                       bin.y = bin.y)
             #   }
             # }
                     #Cs.true.bin <- lapply(res.list[[res.index]]$Cs.true, function(x) BinarizeAssignments(x))
                     Cs.true.bin <- NULL
                     support.true <- NULL
                     do.call(rbind.data.frame, lapply(1:length(res.list[[res.index]]$res), function(hp.index)
                       
                       GetEvalDf(res = res.list[[res.index]]$res[[hp.index]],
                                 fold.data.list = fold.data.list,
                                        Cs.true.bin =  Cs.true.bin,
                                        support.true = support.true, 
                                        res.params = res.list[[res.index]]$res.params[hp.index, , drop = F],
                                 ground.truth = ground.truth,
                                 real.data = T,
                                 bin.y = bin.y,
                                 clust.stats = clust.stats)))
                       
                     }
                     
stopCluster(cl)
closeAllConnections()
# end <- Sys.time()
# end - beg                


