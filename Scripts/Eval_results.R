
res.index <- 1
ground.truth <- "def2"


# beg <- Sys.time()
cl <- parallel::makePSOCKcluster(names = nb.cores)
doParallel::registerDoParallel(cl, nb.cores)

eval.df <- foreach(res.index = 1:length(res.list),
                   #.errorhandling = "pass", 
                   .packages = c("Matrix"),
                   .combine = "rbind.data.frame") %dopar% {
                  
                     # for (res.index in 1:length(res.list)){
                     #   Cs.true.bin <- BinarizeAssignments(res.list[[res.index]]$Cs.true[[ground.truth]])
                     #   support.true <- res.list[[res.index]]$support.true
                     #   for (hp.index in 1:length(res.list[[res.index]]$res)){
                     #     foo <- GetEvalDf(res = res.list[[res.index]]$res[[hp.index]],
                     #                      fold.data.list = fold.data.list,
                     #                      Cs.true.bin =  Cs.true.bin,
                     #                      support.true = support.true, 
                     #                      res.params = res.list[[res.index]]$res.params[hp.index, , drop = F],
                     #                      ground.truth = ground.truth,
                     #                      real.data = real.data,
                     #                      bin.y = bin.y)
                     #   }
                     #  
                     # }
            
                     Cs.true.bin <- BinarizeAssignments(res.list[[res.index]]$Cs.true[[ground.truth]])
                     support.true <- res.list[[res.index]]$support.true
                     do.call(rbind.data.frame, lapply(1:length(res.list[[res.index]]$res), function(hp.index)
                       
                       GetEvalDf(res = res.list[[res.index]]$res[[hp.index]],
                                 fold.data.list = fold.data.list,
                                        Cs.true.bin =  Cs.true.bin,
                                        support.true = support.true, 
                                        res.params = res.list[[res.index]]$res.params[hp.index, , drop = F],
                                 ground.truth = ground.truth,
                                 real.data = real.data,
                                 bin.y = bin.y,
                                 clust.stats = clust.stats)))
                       
                     }
                     
stopCluster(cl)
closeAllConnections()
# end <- Sys.time()
# end - beg                


