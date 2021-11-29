source.files <- list.files(path = function.path, recursive = T)
which.functions <- grep(".R", source.files)
invisible(sapply(source.files[which.functions], 
                 function(x) source(file = paste0(function.path, x))))