GenInactiveVarExpData <- function(n, p, pk.inactive, pk.active, betas, SNR.model, n.rep, cor.x, inactive.type, order.types, wh.grouped = NULL){
  
  require(MASS)
  
  set.seed(92837)
  
  var.types <- AssignVarType(p = p, pk.active = pk.active, pk.inactive = pk.inactive, order.types = order.types)
  block.ids <- AssignBlock(p = p, pk.active = pk.active, pk.inactive = pk.inactive, order.types = order.types, inactive.type = inactive.type)
  var.info <- cbind.data.frame(index = 1:p,
                               var.type = var.types,
                               block.id = block.ids)
  if (!is.null(wh.grouped)){
    if (sum(wh.grouped) == 0){
      var.info$block.id <- 0
    } else {
      var.info$block.id[-wh.grouped] <- 0
    }
  }
  
  n.blocks <- length(unique(var.info$block.id[var.info$block.id != 0]))
  
  
  # Define Sigma_X
  Sigma <- diag(1, nrow = p, ncol = p)
  
  if (n.blocks > 0){
    pairs.list <- lapply(1:n.blocks, function(k) t(combn(var.info[which(var.info[, "block.id"] == k), "index"], 2)))
    for (block.index in 1:n.blocks){
      pairs.cor <- pairs.list[[block.index]]
      for (i in 1:nrow(pairs.cor)){
        row <- pairs.cor[i, 1]
        col <- pairs.cor[i, 2]
        Sigma[row, col] <- Sigma[col, row] <- cor.x
      }
    }
  }
  
  
  # Generate X
  X <- mvrnorm(n = n, mu = rep(0, p), Sigma = Sigma)
  
  # true betas
  betas.x <- rep(0, p)
  K <- length(pk.active)
  active.blocks <- unique(var.info$block.id[var.info$var.type == "active"])
  for (k in 1:K){
    wh.k <- which(var.info$block.id == active.blocks[k])
    betas.x[wh.k[1:(pk.active[k])]] <- betas[k]
  }
  
  # model error
  sd.y <- sqrt(c((t(betas.x)%*%Sigma%*%betas.x)/SNR.model))
  y <- X%*%betas.x + rnorm(n = n, sd = sd.y)
  
  ## Ground-truth clusters
  
  # Definition 1: variable blocks only
  Cs.true1 <- var.info$block.id
  # Definition 2: active in clusters + inactive cluster
  Cs.true2 <- Cs.true1
  Cs.true2[which(var.info$var.type == "inactive")] <- 0
  # Definition 3: active in clusters + inactive in clusters
  Cs.true3 <- Cs.true2
  inactive.blocks <- unique(var.info$block.id[var.info$var.type == "inactive" & var.info$block.id != 0])
  for (k in inactive.blocks){
    wh <- which(var.info$block.id == k & var.info$var.type == "inactive")
    Cs.true3[wh] <- max(Cs.true3) + 1
  }
  
  train.index <- split(1:n, seq(1, n, by=(n/(n.rep))))
  
  train <- lapply(train.index, function(x) list(X = X[x,],y = y[x], betas = betas, support = betas, sigma.x = Sigma, betas.x = betas.x))
 
  clusters <- list(def1 = Cs.true1,
                   def2 = Cs.true2,
                   def3 = Cs.true3)
  output <- list(train = train, clusters = clusters, sd.y = sd.y)
  return(output)
}
