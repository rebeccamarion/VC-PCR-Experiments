AssignBlock <- function(p, pk.active, pk.inactive, order.types, inactive.type){
  
  block <- NULL
  active.index <- 1
  inactive.index <- 1
  block.index <- 1
  for (group.index in 1:length(order.types)){
    order.type <- order.types[group.index]
    
    if (order.type == 1){
      block <- c(block, rep(block.index, pk.active[active.index]))
      active.index <- active.index + 1
      block.index <- block.index + 1
    }
    
    if (order.type == 0){
      
      if (inactive.type == "corr active and inactive"){
        
        if (order.types[group.index - 1] == 1){
          block.index <- block.index - 1
        } 
        
        block <- c(block, rep(block.index, pk.inactive[inactive.index]))
        inactive.index <- inactive.index + 1
        block.index <- block.index + 1
        
      }
      
      if (inactive.type == "corr inactive"){
        
        block <- c(block, rep(block.index, pk.inactive[inactive.index]))
        inactive.index <- inactive.index + 1
        block.index <- block.index + 1
        
      }
      
      if (inactive.type == "uncorrelated"){
        block <- c(block, rep(0, pk.inactive[inactive.index]))
        inactive.index <- inactive.index + 1
        
      }
      
     
    }
    
  }
  
  block <- c(block, rep(0, p - length(block)))
  return(block)
}