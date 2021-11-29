AssignVarType <- function(p, pk.active, pk.inactive, order.types){
  
  types <- NULL
  active.index <- 1
  inactive.index <- 1
  for (group.index in 1:length(order.types)){
    order.type <- order.types[group.index]
    
    if (order.type == 1){
      types <- c(types, rep("active", pk.active[active.index]))
      active.index <- active.index + 1
    }
    
    if (order.type == 0){
      types <- c(types, rep("inactive", pk.inactive[inactive.index]))
      inactive.index <- inactive.index + 1
    }
    
  }
  
  types <- c(types, rep("inactive", p - length(types)))
  return(types)
}