PlotPerfByHP <- function(perf.data, x.var, y.var, col.factor = NULL, 
                         col.factor2, facet.vars, facet.rows){
  
  
  
  if (is.null(col.factor)){
    p1 <- ggplot(perf.data, aes_string(y = y.var, x = x.var))
    p2 <- ggplot(perf.data, aes_string(y = y.var, x = col.factor2))
    
  } else {
    p1 <- ggplot(perf.data, aes_string(y = y.var, x = x.var, 
                                       group = paste0("factor(", col.factor, ")"), 
                                       color =  paste0("factor(", col.factor, ")")))
    p2 <- ggplot(perf.data, aes_string(y = y.var, x = col.factor2, 
                                       group = paste0("factor(", col.factor, ")"), 
                                       color =  paste0("factor(", col.factor, ")")))
  }
  p1 <- p1 +
    geom_line() +
    geom_point() +
    facet_wrap(facets = facet.vars, nrow = facet.rows,
               labeller = "label_both")
  
  p2 <- p2 + 
    geom_line() +
    geom_point() +
    facet_wrap(facets = facet.vars, nrow = facet.rows,
               labeller = "label_both")
  
  return(list(p1 = p1, p2 = p2))
  
}