MakeHeatmapGG = function(map.data, melted = F, square = T, name.scale = "", absol = F, title = "", max.min = NULL){
  # map.data = square matrix
  
  require(reshape2)
  require(ggplot2)
  
  if (melted){
    map.melt <- map.data
  } else {
    colnames(map.data) <- 1:ncol(map.data)
    rownames(map.data) <- 1:nrow(map.data)
    map.melt <- melt(map.data)
    map.melt$y <- ncol(map.data) - map.melt$Var2 + 1
    map.melt$x <- map.melt$Var1
  }
  
  
  if (absol){
    map.melt$value <- abs(map.melt$value)
  }
  
 
  if (is.null(max.min) == T){
    
    max.min <- range(map.melt$value)
   
  } else {
    
    max.min <- max.min
    
  }
  
  # n.breaks <- 299
  # breaks <- seq(max.min[1], max.min[2], length = n.breaks)
  # labels <- rep("", length = n.breaks)
  # labels[1] <- format(max.min[1], 1)
  # labels[n.breaks] <- format(max.min[2], 1)
  # pal <- brewer.pal(9, "YlOrRd")
  min.val <- max.min[1]
  max.val <- max.min[2]
  mid.val <- min.val + (max.val - min.val)/2
  
  p <- ggplot(map.melt, aes(x = x, y = y, fill = value)) + 
    geom_tile() + 
    scale_fill_gradient2(high = "#800026", low = "#ffffcc", mid = "#fd8d3c",
                        name = name.scale, midpoint = mid.val) +
    theme_void()
  
  if (square){
    p <- p + coord_fixed()
  }
  
  return(p)
 
}
  
