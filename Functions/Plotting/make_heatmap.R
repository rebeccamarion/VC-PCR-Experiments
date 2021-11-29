MakeHeatmap <- function(map.data, rnames = NULL, absol = F, title = "", max.min = NULL){
  # map.data = square matrix
  # rnames = seq(1,ncol(map.data))
  # absol = T
  # title = "GFA Factor Loadings"
  
  if (is.null(rnames)){
    rnames <- 1:ncol(map.data)
  }
  
  rownames(map.data) <- rnames
  colnames(map.data) <- rnames
  my_palette <- colorRampPalette(c("blue", "yellow", "green"))(n = 299)
  
  if (is.null(max.min) == T){
    if (absol == T){
      max.min = range(abs(map.data))
    } else {
      max.min = range(map.data)
    }
  } else {
    max.min = max.min
  }
  
  # max.min = c(0.3,max.min[2])
  
  int.length = (max.min[2] - max.min[1])/3 # 3 = number of colors
  intervals = list(red = c(max.min[1],max.min[1]+int.length),
                   yellow = c(max.min[1]+int.length,max.min[1]+int.length*2),
                   green = c(max.min[1]+int.length*2,max.min[2]))
                   
  col_breaks = c(seq(intervals$red[1],intervals$red[2],length=100), # for red
                 seq(intervals$yellow[1],intervals$yellow[2],length=101)[-1],  # for yellow
                 seq(intervals$green[1],intervals$green[2],length=101)[-1]) # for green
  
  if (absol == T){
    heatmap.2(abs(t(as.matrix(map.data))),
              main = paste(title),  # heat map title
              notecol="white",      # change font color of cell labels to black
              density.info="none",  # turns off density plot inside color legend
              trace="none",         # turns off trace lines inside the heat map
              col=my_palette,       # use on color palette defined earlier
              breaks=col_breaks,    # enable color transition at specified limits
              dendrogram="none",# don't draw a row dendrogram
              Rowv = F,
              Colv="Rowv")            # turn off column clustering
  } else {
    heatmap.2(t(map.data),
              main = paste(title),  # heat map title
              notecol="black",      # change font color of cell labels to black
              density.info="none",  # turns off density plot inside color legend
              trace="none",         # turns off trace lines inside the heat map
              col=my_palette,       # use on color palette defined earlier
              breaks=col_breaks,    # enable color transition at specified limits
              dendrogram="none",# don't draw a row dendrogram
              Rowv = F,
              Colv="Rowv",
              cellnote = (map.data))   
  }
  
 
}
  
