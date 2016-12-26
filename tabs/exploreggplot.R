library(ggplot2)


ex.ggplot<-function(data,x,y,color,facet_row,facet_col,jitter,smooth){
  
  p <- ggplot(data, aes_string(x=x, y=y)) + geom_point()
  
  if (color != 'None')
    p <- p + aes_string(color=color)
  
  facets <- paste(facet_row, '~', facet_col)
  if (facets != '. ~ .')
    p <- p + facet_grid(facets)
  
  if (jitter)
    p <- p + geom_jitter()
  if (smooth)
    p <- p + geom_smooth()
  
  return(p)
}

  
  
  
