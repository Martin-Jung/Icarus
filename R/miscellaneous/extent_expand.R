#' Expand an extent by a certain number
#' 
#' @author Martin Jung
#' @param e an extent object
#' @param f value to increase the extent (Default = 0.1)
#' @return returns the unified total extent
#' @export

extent_expand <- function(e,f=0.1){
  
  xi <- (e@xmax-e@xmin)*(f/2)
  yi <- (e@ymax-e@ymin)*(f/2)
  
  xmin <- e@xmin-xi
  xmax <- e@xmax+xi
  ymin <- e@ymin-yi
  ymax <- e@ymax+yi
  
  return(extent(c(xmin,xmax,ymin,ymax)))  
}
