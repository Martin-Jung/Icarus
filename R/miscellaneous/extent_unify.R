#' Unify extent of a rasterstack
#' 
#' @author Martin Jung
#' @param rList a list of raster objects

#' @return returns the unified total extent
#' @export
#' 

max_extent <- function(rlist){
# given list of rasters
# returns union of extent
xmin=min(sapply(rl,FUN=function(x){extent(x)@xmin}))
xmax=max(sapply(rl,FUN=function(x){extent(x)@xmax}))
ymin=min(sapply(rl,FUN=function(x){extent(x)@ymin}))
ymax=max(sapply(rl,FUN=function(x){extent(x)@ymax}))

extent(c(xmin,xmax,ymin,ymax))
}
