#' Creates the focal roughness of a raster
#' 
#' @author Martin Jung
#' @import raster
#' @param byc a raster object
#' @param w Matrix of weighting values
#' @return returns the terrain roughness
#' @export

roughness = function(byc,w){
  f <- matrix(1, nrow=w, ncol=w)
  rough <- focal(byc, w=f, fun=function(x, ...) max(x,na.rm=T) - min(x,na.rm=T), pad=T, padValue=NA)#, na.rm=TUE)
  mask(rough,byc)
}