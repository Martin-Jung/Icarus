#' Creates slope in radians with edges fixed
#' 
#' @author Martin Jung
#' @import raster
#' @param by a raster object
#' @param u The unit of the resulting oject (Default: "radians")
#' @return returns the terrain slope
#' @export

slope.rad = function(by, u ='radians'){
  slope.rad <- raster::terrain(by,opt='slope',unit=u)    
  f <- matrix(1, nrow=5, ncol=5)
  v <- raster::focal(slope.rad, w=f, fun=function(x, ...) mean(x,na.rm=T), pad=T, padValue=NA,na.rm=T)#,
  v[is.na(v)] = median(slope.rad,na.rm=T)
  vm = raster::merge(slope.rad,v)  
  raster::mask(vm,by) 
}
