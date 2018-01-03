#' Creates aspect in radians with edges fixed
#' 
#' @author Martin Jung
#' @import raster
#' @param by a raster object
#' @param u The unit of the resulting oject (Default: "radians")
#' @return returns the terrain aspect
#' @export

terrain_aspect = function(by,u = 'radians'){
  library(raster)
  aspect.rad <- raster::terrain(by,opt='aspect',unit=u)    
  f <- matrix(1, nrow=5, ncol=5)
  v <- raster::focal(aspect.rad, w=f,
                     fun=function(x, ...) mean(x,na.rm=T), pad=T, padValue=NA,na.rm=T)#,
  v[is.na(v)] = median(aspect.rad,na.rm=T)
  vm = raster::merge(aspect.rad,v)  
  raster::mask(vm,by)
}