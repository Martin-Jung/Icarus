#' Allign raster data by bringing it in the same geometry and extend.
#' 
#' If the data is not in the same projection as the template, the allignment
#' will be computed by reprojection only. If the data has already the same
#' projection, the data set will be croped and aggregated prior to resampling
#' in order to reduce computation time.
#' 
#' @author Martin Jung
#' @author Thomas Nauss
#' @import raster
#' @param data raster layer to be resampled
#' @param template raster or spatial data set from which geometry can be extracted
#' @param method method for resampling ("ngb" or "bilinear")
#' @param cl Should cluster computation be used (Default=T)
#'
#' @return raster layer containing geometrically alligned data
#' @export

alignRasters <- function(data, template, method = "bilinear",func = mean,cl = T){
  lib <- c("raster")
  sapply(lib, function(...) stopifnot(require(..., character.only = T)))
  if(cl) beginCluster(parallel::detectCores()-1)
  if(projection(data) == projection(template)){
    data <- crop(data, template, snap = "out")
    if(class(template) == "RasterLayer"){
      if(data@ncols / template@ncols >= 2){
        factor <- floor(data@ncols/template@ncols)
        data <- aggregate(data, fact = factor, fun = func, 
                          expand=TRUE)
      }
      data <- raster::resample(data, template, method = method)
    }
  } else {
    data <- projectRaster(data, template, method = method)
  }
  if(cl) endCluster()
  return(data)
}
