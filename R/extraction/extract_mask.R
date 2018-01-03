#' Extract a raster mask from another raster and apply a function
#' 
#' @author Martin Jung
#' @param r A underlying raster
#' @param mask A raster mask
#' @param verbose Should status messages be plotted?
#' @param func The function to be applied (Default: mean) 
#' @import raster, rgdal
#' @return returns a csv with the extracted values
#' @export

extract_mask <- function(r, mask, verbose = F,func = function(x) mean(x,na.rm=T) ){
  require(raster)
  require(rgdal)
  
  if(class(r)!="RasterLayer" | class(mask) != "RasterLayer") stop("Make sure that both layers are raster files.")
  
  # Check if projection is equal
  if(projection(r) != projection(mask)){
    if(verbose) print(paste0(Sys.time()," - ","Reprojecting mask"))
    mask <- raster::projectRaster(from = mask,crs = projection(r),method = 'ngb',over = T,progress="text")
  }
  
  # Then intersect mask and raster
  if(verbose) print(paste0(Sys.time()," - ","Intersecting raster mask"))
  ix <- raster::intersect(r,mask)
  
  # Apply function
  if(verbose) print(paste0(Sys.time()," - ","Apply function to intersected values"))
  out <- func( getValues(ix) )
  
  return(out)
  
}