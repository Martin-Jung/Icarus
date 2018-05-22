#' @title Create an empty \code{rasterLayer}.  
#'
#' @description .
#' 
#' @param x a \code{raster*} object corresponding.   
#' @param ... other arguments that can be passed to \code{\link{raster}}
#' @return an empty raster, i.e. all cells are \code{NA}.
#' @examples
#' # ...
#' @importFrom raster raster
#' @keywords raster
#' @examples
#' require(raster)
#' r <- raster(matrix(1:100, 5, 20))
#' emptyraster(r)
#' 
#' @export
emptyraster <- function(x, ...) { # add name, filename, 
  
  emptyraster <- raster(nrows=nrow(x), ncols=ncol(x),
                        crs=x@crs, 
                        ext=extent(x), ...)
  
  return(emptyraster)
}