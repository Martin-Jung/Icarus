#' Standardise all rasters to the same extent
#' 
#'   given a list of rasters this function will make all 
#'   rasters conform to the origin of the first in the list
#' @author Martin Jung
#' @import raster
#' @param rasters a raster stack object
#' @param method The resampling method (Default: "bilinear")
#' @return returns the standardises rasters
#' @export

standardiseRasters <- function(rasters,method='bilinear'){

  ol <- sapply(rasters,origin)
  i <- ol[1,] != origin(rasters[[1]])[1]
  # Text progressbar
  pb = txtProgressBar(min = 0, max = nlayers(rasters), initial = 0)
  ii <- 1
  for(r in rasters[i]){
    setTxtProgressBar(pb,ii)
    r <- resample(r,rasters[[1]],method=method)
    rasters[i][ii] <- r
    ii <- ii+1
  }
  return(rasters)
}
