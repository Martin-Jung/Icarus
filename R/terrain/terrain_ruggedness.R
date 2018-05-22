#' Calculates the terrain ruggedness by calculating the vector ruggedness measure
#' 
#' Description: This tool measures terrain ruggedness by calculating the vector ruggedness measure
#' described in Sappington, J.M., K.M. Longshore, and D.B. Thomson. 2007. Quantifiying
#' Landscape Ruggedness for Animal Habitat Anaysis: A case Study Using Bighorn Sheep in
#' the Mojave Desert. Journal of Wildlife Management. 71(5): 1419 -1426.
#' 
#' @author Martin Jung
#' @author Mark Sappington
#' @import raster
#' 
#' @param by a raster object
#' @param asp terrain aspect object
#' @param slp terrain slope object
#' @param cs The focal matrix size
#' @return returns the terrain ruggedness (VRM)
#' @export

terrain_ruggedness <- function(by,asp,slp,cs){
  require(raster)

  # Get Slope and Aspect rasters
  
  asp.rad <- asp
  slp.rad <- slp
  
  # Calculate x, y, and z rasters
  
  xy <- sin(slp.rad)
  z <- cos(slp.rad)
  
  x <- sin(asp.rad) * xy
  y <- cos(asp.rad) * xy
  
  # Calculate sums of x, y, and z rasters for selected neighborhood size
  
  f <- matrix(1, nrow=cs, ncol=cs)
  
  xb <- focal(x, w=f, fun=function(xi, ...) mean(xi,na.rm=T), pad=T, padValue=NA,na.rm=T)#,
  xb[is.na(xb)] = median(xb,na.rm=T)
  xm  = merge(x,xb)
  xsum <- focal(xm,w=cs,fun=sum)^2
  xsum = mask(xsum,by)
  
  yb <- focal(y, w=f, fun=function(yi, ...) mean(yi,na.rm=T), pad=T, padValue=NA,na.rm=T)#,
  yb[is.na(yb)] = median(yb,na.rm=T)
  ym  = merge(y,yb)
  ysum <- focal(ym,w=cs,fun=sum)^2
  ysum = mask(ysum,by)
  
  zb <- focal(z, w=f, fun=function(zi, ...) mean(zi,na.rm=T), pad=T, padValue=NA,na.rm=T)#,
  zb[is.na(zb)] = median(zb,na.rm=T)
  zm  = merge(z,zb)
  zsum <- focal(zm,w=cs,fun=sum)^2
  zsum = mask(zsum,by)
  
  # Calculate the resultant vector
  R <- sqrt(xsum + ysum + zsum)
  
  max <- cs^2
  
  VRM <- 1-(R/ max)
  VRM
  
}

#' @title Terrain Ruggedness Index
#' @description Implementation of the Riley et al (1999) Terrain Ruggedness Index
#'
#' @param r              raster class object
#' @param s              Scale of window. Must be odd number, can represent 2 dimensions (eg., s=c(3,5) would represent a 3 x 5 window)
#' @param exact          Calculate (TRUE/FALSE) the exact TRI or an algebraic approximation. 
#' @param file.name      Name of output raster (optional)
#' @param ...            Additional arguments passed to writeRaster
#'
#' @return raster class object or raster written to disk
#'
#' @note The algebraic approximation is considerably faster. However, because inclusion of the center cell, the larger the scale the larger the divergence of the minimum value 
#' @note Recommended ranges for classifying Topographic Ruggedness Index
#' @note 0-80 (1) level terrain surface.
#' @note 81-116 (2) nearly level surface.
#' @note 117-161 (3) slightly rugged surface.
#' @note 162-239 (4) intermediately rugged surface.
#' @note 240-497 (5) oderately rugged surface.
#' @note 498-958 (6) highly rugged surface.
#' @note >959 (7) extremely rugged surface. 
#' 
#' @note Depends: raster
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'  
#' @references Riley, S.J., S.D. DeGloria and R. Elliot (1999) A terrain ruggedness index that quantifies topographic heterogeneity, Intermountain Journal of Sciences 5(1-4):23-27.
#
#' @examples 
#'  library(raster)
#'  r <- raster(nrows=180, ncols=360, xmn=571823.6, xmx=616763.6, ymn=4423540, 
#'              ymx=4453690, resolution=270, crs = CRS("+proj=utm +zone=12 +datum=NAD83 
#'              +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
#'    r[] <- runif(ncell(r), 1000, 5000) 
#'    r <- focal(r, w=matrix(1/121,nrow=11,ncol=11)) 
#'     
#'   ( tri.ext <- tri(r) )
#'   ( tri.app <- tri(r, exact = FALSE) )
#'   plot(stack(tri.ext, tri.app))
#'
#' @export
terrain_tri <- function(r, s = 3, exact = TRUE, file.name = NULL, ...) {
  if(length(s) > 2) stop( "Specified window exceeds 2 dimensions")   
  if(length(s) == 1) s = rep(s,2)
  r.sqrt <- function(x) {
    x <- x[x >= 0]
    if(length(x) >= 1) { 
      return(sqrt(x)) 
    } else{
      return(NA)  
    }
  }	
  tri.calc <- function(x) {
    xc <- x[(length(x)+1) / 2]
    x <- x[-(length(x)+1) / 2]
    x <- x[!is.na(x)]
    if(!is.na(xc) & length(x) > 0) {
      x.dev <- vector()
      for(i in 1:length(x)) { x.dev <- append(x.dev, (xc - x[i])^2) }
      return( sqrt(sum(x.dev)) )
    } else {
      return( NA )  
    }
  }	  
  if(exact == TRUE) {  
    if(!is.null(file.name)) {
      return( raster::focal(r, w=matrix(1,s[1],s[2]), fun=tri.calc, na.rm=FALSE,
                            filename = file.name, ...) )
    } else {
      return( raster::focal(r, w=matrix(1,s[1],s[2]), fun=tri.calc, na.rm=FALSE) )
    }  
  } else {
    sx <- raster::focal(r, w = matrix(1,s[1],s[2]), sum)
    e2 <- r * r
    st <- raster::focal(e2, w = matrix(1,s[1],s[2]), sum)
    r2 <- st + (s[1]*s[2]) * e2 - 2 * r * sx
    if(!is.null(file.name)) {
      return( raster::calc(r2, fun= r.sqrt, filename = file.name, ...) )
    } else {
      return( raster::calc(r2, fun= r.sqrt) )
    }
  }
}

