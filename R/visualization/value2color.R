#' Convert a range of values to a vector of color levels
#' 
#' This function converts a vector of values("z") to a vector of color
#' levels. One must define the number of colors. The limits of the color
#' scale("zlim") or the break points for the color changes("breaks") can 
#' also be defined. when breaks and zlim are defined, breaks overrides zlim.
#' @author Anonymous
#' @param z A vector of values
#' @param zlim A vector limiting the color scale
#' @param col A vector of colors (Heat colors by default)
#' @param breaks number of breaks
#' @return returns the standardises rasters
#' @export


val2col<-function(z, zlim, col = heat.colors(12), breaks){
  if(!missing(breaks)){
    if(length(breaks) != (length(col)+1)){stop("must have one more break than colour")}
  }
  if(missing(breaks) & !missing(zlim)){
    zlim[2] <- zlim[2]+c(zlim[2]-zlim[1])*(1E-3)#adds a bit to the range in both directions
    zlim[1] <- zlim[1]-c(zlim[2]-zlim[1])*(1E-3)
    breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1)) 
  }
  if(missing(breaks) & missing(zlim)){
    zlim <- range(z, na.rm=TRUE)
    zlim[2] <- zlim[2]+c(zlim[2]-zlim[1])*(1E-3)#adds a bit to the range in both directions
    zlim[1] <- zlim[1]-c(zlim[2]-zlim[1])*(1E-3)
    breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1))
  }
  CUT <- cut(z, breaks=breaks)
  colorlevels <- col[match(CUT, levels(CUT))] # assign colors to heights for each point
  return(colorlevels)
}

