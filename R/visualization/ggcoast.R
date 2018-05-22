#' Make a coastline using maptools world map
#' 
#' @author Martin Jung
#' @import maptools
#' @param range A spatial* object from which a bbox can be extracted
#' @param country Optional country (Default = "world")
#' 
#' @return returns a geom_path object for ggtools
#' @export


ggcoast <- function(range,country, ...){
  suppressPackageStartupMessages(library(maptools))
  coast <- map_data("world",
                    xlim=c(bbox(range)[1,1]-1,bbox(range)[1,2]+1),
                    ylim=c(bbox(range)[2,1]-1,bbox(range)[2,2]+1))
  return( geom_path(data=coast,aes(x=long,y=lat,group = group),lwd=.1,...) )
}



