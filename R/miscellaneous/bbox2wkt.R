#' Converts a bounding box to a Well Known Text polygon
#' 
#' @author Scott Chamberlain
#' @param minx Minimum x value, or the most western longitude
#' @param miny Minimum y value, or the most southern latitude
#' @param maxx Maximum x value, or the most eastern longitude 
#' @param maxy Maximum y value, or the most northern latitude
#' @param all A vector of length 4, with the elements: minx, miny, maxx, maxy
#' @return An object of class charactere, a Well Known Text string of the form
#' 'POLYGON((minx miny, maxx miny, maxx maxy, minx maxy, minx miny))'
#' @export

bbox2wkt <- function(minx=NA, miny=NA, maxx=NA, maxy=NA, bbox=NULL){
  require(assertthat)
  if(is.null(bbox))
    bbox <- c(minx, miny, maxx, maxy)
  
  assert_that(length(bbox)==4) #check for 4 digits
  assert_that(noNA(bbox)) #check for NAs
  assert_that(is.numeric(as.numeric(bbox))) #check for numeric-ness
  paste('POLYGON((', 
        sprintf('%s %s',bbox[1],bbox[2]), ',', sprintf('%s %s',bbox[3],bbox[2]), ',', 
        sprintf('%s %s',bbox[3],bbox[4]), ',', sprintf('%s %s',bbox[1],bbox[4]), ',', 
        sprintf('%s %s',bbox[1],bbox[2]), 
        '))', sep="")
}