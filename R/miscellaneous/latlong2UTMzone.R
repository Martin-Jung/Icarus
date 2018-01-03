#' Returns the UTM zone for a given combination of lat-long coordinates
#' 
#' @author Martin Jung
#' @param lon = a Longitude coordinate
#' @param lat = a Latitude coordinate
#' @import sp
#' @return returns the respective UTM zone
#' @export


latlong2UTMzone <- function(lon,lat){
  # Normal calculation
  ZoneNumber = floor((lon + 180)/6) + 1
  
  # Special case for higher longitude levels
  if( lat >= 56.0 && lat < 64.0 && lon >= 3.0 && lon < 12.0 ){
    ZoneNumber = 32
  }
  # Special cases for svalbard
  if( lat >= 72.0 && lat < 84.0 ) {
    if  ( lon >= 0.0  && lon <  9.0 ) ZoneNumber = 31 
      else if( lon >= 9.0  && lon < 21.0 ) ZoneNumber = 33
      else if( lon >= 21.0 && lon < 33.0 ) ZoneNumber = 35
      else if( lon >= 33.0 && lon < 42.0 ) ZoneNumber = 37
  }
    
 #Return the result
 return(ZoneNumber)
}

