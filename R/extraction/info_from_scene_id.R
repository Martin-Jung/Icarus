#' Extracts metadata from a landsat scene ID.
#' 
#' @title Extracts metadata from a landsat scene ID.
#' 
#' @description The metadata is returned as a data frame.
#' @param scene_id identifier of a landsat scene. This it can also be the name of the 
#' downloaded .tar.gz file the .MTL metadata file and include a path.
#' @details \cr
#' LC8 - Landsat 8 OLI and TIRS \cr 
#' LO8 - Landsat 8 OLI only \cr
#' LT8 - Landsat 8 TIRS only \cr
#' LE7 - Landsat 7 ETM+ \cr
#' LT4 - Landsat 4 TM \cr
#' LM1 - Landsat 1 MSS \cr
#' \url{http://landsat.usgs.gov/naming_conventions_scene_ids.php}
#' @examples
#' ls_info_from_scene_id(c( "LC82250602013158LGN00", 
#'                          "LC82250602013158LGN00" ))
#'                                  
#' ls_info_from_scene_id("LC82250602013158LGN00.MTL")
#' ls_info_from_scene_id("LC82250602013158LGN00.tar.gz")
#' ls_info_from_scene_id("D:/LC82250602013158LGN00.tar.gz")
#' @export
info_from_scene_id <-  function (scene_id) {
  
  
  extract_info <- function(scene_id) {
    
    proctype=NA
    if (!is.null(grep(".tar.gz", scene_id)))
      proctype <- ".tar.gz"
    if (!is.null(grep(".MTL", scene_id)))
      proctype <- ".MTL"
    
    bu <- scene_id
    
    
    ### if it is a MTL file get cloud cover
    #     cloud_cover=NA                                                      # added 09-03
    #     try({
    #       browser()
    #       ans <- scan(scene_id, what="character")
    #       cloud_cover <- ans[which(ans=="CLOUD_COVER")+2]
    #     })
    
    
    ### remove path if there is one
    scene_id <- strsplit(scene_id, "/")[[1]]
    scene_id <- scene_id[length(scene_id)]
    
    ### remove extension if there is one 
    scene_id <- strsplit(scene_id, "[.]")[[1]][1]
    # scene_id <- gsub(".tar.gz", "", scene_id)
    
    sensor <- substr(scene_id, 1, 3)
    path <- as.numeric(substr(scene_id, 4, 6))
    row <- as.numeric(substr(scene_id, 7, 9))
    year <- as.numeric(substr(scene_id, 10, 13))
    dayofyear <- as.numeric(substr(scene_id, 14, 16))
    date <- as.Date(dayofyear - 1, origin = paste(year, "-01-01", sep=""))
    month= as.numeric(strsplit(as.character(date), "-")[[1]][2])
    day= as.numeric(strsplit(as.character(date), "-")[[1]][3])
    rtrn <- data.frame(
      path=path, 
      row=row,
      sensor = sensor, 
      year=year, 
      month=month, 
      day=day,
      date = date,
      day_of_year = dayofyear,
      # cloud_cover = cloud_cover,                                       # added 09-03
      # proctype = proctype,
      scene_id = scene_id, 
      stringsAsFactors = FALSE
    )
    return(rtrn)
  }
  
  #browser()
  
  rtrn <- c()
  for (i in 1:length(scene_id))
    rtrn <- rbind(rtrn, extract_info(scene_id[i]))
  
  
  rtrn[order(rtrn$path, rtrn$row, rtrn$year, rtrn$day_of_year), ]
  
  return(rtrn)
}