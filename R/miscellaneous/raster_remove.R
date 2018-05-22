#' Truly delete raster and associated temporary file
#'
#' @author Martin Jung
#' @description Really Remove Raster Temp files. This function checks the path of a raster object and removes it from disk as well as from the R workspace.
#' @param x A raster* object. 
#' @param check Security check enabled
#' @export

rmRaster <- function(x,check=T){                                                                          
  if(check) ask <- readline("Are you sure to delete the raster? (y)") else ask="y"
  if(ask=="y"){
    if(grepl("Raster",class(x))&fromDisk(x)==T){
      if(class(x)=="RasterStack") file.remove( x[[1]]@file@name ) else
        file.remove(x@file@name)                                                                                                                                  
      rm(x)                                                                                                                                                                                  
    }
  }
} 