#' Get information from Landsat 8 or Hyperion standard level 1B/T filename.
#' 
#' If the data is not in the same projection as the template, the allignment
#' will be computed by reprojection only. If the data has already the same
#' projection, the data set will be croped and aggregated prior to resampling
#' in order to reduce computation time.
#' 
#' @author Thomas Nauss
#' @import raster
#' @param filepath path and filename to the landsat band file (not the metadata)
#' @param sensor sensor name (i.e. landsat, hyperion) (Default: Landsat)
#' @param method method for resampling ("ngb" or "bilinear")
#'
#' @return vector containing band as character, metadata filepath
#' @export
#' 
L8_getInfoFromLevel1Name <- function(filepath, sensor){
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#  Please send any comments, suggestions, criticism, or (for our sake) bug
#  reports to admin@environmentalinformatics-marburg.de

if(sensor == "landsat"){
  pos <- gregexpr(pattern ='_B', 
                  sub("(.+)[.][^.]+$", "\\1", 
                      basename(filepath)))[[1]][1]
  band <- substr(basename(filepath), pos + 2, 
                 nchar(sub("(.+)[.][^.]+$", "\\1", basename(filepath))))
  meta.filepath <- paste0(dirname(filepath), "/", 
                          substr(basename(filepath), 1, pos), 
                          "MTL.txt")
} else if(sensor == "hyperion"){
  pos <- gregexpr(pattern ='_B', 
                  sub("(.+)[.][^.]+$", "\\1", 
                      basename(filepath)))[[1]][1]
  band <- substr(basename(filepath), pos + 2, pos +4)
  meta.filepath <- paste0(dirname(filepath), "/", 
                          substr(basename(filepath), 1, pos), 
                          "MTL_L1T.txt")
}
result <- c(band, meta.filepath)
attr(result, "Info") <- c("Band", "MetaFile")
return(result)
}