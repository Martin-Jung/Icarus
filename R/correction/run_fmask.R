#'  Run fmask on a downloaded .tar.gz landsat data set
#' @title Run fmask
#' 
#' @description ...
#' @param data the filename of a downloaded .tar.gz landsat data set (with extension!) or the folder with the unpackaged data
#' @param fmask path and name of the fmask executable
#' @param out_folder output folder, if NULL the file is written to the folder of the data_file
#' @param out_basename output file, if NULL the file name of the input file is used. do not specify an extension
#' @param out_format convert the file to another raster file type (\raster{\link[raster]{writeFormat}}).
#' If ENVI is given the file kept as is.
#' @param out_folder_extracted where store the extracted landsat data
#' @param delete_archive delete the archive after extraction
#' @param overwrite logical, if \code{TRUE} overwrite the file if it already exists
#' @export
run_fmask <- function(data, fmask, out_folder=NULL, out_basename=NULL, out_format="ENVI", 
                     out_folder_extracted=NULL) {
  
  if (length(grep(".tar.gz", data))==1) {
    if (is.null(out_folder_extracted))
      out_folder_extracted <- dirname(data)
    untar(data, compressed = 'gzip', exdir = out_folder_extracted)
    extracted <- TRUE
    data <- out_folder_extracted
  }
  
  ### run fmask
  shell(paste("cd /D", data, "&", fmask), wait=TRUE)
  
  ### get the filename 
  in_hdr <- dir(data, pattern=".hdr", full.names=TRUE)
  
  ### get default dir and file name
  if (is.null(out_folder))
    out_folder <- dirname(in_hdr)
  if (is.null(out_basename))
    out_basename <- gsub(".hdr", "", basename(in_hdr))
  
  out <- paste(out_folder, "/", out_basename, sep="")
  
  dir.create(out_folder, showWarnings=FALSE)
  
  if (out_format!="ENVI") {
    r <- raster(gsub(".hdr", "", in_hdr))
    writeRaster(r, filename=out, format=out_format)
    unlink(in_hdr)
    unlink(gsub(".hdr", "", in_hdr))
  } else {
    out_hdr <- paste(out, ".hdr", sep="")
    if (!(in_hdr==out_hdr)) {
      file.remove(from=in_hdr, to=paste(out, ".hdr", sep=""))
      file.remove(from=gsub(".hdr", "", in_hdr), to=out)
    }
  }
  
}
