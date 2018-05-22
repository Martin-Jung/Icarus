#' Saves a dbf file to 
#' 
#' @author Martin Jung
#' @param infile The path to a .dbf file
#' @param columnDrop An optional vector of columns to be dropped (Default)
#' @import foreign
#' @return returns the unified total extent
#' @export

dbf2csv <- function(infile, columnDrop=0, ...){
  require(foreign)
  "%+%" <- function(x,y)paste(x,y,sep="")
  if(!file.exists(infile)){stop("Can't find file. Please check path and file name.")}
  dbf <- read.dbf(infile)
  if(columnDrop!=0){
    dbf <- dbf[,-columnDrop] 
  }
  write.csv(dbf,dirname(infile)%+%"/"%+%substr(basename(infile),1,nchar(basename(infile))-4)%+%".csv",row.names=FALSE)
}
