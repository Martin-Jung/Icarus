#' Extract a point value from a given file
#' 
#' @author Martin Jung
#' @param sp a spatial points object
#' @param id a id column in sp to save in the output
#' @param r a raster file
#' @param buf Should a buffer be applied to the point. Specify column name (Default=False)
#' @param proj The projection of shapefile. Transform if unequal
#' @param cluster Should parallel processing be used for csv generation (Default = T)
#' @param outputType What kind of output should be generated? csv | asc (Default: CSV)
#' @param output The path where output files are created (Default= Current Folder)
#' @param outputName The filename of the output (Default = basename of folder)
#' @param verbose Should message be printed? (Default=T)
#' @param savemask Should the mask for extraction be saved too (Default = F). If yes writes an asc with id if yes
#' @param func Default function to apply for extraction with buffer
#' @import raster, sp, rgeos
#' @return returns the unified total extent
#' @export

extract_point <- function(sp, id, r, buf=NA, proj = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",cluster=T, output = ".", outputType = "asc",outputName = ".", verbose=T,savemask = F,func = function(x) modal(x,ties="highest",na.rm=T) ) 
{
  # Install packages if not already existing
  if(!require(sp)) install.packages("sp");library(sp)
  if(!require(raster)) install.packages("raster");library(raster)
  if(!require(rgeos)) install.packages("rgeos");library(rgeos)
  if(cluster){
    if(!require(snow)) install.packages("snow");library(snow)
    if(!require(parallel)) install.packages("parallel");library(parallel)
  }
  
  # Set raster options to load more into memory
  #raster::rasterOptions(maxmemory=5e+08,chunksize=5e+07)
  
  # Check inputs
  if(class(sp) != "SpatialPointsDataFrame"){
    stop("Supplied sp object is not a SpatialPointsDataFrame")
  }
  # Is the raster there?
  if(grep("Raster",class(r),ignore.case = T)!=1) stop("Please set a Rasterlayer")
  
  # Is Id column existing in the PointsDataFrame?
  if(!(id %in% names(sp))){
    stop("The specified ID column is not present in the SpatialObject")
  }
  
  # Check projections
  if(is.na(proj4string(sp))) {
    warning("Projection not set! Will use proj")
    proj4string(sp) <- proj
  }
  
  # Check for Buffer if not found
  if(!is.na(buf)) if(!(buf %in% names(sp))) stop("Column with buffer values is not present in the shapefile") else {
    # Create a buffered polygon object 
    if(proj=="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") stop("Set projection is in lat-long. Please use a meter based projection!")
    sp <- spTransform(sp,CRSobj = CRS(proj))
    sp <- rgeos::gBuffer(sp, byid = T, width = sp[[buf]], quadsegs = 50) #buffer the points
  }
  
  # Harmonize projections
  if( proj4string(sp) != proj4string(r) ){
    warning("Projection of input files is not equal. Will try to reproject point file")
    if(is.na(proj4string(r))) stop("CRS of RasterLayer must be set!")
    sp <- spTransform(sp,CRSobj = CRS(proj4string(r)))
  }
  
  # Check if path exists and create
  dir.create(output,showWarnings = F)
  
  # Preperation and variable setting
  old.dir = getwd()
  setwd(output)
  # Multiprocessing of extraction function for csv
  # The R extract function is internally multiprocessed, thus we just need to specify the number of cores
  if(outputType == "csv"){
    
    if(cluster) beginCluster( detectCores()-1 )
    result <- data.frame()
  } 
  
  #### Start the extraction ####
  # Do the loop
  for(i in unique(sp[[id]])) { 
    if(verbose) print(paste0("Now processing ",i))
    sub <- sp[which(sp[[id]]==i),] # make a subset
    
    # Crop the raster to the focal area
    if(class(sub)=="SpatialPolygonsDataFrame"){
      mask <- gUnionCascaded(sub)
      # Clip the raster with the extraction mask
      rt <- try(raster::crop(r,mask,snap="out"))
      if(class(rt)=="try-error") next()
      # Get the outer borders and mask again to correct for on-border pixels
      ex <- raster::boundaries(rt,type="outer",directions=8)
      rt <- raster::crop(r,ex)
      
    } else {
      # Simply crop with the extent
      rt <- try(raster::crop(r,extent(sub),snap="out"))
      if(class(rt)=="try-error") next()
      # Get the outer borders and mask again to correct for on-border pixels
      ex <- raster::boundaries(rt,type="outer",directions=8)
      rt <- raster::crop(r,ex)
    }
    # Enable saving a rasterized mask too.
    if(savemask){
      rma <- raster::rasterize(mask,ex,field=1,background=0)
      writeRaster(rma,filename=paste0(gsub('([[:punct:]])|\\s+','_',i),"-mask", ".asc"),format="ascii",
                  overwrite=T,progress="text",prj=T)
    }
    rm(mask,ex)
    
    # Do the extraction
    if(outputType == "csv") {
      out <- raster::extract(rt, sub,
                             method="bilinear", # take bilinear as majority filter is applied anyway
                             df=T, # Return results as data.frame
                             nl = nlayers(rt), # Get all bands if it is amulterlayer object
                             fun = func # Apply function
      )
      # Rename
      names(out) <- c("ID","values")
      out$ID <- gsub('([[:punct:]])|\\s+','_',i)
      result <- rbind(result,out)
      #write.csv(out,paste0(gsub('([[:punct:]])|\\s+','_',i), ".csv"),row.names=F)
    } else if(outputType == "asc") {
      #       out <- raster::extract(rt, sub,
      #                              method="bilinear", # take bilinear as majority filter is applied anyway
      #                              df=T, # Return results as data.frame
      #                              cellnumbers =T, # Return cellnumbers
      #                              nl = nlayers(rt) # Get all bands if it is amulterlayer object
      #       )
      #       names(out) <- c("ID","cell","values")
      #       out$x <- xyFromCell(rt,out$cell,spatial=F)[,1] # query the original x-value
      #       out$z <- xyFromCell(rt,out$cell,spatial=F)[,2] # query the original y-value
      #       a = rasterize(xyFromCell(rt,out$cell,spatial=T),rt,field=out$values)
      
      # Test if all points fall within the raster
      
      # Write
      writeRaster(rt,filename=paste0(gsub('([[:punct:]])|\\s+','_',i), ".asc"),format="ascii",
                  overwrite=T,progress="text",prj=T)
      
    } else {
      stop(paste0(outputType," not found!"))
    }
    
    # Save output
    
    rm(out,sub,rt)
  } 
  
  # End cluster
  if(outputType == "csv"){
    if(cluster) endCluster()
    write.csv(result,paste0(basename(outputName),".csv"), row.names=F)
  } 
  # Switch back to old folder
  setwd(old.dir)
  
}