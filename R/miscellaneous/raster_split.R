split_raster <- function(infile,outpath,parts=3) {
  ## parts = division applied to each side of raster, i.e. parts = 2 gives 4 tiles, 3 gives 9, etc.
  lib <- c("gdalUtils","parallel","reshape2")
  sapply(lib, function(...) stopifnot(require(..., character.only = T)))
  # Check if infile exists
  stopifnot(file.exists(infile))
  
  filename <- strsplit(basename(infile),".tif")[[1]]
  # Get the dimensions of the file  
  dims <- as.numeric(
    strsplit(gsub('Size is|\\s+', '', grep('Size is', gdalinfo(infile), value=TRUE)), 
             ',')[[1]]
  )
  
  # Generate window frames
  xy <- list()
  # t is nr. of iterations per side
  t <- parts - 1
  for (i in 0:t) {
    for (j in 0:t) {
      # [-srcwin xoff yoff xsize ysize] src_dataset dst_dataset
      srcwin <- paste(i * dims[1]/parts, j * dims[2]/parts, dims[1]/parts, dims[2]/parts)
      xy[[paste0(i,"_",j)]] <- data.frame(infile = infile,srcwin = srcwin, file = paste0(outpath,"/",filename,"_",i,"_",j,".tif"))
    }
  }
  df <- melt(xy)
  
  # Then process per src_win
  cat("Start splitting: ",filename)
  
  # Create a function to split the raster using gdalUtils::gdal_translate
  split <- function(input, outfile, srcwin) {
    gdal_translate(input, outfile, srcwin=srcwin)
  }
  
  # Make a copy for export
  df_org <- df
  # Kick out files already existing
  df <- df[which(!file.exists(as.character(df$file))),]
  
  # Make cluster
  cl <- makeCluster(detectCores()-1)
  clusterExport(cl, c('split', 'df')) 
  clusterEvalQ(cl,library(gdalUtils))
  
  system.time({
    parLapply(cl, seq_len(nrow(df)), function(i) {
      split(df$infile[i], df$file[i], df$srcwin[i])  
    })
  })
  stopCluster(cl)
  cat("\n")
  cat("Done")
  return(df_org)
}

emptyraster <- function(x, ...) {
  emptyraster <- raster(nrows=nrow(x), ncols=ncol(x),
                        crs=x@crs, 
                        ext=extent(x), ...)
  return(emptyraster)
}