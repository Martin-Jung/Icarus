#' @name raster_scale
#' 
#' @title Band-wise scaling of a \code{rasterBrick} or \code{rasterStack}. 
#' 
#' @description For each band of image x, scale the values 
#' lying between the specified percentiles (rng.in) to lie 
#' within the range rng.out[1] and rng.out[2]. Use cut.tails 
#' to scale only the values between
#' the cut.tails[1]-th and cut.tails[2]-th percentiles. values out of that
#' range will be set to the closest data extreme.
#' stats_from cells from which to derive the statistics.
#' 
#' @param x a \code{rasterBrick} or \code{rasterStack}
#' @param rng.in percentiles to be used to derive the extreme values of the input data   
#' @param rng.out the output values corresponding to input values
#' @param cut.tails ...
#' @param rng.in.from ...
#' @param return_models ...
#' @param filename ...  
#' @param ... arguments that can be passed to \code{\link{writeRaster}} 
#' @return if return_models is FALSE the scaled raster, else a list wiht the raster and the fitted linear models
#' @examples
#' \notrun{
#' r <- brick(system.file("external/rlogo.grd", package="raster"))
#' \\\plot(r[[1]])\
#' r_scaled <- scale_raster(r)
#' plot(r_scaled[[1]])
#' fname <- paste(tempfile() ".tif", sep="")
#' r_scaled <- scale_raster(r, filename=fname,
#'              format="GTiff")
#' }
#' @export
scale_raster <- function(x, rng.in=c(.02, .98), 
                         rng.out=c(0,1), 
                         cut.tails=TRUE, 
                         rng.in.from=NULL, 
                         return_models=FALSE, 
                         filename=NULL, ...
) {
  # scale the values of each band of image x to lie within the range
  # rng.out[1] and rng.out[2]. use cut.tails to scale only the values between
  # the cut.tails[1]-th and cut.tails[2]-th percentiles. values out of that
  # range will be set to the closest data extreme.
  # stats_from cells from which to derive the statistics
  #   x=banana$x
  #   rng.in=c(0, 1) 
  #   rng.out=c(0,1) 
  #   cut.tails=FALSE
  #   rng.in.from=as.numeric(rownames(train_pos))
  
  x.scaled <- x
  fit <- list()
  
  for (ii in 1: nlayers(x) ) {
    cat(ii, "\n")
    vals <- values(x[[ii]])
    if (!is.null(rng.in.from)) {
      mima <- quantile(vals[rng.in.from], 
                       probs=c(rng.in[1], rng.in[2]))
    } else {
      mima <- quantile(vals, probs=c(rng.in[1], rng.in[2]))
    }
    df <- data.frame(x=mima, y=rng.out)
    
    if (cut.tails) {
      vals[vals<mima[1]] <- mima[1]
      vals[vals>mima[2]] <- mima[2]
    }
    
    fit[[ii]] <- lm(y~x, data=df)
    #     plot(df)
    #     abline(fit$coefficients[1], fit$coefficients[2])
    pred <- predict(fit[[ii]], data.frame(x=vals) )
    
    x.scaled <- setValues(x.scaled, pred, layer=ii)
    #    }
    gc()
  }
  
  if (!is.null(filename)) {
    writeRaster(x.scaled, filename=filename, ...)
    rm(x.scaled); gc()
    x.scaled <- brick(filename)
  }
  
  
  if (return_models) {
    names(fit) <- paste( "layer", 1:nlayers(x), sep="_" )
    return( list(x = x.scaled, fit = fit, percentiles.in = mima) )
  } else { 
    return(x.scaled)
  }
}