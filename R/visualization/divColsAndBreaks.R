#' @name divColsAndBreaks
#' 
#' @title Calculates breaks for a diverging color palette 
#' 
#' @description see example
#' 
#' @param x values
#' @param th the mid-range value  
#' @param colors colors, a color vector, which has an even length (!)
#' @seealso \code{\link[RColorBrewer]{color}}
#' @examples
#' require(RColorBrewer)
#' vals <- c(rnorm(100, -4, 1), rnorm(100, 1, 1))
#' colors <- brewer.pal(10, "RdBu")
#' 
#' cab <- divColsAndBreaks(vals, th=0, colors=colors)
#' hist(vals, breaks=cab$breaks, col=cab$colors)
#' 
#' @export
divColsAndBreaks <- function(x, th, colors=NULL) {
  
  
  #   ## add option for identical intervals 
  #   
  #   cab <- divColsAndBreaks(vals, th=0, colors=colors, trim="upper")
  #   hist(vals, breaks=cab$breaks, col=cab$colors)
  #   
  #   cab <- divColsAndBreaks(vals, th=0, colors=colors, trim="lower")
  #   hist(vals, breaks=cab$breaks, col=cab$colors)
  #   
  #   cab <- divColsAndBreaks(vals, th=0, colors=colors, trim="upper")
  #   hist(vals, breaks=cab$breaks, col=cab$colors)
  #   
  #   @param trim character. default is \"n\" which means no trimming. \"upper\" (and \"lower\") trims the upper (and lower) values such that they have the same distance. thus the intervals on both sides of the thresholds are equal.
  # 
  #  , trim="n"
  #   if (!(trim=="n"))
  #     x.rng <- range(x)
  #   if (trim=="lower") {
  #     x[x < th-(max(x)-th)] <- th-(max(x)-th)
  #   } else if (trim=="upper") {
  #     x[x > th+(th-min(x))] <-  th+(th-min(x))
  #   } else {
  #     stop("argument \"trim\" must be \"n\", \"lower\", or \"upper\".")
  #   }
  
  
  if (is.null(colors))
    colors <- c("#7f3b08", 
                "#b35806", 
                "#e08214", 
                "#fdb863", 
                "#fee0b6", 
                "#d8daeb", 
                "#b2abd2", 
                "#8073ac", 
                "#542788", 
                "#2d004b")
  
  if (length(colors)/2 == round(length(colors)/2)) {  # even number of colors
    stop("Only odd number of colors supported.")
    vlsPos <- x[x>=th]
    vlsNeg <- x[x<th]
    
    # exclude possibly long tails
    lowerTh <- quantile(vlsNeg, .01, na.rm=TRUE)
    upperTh <- quantile(vlsPos, .99, na.rm=TRUE)
    
    # number of colors
    nColors <- length(colors)
    # number of breaks
    nBrks <- nColors
    
    intervalsPos <- seq(min(vlsPos, na.rm=TRUE), upperTh, length.out=(nBrks/2)+2)
    intervalsPos <- (intervalsPos[-1]+diff(intervalsPos)/2)
    intervalsPos <- intervalsPos[-length(intervalsPos)]
    intervalsNeg <- seq(lowerTh, max(vlsNeg, na.rm=TRUE), length.out=(nBrks/2)+2)
    
  } else {
    vlsPos <- x[x>=th]
    vlsNeg <- x[x<th]
    
    # exclude possibly long tails
    lowerTh <- quantile(vlsNeg, .01, na.rm=TRUE)
    upperTh <- quantile(vlsPos, .99, na.rm=TRUE)
    
    # number of colors
    nColors <- length(colors)
    # number of breaks
    nBrks <- nColors
    
    breaksPos <- seq(th, upperTh, length.out=nBrks/2)
    midsPos <- breaksPos[-length(breaksPos)]+(diff(breaksPos)/2)
    breaksNeg <- seq(lowerTh, th, length.out=nBrks/2)
    midsNeg <- breaksNeg[-1]-(diff(breaksNeg)/2)
    
    mids <- c(midsNeg, th, midsPos)
    
    breaksNeg <- midsNeg+(diff(c(midsNeg, th))/2)
    breaksPos <- midsPos-(diff(c(th, midsPos))/2)
    
    breaks <- c(breaksNeg, breaksPos)
    
    
  }
  return(list(colors=colors, 
              breaks=breaks, 
              mids=mids))
}