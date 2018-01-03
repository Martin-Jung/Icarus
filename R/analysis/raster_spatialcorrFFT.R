#' Calculates the full spatial autocorrelation on a raster using fft.
#'
#' @description Applies the Wiener-Khinchin theorem to extract spatial autocorrelation using Fast Fourier Transform techniques.  This results in an extremely fast way to calculate a complete correlogram (correlation as a function of distance) for a raster image.  
#' @param x A raster* object. Missing values are indicated by NA.
#' @param file File to write results to as in writeRaster.  If NULL a temporary file is written as in the raster package.
#' @return The spatial autocorrelation matrix
#' @author Adam M. Wilson
#' @references \url{en.wikipedia.org/wiki/WienerKhinchin_theorem}
#' @references Xianlin Ma, Tingting Yao, A program for 2D modeling (cross) correlogram tables using fast Fourier transform, Computers & Geosciences, Volume 27, Issue 7, August 2001, Pages 763-774, ISSN 0098-3004, \url{http://dx.doi.org/10.1016/S0098-3004(01)00007-3}.
#' @references \url{http://www.johnloomis.org/ece563/notes/freq/autoself/autoself.htm}
#' @references \url{http://www.seas.upenn.edu/~ese502/NOTEBOOK/Part_II/4_Variograms.pdf}

raster_spatialcorrFFT=function(x,padlongitude=T,verbose=T,...){
  # dimensions of raster
  nr <- nrow(x)
  nc <- ncol(x)
  ## Images must be padded to size 2N-1 by 2M-1
  # find the closest multiple of 8 to obtain a good compromise between
  # speed (a power of 2) and memory required
  nr2=ifelse(nr<5,5,ceiling((2*nr-1)/8)*8)
  nc2=ifelse(!padlongitude,nc,ifelse(nc<5,5,ceiling((2*nc-1)/8)*8))  
  ## create a new extent by padding to the right and below with 0s
  resx=res(x)
  extx=extent(x)
  extx2=extent(c(xmin=extx@xmin,xmax=extx@xmax+(resx[1]*(nc2-nc)),ymin=extx@ymin-(resx[2]*(nr2-nr)),ymax=extx@ymax))
  if(verbose) print("Padding the array")
  rx=extend(x,extx2,val=0)
  ## convert to matrix
  x1=as(rx,"matrix")
  # make an indicator matrix with 1's for all data values & O's for missing values
  if(verbose) print("Identifying missing data")
  xnull=as(extend(!is.na(x),extx2,val=0),"matrix")
  # in data matrix, replace missing values by 0;
  x1[xnull==0]=0
  
  if(verbose) print("Running the initial FFTs")  
  fx1=fft(x1)  # fourier transform of xl
  #fx1_x1=fft(x1*x1)    # fourier transform of x1*x1
  
  fxnull=fft(xnull)  # fourier transform of the indicator matrix
  # compute number of pairs at all lags
  if(verbose) print("Computing the number of observations at each lag")
  nobs=round(Re(ifft(Conj(fxnull)*fxnull)))
  mnobs=nobs
  mnobs[mnobs<1]=1
  ## compute the correlogram
  m1=Re(ifft(Conj(fx1)*fxnull))/mnobs
  m2=Re(ifft(Conj(fxnull)*fx1))/mnobs
  g=Re(ifft(Conj(fx1)*fx1)/mnobs-m1*m2)
  
  if(verbose) print("Shifting the FFT array")
  
  nobs2=fftshift2(nobs)      
  g2=fftshift2(g)*10      
  
  ## get distances in km
  if(verbose) print("Calculating distances")
  d1=acorr_dist(rx)
  
  # convert back to raster
  if(verbose) print("Convert back to raster* format")
  g3=d1;values(g3)=g2
  nobs3=d1;values(nobs3)=log10(nobs2)
  acor=stack(g3,nobs3,d1)
  names(acor)=c("acor","nobs","dist")
  #  if(exists("filename",inherits=F)) acor2=writeRaster(acor,...)
  if(verbose) print("Cleaning up")
  rm(x,rx,x1,fx1,fxnull,m1,m2,g,nobs,g3,d1,nobs3);gc()
  return(acor)
}

#' Rearranges FFT output to put zero-distance in center of image
#' @description Rearranges outputs from fft to move the zero-frequency component to the center of the matrix.  This is useful to visualize a Fourier transform with the zero-frequency component in the center of the image.
#' @param x A matrix returned from \code{fft()}
#' @return The transformed matrix
#' @references Adapted from GNU Octave's fftshift function (\url{http://octave.sourceforge.net/octave/function/fftshift.html} \url{http://hg.savannah.gnu.org/hgweb/octave/file/914c0b103a3d/scripts/signal/fftshift.m#l76})
#' @references \url{http://www.mathworks.com/help/matlab/ref/fftshift.html}
#' @references \url{http://stackoverflow.com/questions/5735720/effcient-way-to-do-fft-shift-in-matlab-without-using-fftshift-function}
#' @references The waved package (\url{http://cran.r-project.org/web/packages/waved/index.html}) has a 1-dimensional fftshift function.

fftshift2=function(x){
  nd = length(dim(x))
  sz = dim(x)
  sz2 = ceiling(sz/2);
  idx = list()
  for (i in 1:nd)  idx[[i]] = c((sz2[i]+1):sz[i], 1:sz2[i])
  retval = x[idx[[1]],idx[[2]]];
  return(retval)
}

#' Calculates the normalized inverse fft of an array
#'
#' @description Convenience function to calculates the normalized inverse fft of an array (like in MatLab)
#' @param x An array as used by fft()
#' @return The normalized inverse fft of x
#' @references \url{http://stackoverflow.com/questions/8162562/is-there-a-package-in-r-that-gives-normalized-inverse-fft}


ifft <- function(x) { fft(x, inverse=TRUE ) / length(x) }

#' Calculates the distance of each cell in a raster to the center pixel to faciltate comparison of results from \code{acorr} as a function of distance.
#'
#' @param x A raster* object
#' @return A raster object showing the distances from the center pixel
#' 

acorr_dist=function(x){
  x2=acorr_center(x)
  ## distance units need to be updated from raster somehow...
  ## currently it assumes x is in lat-lon and so distance returns meters
  ## divide by 1000 to km
  dist=distance(x2)/1000
  return(dist)
}

#' Finds the center pixel to faciltate comparison of results from \code{acorr}.
#'
#'
#' @description Performs the following functions. 1) create an empty raster to hold the output, 2) fill the raster with NAs, 3) Set the center pixel equal to 1.
#' This faciliates using other functions in the \pkg{raster} package that calculate distance and direction to the closest non-NA pixel.  This is useful because the output of \code{\link{acorr}} returns the spatial autocorrelation for all directions simutaneously and thus needs to be linked to distance and direction to generate a correlogram.
#' @param x A raster* object
#' @return A raster object with all cells NA except the center pixel.
#' 

acorr_center=function(x){
  center = ceiling(dim(x)/2)
  # Create an empty raster to hold the output
  center2=x#raster(x,xmn=-nc/2,xmx=nc/2,ymn=-nr/2,ymx=nr/2)  
  # Fill the raster with NAs
  values(center2)=NA
  # Replace the center pixel with a one  
  nre=ifelse(nrow(x)/2==round(nrow(x)/2),1,0)
  nce=ifelse(ncol(x)/2==round(ncol(x)/2),1,0)
  center2[center[1]+nre,center[2]+nce]=1
  return(center2)
}

#' Calculates correlogram values on a raster using fft.
#'
#' @description Applies the Wiener-Khinchin theorem to extract spatial autocorrelation using Fast Fourier Transform techniques.  This results in an extremely fast way to calculate a complete correlogram (correlation as a function of distance) for a raster image.  
#' @param x A raster* object. Missing values are indicated by NA.
#' @author Adam M. Wilson
#' @param maxdist Maximum distance (in km) to include in correlogram table.  All possible distances are calculated using FFT, then trimmed to this value.
#' @return The table of distances
#' @references \url{en.wikipedia.org/wiki/WienerKhinchin_theorem}
#' @references Xianlin Ma, Tingting Yao, A program for 2D modeling (cross) correlogram tables using fast Fourier transform, Computers & Geosciences, Volume 27, Issue 7, August 2001, Pages 763-774, ISSN 0098-3004, \url{http://dx.doi.org/10.1016/S0098-3004(01)00007-3}.
#' @references \url{http://www.johnloomis.org/ece563/notes/freq/autoself/autoself.htm}
#' @references \url{http://www.seas.upenn.edu/~ese502/NOTEBOOK/Part_II/3_Spatially_Dependent_Random_Effects.pdf}


raster_correlogramTable=function(x,maxdist=1500,verbose=F){
  ## run the autocorrelation function and write out the output raster
  wrapglobe=!(extent(x)@xmin==-180&extent(x)@xmax==180)  #does this region wrap the globe?
  ac=raster_spatialcorrFFT(x,padlongitude=wrapglobe,verbose=verbose)
  ## build the table of values to construct the correlograms
  if(verbose) print("Extracting autocorrelation values for table and filtering to maxdist")
  ftd=rbind.data.frame(
    data.frame(values=values(ac[["acor"]])/10,dist=values(ac[["dist"]]),n=values(ac[["nobs"]]))
  )
  ## filter to a reasonable distance, signal gets noisy above a few thousand km due to sparse measurements
  ftd <- filter(ftd, dist <= maxdist)
  ## normalize the covariogram to a correlogram by dividing value at lag=0
  ftd$values=ftd$values/max(ftd$values[which.min(ftd$dist)])
  ## round to approximate resolution of raster (in km)
  rasres=round(rasterRes(x))
  rasbins=c(-1,seq(0,max(ftd$dist)+rasres,by=rasres))
  ftd$dist=as.numeric(as.character((cut(ftd$dist,rasbins,labels=rasres/2+rasbins[-length(rasbins)]))))
  ## take mean by distance bin
  if(verbose) print("Summarizing by distance bin")
  ftd2 <- group_by(ftd, dist)
  ftd2 <- summarise(ftd2,
                    min = min(values, na.rm = TRUE),
                    max = max(values, na.rm = TRUE),
                    sd = sd(values, na.rm = TRUE),
                    mean = mean(values, na.rm = TRUE)
  )
  return(ftd2)
}