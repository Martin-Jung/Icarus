#' Calculates a pca on a raster stack
#' 
#' Performs pca on raster stack
#' returns results of pca and new stack of pca scores
#' 
#' @author Martin Jung
#' @author anonymous
#' @import raster
#' 
#' @param stack a raster stack object
#' @param scale Should the stack be scaled (Default = T)
#' @return returns the pca result and pca stack
#' @export

raster_pca <- function(stack,scale=T){
  
  i <- complete.cases(stack[])
  mydata <- stack[i]
  if (scale==T){mydata <- scale(mydata,center=T)}
  pca <- princomp(mydata,cor=T)  
  pca.stack <- stack
  values(pca.stack)[i] <- pca$scores
  dimnames(pca.stack@data@values)[[2]] <- names(pca$sdev)
  names(pca.stack) <- names(pca$sdev)
  print(summary(pca))

  return(list(pca=pca,stack=pca.stack))
  
}

