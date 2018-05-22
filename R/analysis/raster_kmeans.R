#' Calculates a kmeans classification on a raster stack
#' 
#' Given a raster stack object this function will 
#' apply a multivariate kmeans classification
#' 
#' @author Martin Jung
#' @author anonymous
#' @import raster
#' 
#' @param stack a raster stack object
#' @param cols Which columns to exclude
#' @param cents How many classes (Default: 3-5)
#' @param plot Should the result be plotted (Default = F)
#' @param scale Should the stack be scaled (Default = F)
#' @return returns the clustered stack
#' @export

raster_kmeans <- function(stack,cols=NA,cents=3:5,plot=F,scale=F){
  
  i <- complete.cases(stack[])
  data <- stack[i]
  
  if (!is.na(cols)){data <- data[,cols]}
  
  if (scale==T){data <- scale(data,center=T)}
  
  if (plot==T){kmeans.plot(data)}  
  
  clst.lst <- list()
  for (c in cents){
    clust <- kmeans(data,centers=c,nstart=99)
    clst.lst <- append(clst.lst,list(clust))
    names(clst.lst)[length(clst.lst)] <- paste('c',c,sep='')
  }
  
  stack <- stack(stack)  
  rlst <- list()
  for (c in clst.lst){
    cr <- stack@layers[[1]]
    cr[!is.na(cr)] <- c['cluster'][[1]]
    rlst <- append(rlst,list(cr))
  }
  clst.stack <- stack(rlst)
  names(clst.stack) <- names(clst.lst)  
  clst.lst <- append(clst.lst,list(stack=clst.stack))    
  return(clst.lst)
}
