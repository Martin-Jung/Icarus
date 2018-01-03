#' Creates a bivariate map of two correlated raster layers
#' 
#' @author Martin Jung
#' @author José Hidasi Neto
#' @param x Input numeric vector
#'
#' @examples
#' 
#' #r1 <- raster("...")
#' #r2 <- raster("...")
#' 
#' #bivmap<-bivariate.map(r1,r2)
#' #plot(bivmap,frame.plot=F,axes=F,box=F,add=F,legend=F,col=unique(as.vector(col.matrix)))
#' #map(interior=T,add=T)
#' 
#' @return returns a normalized vector
#' @export

library(classInt)
library(raster)
library(rgdal)
library(dismo)
library(XML)
library(maps)
library(RColorBrewer)
library(ggplot2)
library(dplyr)

#r1 <- raster("/home/martin/Downloads/qgis_testing/amphibians.grd")
#r2 <- raster("/home/martin/Downloads/qgis_testing/reptiles.grd")
#r1 <- raster("/home/martin/Documents/Studium/Kopenhagen/Masters/GIS/Climate/p_proj_bio6.img")
#r2 <- raster("/home/martin/Documents/Studium/Kopenhagen/Masters/GIS/Climate/p_proj_bio7.img")

#col.matrix <- bivariate.legend(remNa(getValues(r1)),remNa(getValues(r2)),doplot = F)

#col.matrix

#col.matrix %>% melt() %>%  ggplot(aes(Var1,Var2,fill=value)) + geom_raster() 

#bivmap<-bivariate.map(r1,r2)

#plot(bivmap,frame.plot=F,axes=F,box=F,add=F,legend=F,col=unique(as.vector(col.matrix)))
#map(interior=T,add=T)

bivariate.map <- function(rx, ry,...){
  # todo - Unify rasters
  
  # Acquire data raster x
  quanmean <- getValues(rx)
  temp <- data.frame(quanmean, quantile=rep(NA, length(quanmean)))
  brks <- with(temp, quantile(temp,na.rm=TRUE, probs = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)))
  r1 <- within(temp, quantile <- cut(quanmean, breaks = brks, labels = 2:11,include.lowest = TRUE))
  quantr<-data.frame(r1[,2]) 
  quanvar<-getValues(ry)
  temp <- data.frame(quanvar, quantile=rep(NA, length(quanvar)))
  brks <- with(temp, quantile(temp,na.rm=TRUE, probs = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)))
  r2 <- within(temp, quantile <- cut(quanvar, breaks = brks, labels = 11:2,include.lowest = TRUE))
  quantr2<-data.frame(r2[,2])
  as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
  col.matrix2<-col.matrix
  cn<-unique(col.matrix)
  for(i in 1:length(col.matrix2)){
    ifelse(is.na(col.matrix2[i]),col.matrix2[i]<-1,col.matrix2[i]<-which(col.matrix2[i]==cn)[1])}
  cols<-numeric(length(quantr[,1]))
  for(i in 1:length(quantr[,1])){
    a<-as.numeric.factor(quantr[i,1])
    b<-as.numeric.factor(quantr2[i,1])
    cols[i]<-as.numeric(col.matrix2[b,a])}
  r<-rx
  r[1:length(r)]<-cols
  return(r)
}




#' Creates a seperate legend for a bivariate map of two correlated raster layers
#' 
#' @author Martin Jung
#' @concept by José Hidasi Neto
#' @param x Input numeric vector for x axis
#' @param y Input numeric vector for y axis
#' @param type a interval style ("fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", or "jenks")
#' @param n Number of breaks
#' @param xlab x-axis label
#' @param ylab y-axis label 
#' @param doplot Should a plot be made?
#' @examples
#'  raw <- vector(2,5,2,6,11,6,4,3)
#'
#'  normalize(raw)               
#'
#' @return returns a normalized vector
#' @export

bivariate.legend <- function(x,y,type="quantile",n=10,xlab="",ylab="",doplot=F,...){
  stopifnot(length(x)==length(y))
  # Define color classes
  my.class.x<-classIntervals(x,n=n,style=type)
  my.class.y<-classIntervals(y,n=n,style=type)
  
  # Find Colours
  my.pal.1<-findColours(my.class.x,c(rgb(0,150,235, maxColorValue=255),"grey"))
  my.pal.2<-findColours(my.class.x,c(rgb(130,0,80, maxColorValue=255), rgb(255,230,15, maxColorValue=255)))
  
  # Make Matrix
  col.matrix<-matrix(nrow = length(x), ncol = length(y), NA)
  for(i in 1:length(x)){
    my.col<-c(paste(my.pal.1[i]),paste(my.pal.2[i]))
    col.matrix[length(x)+1-i,]<-findColours(my.class.x,my.col)
  }
  # Should a plot be done?
  if(doplot){
    plot(c(1,1),pch=19,col=my.pal.1, cex=0.5,xlim=c(0,1),ylim=c(0,1),frame.plot=F,xlab=xlab,ylab=ylab)
    for(i in 1:101){
      col.temp<-col.matrix[i-1,]
      points(seq(0,1,.01),rep((i-1)/100,101),pch=15,col=col.temp, cex=1)
    }    
  }  
  col.matrix <- col.matrix[c(1,10,20,30,40,50,60,70,80,90,100), c(1,10,20,30,40,50,60,70,80,90,100)]
  return(col.matrix)
}

#' Creates a hue rainbow color scale of n colors
#' 
#' @author Martin Jung
#' @param n number of colors
#'
#' @return a vector of n colors
#' @keywords internal

clrs.hcl <- function(n) {
  hcl(h = seq(230, 0, length.out = n), 
      c = 60, l = seq(10, 90, length.out = n), 
      fixup = TRUE)
}


## 16 bit NDVI color bar
cndvi=function(br=0.2,c1=c("darkgrey","burlywood4"),c2=c("burlywood4","darkgreen","green")){
  at=unique(c(seq(-1,0,len=32768),seq(0,1,len=32768)))
  bg=colorRampPalette(c1)
  gr=colorRampPalette(c2)
  return(list(at=at,col=c(bg(sum(at<br)),gr(sum(at>=br)))))
}
ndvi.colors=cndvi()$col