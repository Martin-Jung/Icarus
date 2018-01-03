#' Adds a map frame to spatial plot
#' 
#' @author Anonymous
#' @import PBSmapping
#' @param bar.width is the width of the frame in inches
#' @param deg.ext is the extention of the frame segments in degrees
#' @return returns the standardises rasters
#' @examples
#'require(maps)
#'require(PBSmapping)
#'#example plot
#'par(mar=c(4,4,1,1))
#'plot(0,0,t="n",
#'     xlim=c(-180, 180),ylim=c(-80,80),
#'     xlab="", ylab="",
#'     xaxs="i", yaxs="i",
#'     xaxt="n", yaxt="n"
#')
#'map("world", add=TRUE, fill=TRUE, col="grey90", lwd=0.5)
#'axis(1, at=seq(-150, 150, 30), line=-0.5, lwd = 0)
#'axis(2, at=seq(-60, 60, 30), line=-0.5, lwd = 0)
#'abline(h=seq(-90,90,10), lty=3, col="grey")
#'abline(v=seq(-180,180,10), lty=3, col="grey")
#'map.frame(deg.ext=30)
#'
#' @export


map.frame <- function(bar.width=NULL, deg.ext=1, ...){
  if(missing(bar.width)) bar.width <- mean(par()$pin)*0.02
  usr <- par()$usr
  bar.width.x <- bar.width/par()$pin[1] * (usr[2]-usr[1])
  bar.width.y <- bar.width/par()$pin[2] * (usr[4]-usr[3])
  bar.lims.x <- seq(-180,180,deg.ext)
  bar.lims.y <- seq(-90,90,deg.ext)
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
  bar.bottom <- data.frame(PID=rep(1, 4), POS=1:4, X=c(usr[1],usr[1]+bar.width.y,usr[2]-bar.width.y,usr[2]), Y=c(usr[3],usr[3]+bar.width.x,usr[3]+bar.width.x,usr[3]))
  bar.top <- data.frame(PID=rep(1, 4), POS=1:4, X=c(usr[1],usr[1]+bar.width.y,usr[2]-bar.width.y,usr[2]), Y=c(usr[4],usr[4]-bar.width.x,usr[4]-bar.width.x,usr[4]))
  bar.left <- data.frame(PID=rep(1, 4), POS=1:4, X=c(usr[1],usr[1],usr[1]+bar.width.y,usr[1]+bar.width.y), Y=c(usr[3],usr[4], usr[4]-bar.width.x,usr[3]+bar.width.x))
  bar.right <- data.frame(PID=rep(1, 4), POS=1:4, X=c(usr[2],usr[2]-bar.width.y,usr[2]-bar.width.y,usr[2]), Y=c(usr[3],usr[3]+bar.width.x,usr[4]-bar.width.x,usr[4]))
  
  #X axis
  for(i in seq(length(bar.lims.x)-1)){
    xs <- c(bar.lims.x[i], bar.lims.x[i], bar.lims.x[i+1], bar.lims.x[i+1])
    #bottom
    ys <- c(usr[3], usr[3]+bar.width.y, usr[3]+bar.width.y, usr[3])
    bottom <- data.frame(PID=rep(1, 4), POS=1:4, X=xs, Y=ys)
    bottom.join <- joinPolys(bottom,bar.bottom)
    
    #top
    ys <- c(usr[4]-bar.width.y, usr[4], usr[4], usr[4]-bar.width.y)
    top <- data.frame(PID=rep(1, 4), POS=1:4, X=xs, Y=ys)
    top.join <- joinPolys(top,bar.top)
    
    tmp.col <- ifelse(is.wholenumber(i/2), "black", "white")
    polygon(bottom.join$X, bottom.join$Y, col=tmp.col, ...)
    polygon(top.join$X, top.join$Y, col=tmp.col, ...)
  }
  #Y axis
  for(i in seq(length(bar.lims.y)-1)){
    ys <- c(bar.lims.y[i], bar.lims.y[i], bar.lims.y[i+1], bar.lims.y[i+1])
    #left
    xs <- c(usr[1], usr[1]+bar.width.x, usr[1]+bar.width.x, usr[1])
    left <- data.frame(PID=rep(1, 4), POS=1:4, X=xs, Y=ys)
    left.join <- joinPolys(left,bar.left)
    
    #right
    xs <- c(usr[2], usr[2]-bar.width.x, usr[2]-bar.width.x, usr[2])
    right <- data.frame(PID=rep(1, 4), POS=1:4, X=xs, Y=ys)
    right.join <- joinPolys(right,bar.right) 
    
    
    
    tmp.col <- ifelse(is.wholenumber(i/2), "black", "white")
    polygon(left.join$X, left.join$Y, col=tmp.col, ...)
    polygon(right.join$X, right.join$Y, col=tmp.col, ...)
  }
  box()
}

