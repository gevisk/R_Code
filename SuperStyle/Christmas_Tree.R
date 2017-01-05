########================================================================####GvK#
######                                                                    ######
####    Christmas Tree                                                      ####
##        An animated christmas tree                                          ##
########================================================================########


# Inspired by the rblogger christmas tree
# https://www.r-bloggers.com/make-a-christmas-tree-in-r-with-random-ornamentspresents/
# I changed the colors and animated the christmas ball.

# rcol() return a random color
rcol <- function() colors()[sample(x=1:657, 1)]


christmas.tree <- function(anim=10, flash=0.5) {
  # Make the canvas
  plot(1:10, 1:10, xlim=c(-5,5), ylim=c(0,10), type="n", xlab="",
       ylab="", xaxt="n", yaxt="n")
  # Make the branches
  rect(-1,0,1,2,col="tan4",border="brown4",lwd=3)
  polygon(c(-5,0,5),c(2,4,2),col="forestgreen" ,border="olivedrab4",lwd=3)
  polygon(c(-4,0,4),c(3.5,5.5,3.5),col="darkgreen",border="olivedrab4",lwd=3)
  polygon(c(-3,0,3),c(5,6.5,5),col="forestgreen",border="olivedrab4",lwd=3)
  polygon(c(-2,0,2),c(6.25,7.5,6.25),col="darkgreen", border="olivedrab4",lwd=3)
  Sys.sleep(0.5)

  # Add some presents
  xPres = runif(10,-4.5,4.5)
  xWidth = runif(10,0.1,0.5)
  xHeight=runif(10,0,1)
  for(i in 1:10){
    rect(xPres[i]-xWidth[i],0,xPres[i]+xWidth[i],xHeight[i],
         col=sample(c("blue","red"),size=1))
    rect(xPres[i]-0.2*xWidth[i],0,xPres[i]+0.2*xWidth[i],xHeight[i],
         col=sample(c("gold","grey87"),size=1))
  }

  Sys.sleep(0.5)
  x1=runif(4,-5,5)
  x2=runif(4,-4,4)
  x3=runif(4,-3,3)
  x4=runif(4,-2,2)
  duration <- 0
  while (anim >= duration) {
    points(x=x1,y=rep(2,4),col=sample(c(rcol(),rcol()),size=4,replace=T),cex=3,pch=19)
    points(x=x2,y=rep(3.5,4),col=sample(c(rcol(),rcol()),size=4,replace=T),cex=3,pch=19)
    points(x=x3,y=rep(5,4),col=sample(c(rcol(),rcol()),size=4,replace=T),cex=3,pch=19)
    points(x=x4,y=rep(6.25,4),col=sample(c(rcol(),rcol()),size=4,replace=T),cex=3,pch=19)
    points(0,7.5,pch=8,cex=5,col="gold",lwd=3)
    duration <- duration + flash
    Sys.sleep(flash)
  }
}

# Execute:
  
christmas.tree()
