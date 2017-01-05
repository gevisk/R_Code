########================================================================####GvK#
######                                                                    ######
####    Christmas Tree                                                      ####
##        An animated christmas tree                                          ##
########================================================================########


# Inspired by the rblogger christmas tree
# https://www.r-bloggers.com/make-a-christmas-tree-in-r-with-random-ornamentspresents/
# I changed the colors and animated the christmas ball.

# rcol() return a random color
rcol <- function() {  
  # vector of color names. Feel free to change those by the colors you like
  x=c("salmon", "slateblue", "red", "yellow",
      "goldenrod", "seagreen", "wheat2",
      "magenta", "orange2", "lavenderblush2",
      "honeydew", "royalblue3", "snow",
      "turquoise", "purple")
  # return a random color
  return(sample(x, 1))
}

christmas.tree <- function(anim=10, flash=0.5) {
  # Canvas (empty plot)
  plot(1:10, 1:10, xlim=c(-5,5), ylim=c(0,10), type="n", xlab="",
       ylab="", xaxt="n", yaxt="n", main="Merry Christmas !!", col.main="red")
  # Construct the tree
  rect(-1,0,1,2,col="tan4",border="brown4",lwd=3)
  polygon(c(-5,0,5),c(2,4,2),col="forestgreen" ,border="olivedrab4",lwd=3)
  polygon(c(-4,0,4),c(3.5,5.5,3.5),col="darkgreen",border="olivedrab4",lwd=3)
  polygon(c(-3,0,3),c(5,6.5,5),col="forestgreen",border="olivedrab4",lwd=3)
  polygon(c(-2,0,2),c(6.25,7.5,6.25),col="darkgreen", border="olivedrab4",lwd=3)

  # Add some christmas presents
  xPres = runif(10,-4.5,4.5)
  xWidth = runif(10,0.1,0.5)
  xHeight=runif(10,0,1)
  for(i in 1:10){
    rect(xPres[i]-xWidth[i],0,xPres[i]+xWidth[i],xHeight[i],
         col=sample(c("blue","red"),size=1))
    rect(xPres[i]-0.2*xWidth[i],0,xPres[i]+0.2*xWidth[i],xHeight[i],
         col=sample(c("gold","grey87"),size=1))
  }
  
  # Random positions for the balls on the tree
  x1=runif(9,-5,5)
  x2=runif(7,-4,4)
  x3=runif(5,-3,3)
  x4=runif(3,-2,2)
  
  # Loop
  duration <- 0
  while (anim >= duration) {
    # add points of random color at the positions defined earlier
    points(x=x1,y=rep(2,9),col=sample(c(rcol(),rcol()),size=4,replace=T),cex=3,pch=19)
    points(x=x2,y=rep(3.5,7),col=sample(c(rcol(),rcol()),size=4,replace=T),cex=3,pch=19)
    points(x=x3,y=rep(5,5),col=sample(c(rcol(),rcol()),size=4,replace=T),cex=3,pch=19)
    points(x=x4,y=rep(6.25,3),col=sample(c(rcol(),rcol()),size=4,replace=T),cex=3,pch=19)
    points(0,7.5,pch=8,cex=5,col="gold",lwd=3)
    duration <- duration + flash
    Sys.sleep(flash) # wait a little
  }
}

# Execute:
christmas.tree()
