###############################################################################
###############################################################################
#code borrowed from http://is-r.tumblr.com/
###############################################################################
###############################################################################

library(png)
library(reshape)
library(ggplot2)

taface<-readPNG("face.png")
longImage<-melt(taface)
rgbImage<-reshape(longImage, timevar = "X3",idvar = c("X1", "X2"), direction = "wide")
rgbImage$X1<- -rgbImage$X1
voronoiMeans <- kmeans(rgbImage, centers = 1000, iter.max = 50)
voronoiColor <- voronoiMeans$centers[voronoiMeans$cluster, 3:5]
with(rgbImage, plot(X2, X1, col = rgb(voronoiColor), asp = 1, pch = "."))
plot(rgbImage$X2,rgbImage$X1, col = rgb(voronoiColor), 
     asp = 1, pch = ".",ann=F,bty="n",xaxt="n",yaxt="n",
     usr=c(min(rgbImage$X2),max(rgbImage$X2),min(rgbImage$X1),max(rgbImage$X1)),
     mar=c(0,0,0,0),oma=c(0,0,0,0),omd=c(0,0,0,0),mgp=c(0,0,0))

