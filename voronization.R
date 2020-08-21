##############################################################################/
##############################################################################/
#code borrowed from http://is-r.tumblr.com/
##############################################################################/
##############################################################################/

library(png)
library(reshape)
library(ggplot2)

#a cute example
otter<-readPNG("data/otter.png")
longImage<-melt(otter)
rgbImage<-reshape(longImage,timevar="X3",idvar=c("X1","X2"),direction="wide")
rgbImage$X1<-(-rgbImage$X1)
voronoiMeans<-kmeans(rgbImage,centers=1000,iter.max=50)
voronoiColor<-voronoiMeans$centers[voronoiMeans$cluster,3:5]
with(rgbImage,plot(X2,X1,col=rgb(voronoiColor),asp=1,pch="."))
plot(rgbImage$X2,rgbImage$X1,col=rgb(voronoiColor), 
     asp=1,pch=".",ann=F,bty="n",xaxt="n",yaxt="n",
     usr=c(min(rgbImage$X2),max(rgbImage$X2),
           min(rgbImage$X1),max(rgbImage$X1)),
     mar=c(0,0,0,0),oma=c(0,0,0,0),omd=c(0,0,0,0),mgp=c(0,0,0))


#a less cute example, me
maface<-readPNG("data/maface.png")
longImage<-melt(maface)
rgbImage<-reshape(longImage,timevar="X3",idvar=c("X1","X2"),direction="wide")
rgbImage$X1<-(-rgbImage$X1)
voronoiMeans<-kmeans(rgbImage,centers=1000,iter.max=50)
voronoiColor<-voronoiMeans$centers[voronoiMeans$cluster,3:5]
with(rgbImage,plot(X2,X1,col=rgb(voronoiColor),asp=1,pch="."))

png(file="output/benoitB.png")
plot(rgbImage$X2,rgbImage$X1,col=rgb(voronoiColor), 
     asp=1,pch=".",ann=F,bty="n",xaxt="n",yaxt="n",
     usr=c(min(rgbImage$X2),max(rgbImage$X2),
           min(rgbImage$X1),max(rgbImage$X1)),
     mar=c(0,0,0,0),oma=c(0,0,0,0),omd=c(0,0,0,0),mgp=c(0,0,0))
dev.off()

#Annie
maface<-readPNG("data/annieM.png")
longImage<-melt(maface)
rgbImage<-reshape(longImage,timevar="X3",idvar=c("X1","X2"),direction="wide")
rgbImage$X1<-(-rgbImage$X1)
voronoiMeans<-kmeans(rgbImage,centers=1000,iter.max=50)
voronoiColor<-voronoiMeans$centers[voronoiMeans$cluster,3:5]
with(rgbImage,plot(X2,X1,col=rgb(voronoiColor),asp=1,pch="."))

png(file="output/annieM.png")
plot(rgbImage$X2,rgbImage$X1,col=rgb(voronoiColor), 
     asp=1,pch=".",ann=F,bty="n",xaxt="n",yaxt="n",
     usr=c(min(rgbImage$X2),max(rgbImage$X2),
           min(rgbImage$X1),max(rgbImage$X1)),
     mar=c(0,0,0,0),oma=c(0,0,0,0),omd=c(0,0,0,0),mgp=c(0,0,0))
dev.off()

#Lucile
maface<-readPNG("data/lucileB.png")
longImage<-melt(maface)
rgbImage<-reshape(longImage,timevar="X3",idvar=c("X1","X2"),direction="wide")
rgbImage$X1<-(-rgbImage$X1)
voronoiMeans<-kmeans(rgbImage,centers=1000,iter.max=50)
voronoiColor<-voronoiMeans$centers[voronoiMeans$cluster,3:5]
with(rgbImage,plot(X2,X1,col=rgb(voronoiColor),asp=1,pch="."))

png(file="output/lucileB.png")
plot(rgbImage$X2,rgbImage$X1,col=rgb(voronoiColor), 
     asp=1,pch=".",ann=F,bty="n",xaxt="n",yaxt="n",
     usr=c(min(rgbImage$X2),max(rgbImage$X2),
           min(rgbImage$X1),max(rgbImage$X1)),
     mar=c(0,0,0,0),oma=c(0,0,0,0),omd=c(0,0,0,0),mgp=c(0,0,0))
dev.off()

#Christine
maface<-readPNG("data/christineB.png")
longImage<-melt(maface)
rgbImage<-reshape(longImage,timevar="X3",idvar=c("X1","X2"),direction="wide")
rgbImage$X1<-(-rgbImage$X1)
voronoiMeans<-kmeans(rgbImage,centers=1000,iter.max=50)
voronoiColor<-voronoiMeans$centers[voronoiMeans$cluster,3:5]
with(rgbImage,plot(X2,X1,col=rgb(voronoiColor),asp=1,pch="."))

png(file="output/christineB.png")
plot(rgbImage$X2,rgbImage$X1,col=rgb(voronoiColor), 
     asp=1,pch=".",ann=F,bty="n",xaxt="n",yaxt="n",
     usr=c(min(rgbImage$X2),max(rgbImage$X2),
           min(rgbImage$X1),max(rgbImage$X1)),
     mar=c(0,0,0,0),oma=c(0,0,0,0),omd=c(0,0,0,0),mgp=c(0,0,0))
dev.off()

#Claire
maface<-readPNG("data/claireM.png")
longImage<-melt(maface)
rgbImage<-reshape(longImage,timevar="X3",idvar=c("X1","X2"),direction="wide")
rgbImage$X1<-(-rgbImage$X1)
voronoiMeans<-kmeans(rgbImage,centers=1000,iter.max=50)
voronoiColor<-voronoiMeans$centers[voronoiMeans$cluster,3:5]
with(rgbImage,plot(X2,X1,col=rgb(voronoiColor),asp=1,pch="."))

png(file="output/claireM.png")
plot(rgbImage$X2,rgbImage$X1,col=rgb(voronoiColor), 
     asp=1,pch=".",ann=F,bty="n",xaxt="n",yaxt="n",
     usr=c(min(rgbImage$X2),max(rgbImage$X2),
           min(rgbImage$X1),max(rgbImage$X1)),
     mar=c(0,0,0,0),oma=c(0,0,0,0),omd=c(0,0,0,0),mgp=c(0,0,0))
dev.off()

#Evelyne
maface<-readPNG("data/evelyneM.png")
longImage<-melt(maface)
rgbImage<-reshape(longImage,timevar="X3",idvar=c("X1","X2"),direction="wide")
rgbImage$X1<-(-rgbImage$X1)
voronoiMeans<-kmeans(rgbImage,centers=1000,iter.max=50)
voronoiColor<-voronoiMeans$centers[voronoiMeans$cluster,3:5]
with(rgbImage,plot(X2,X1,col=rgb(voronoiColor),asp=1,pch="."))

png(file="output/evelyneM.png")
plot(rgbImage$X2,rgbImage$X1,col=rgb(voronoiColor), 
     asp=1,pch=".",ann=F,bty="n",xaxt="n",yaxt="n",
     usr=c(min(rgbImage$X2),max(rgbImage$X2),
           min(rgbImage$X1),max(rgbImage$X1)),
     mar=c(0,0,0,0),oma=c(0,0,0,0),omd=c(0,0,0,0),mgp=c(0,0,0))
dev.off()

#Florent
maface<-readPNG("data/florentR.png")
longImage<-melt(maface)
rgbImage<-reshape(longImage,timevar="X3",idvar=c("X1","X2"),direction="wide")
rgbImage$X1<-(-rgbImage$X1)
voronoiMeans<-kmeans(rgbImage,centers=1000,iter.max=50)
voronoiColor<-voronoiMeans$centers[voronoiMeans$cluster,3:5]
with(rgbImage,plot(X2,X1,col=rgb(voronoiColor),asp=1,pch="."))

png(file="output/florentR.png")
plot(rgbImage$X2,rgbImage$X1,col=rgb(voronoiColor), 
     asp=1,pch=".",ann=F,bty="n",xaxt="n",yaxt="n",
     usr=c(min(rgbImage$X2),max(rgbImage$X2),
           min(rgbImage$X1),max(rgbImage$X1)),
     mar=c(0,0,0,0),oma=c(0,0,0,0),omd=c(0,0,0,0),mgp=c(0,0,0))
dev.off()

#Laetitia
maface<-readPNG("data/laetitiaC.png")
longImage<-melt(maface)
rgbImage<-reshape(longImage,timevar="X3",idvar=c("X1","X2"),direction="wide")
rgbImage$X1<-(-rgbImage$X1)
voronoiMeans<-kmeans(rgbImage,centers=1000,iter.max=50)
voronoiColor<-voronoiMeans$centers[voronoiMeans$cluster,3:5]
with(rgbImage,plot(X2,X1,col=rgb(voronoiColor),asp=1,pch="."))

png(file="output/laetitiaC.png")
plot(rgbImage$X2,rgbImage$X1,col=rgb(voronoiColor), 
     asp=1,pch=".",ann=F,bty="n",xaxt="n",yaxt="n",
     usr=c(min(rgbImage$X2),max(rgbImage$X2),
           min(rgbImage$X1),max(rgbImage$X1)),
     mar=c(0,0,0,0),oma=c(0,0,0,0),omd=c(0,0,0,0),mgp=c(0,0,0))
dev.off()

#Michael
maface<-readPNG("data/michaelLG.png")
longImage<-melt(maface)
rgbImage<-reshape(longImage,timevar="X3",idvar=c("X1","X2"),direction="wide")
rgbImage$X1<-(-rgbImage$X1)
voronoiMeans<-kmeans(rgbImage,centers=1000,iter.max=50)
voronoiColor<-voronoiMeans$centers[voronoiMeans$cluster,3:5]
with(rgbImage,plot(X2,X1,col=rgb(voronoiColor),asp=1,pch="."))

png(file="output/michaelLG.png")
plot(rgbImage$X2,rgbImage$X1,col=rgb(voronoiColor), 
     asp=1,pch=".",ann=F,bty="n",xaxt="n",yaxt="n",
     usr=c(min(rgbImage$X2),max(rgbImage$X2),
           min(rgbImage$X1),max(rgbImage$X1)),
     mar=c(0,0,0,0),oma=c(0,0,0,0),omd=c(0,0,0,0),mgp=c(0,0,0))
dev.off()

#Séverine
maface<-readPNG("data/severineF.png")
longImage<-melt(maface)
rgbImage<-reshape(longImage,timevar="X3",idvar=c("X1","X2"),direction="wide")
rgbImage$X1<-(-rgbImage$X1)
voronoiMeans<-kmeans(rgbImage,centers=1000,iter.max=50)
voronoiColor<-voronoiMeans$centers[voronoiMeans$cluster,3:5]
with(rgbImage,plot(X2,X1,col=rgb(voronoiColor),asp=1,pch="."))

png(file="output/severineF.png")
plot(rgbImage$X2,rgbImage$X1,col=rgb(voronoiColor), 
     asp=1,pch=".",ann=F,bty="n",xaxt="n",yaxt="n",
     usr=c(min(rgbImage$X2),max(rgbImage$X2),
           min(rgbImage$X1),max(rgbImage$X1)),
     mar=c(0,0,0,0),oma=c(0,0,0,0),omd=c(0,0,0,0),mgp=c(0,0,0))
dev.off()

#Christophe
maface<-readPNG("data/christopheP.png")
longImage<-melt(maface)
rgbImage<-reshape(longImage,timevar="X3",idvar=c("X1","X2"),direction="wide")
rgbImage$X1<-(-rgbImage$X1)
voronoiMeans<-kmeans(rgbImage,centers=1000,iter.max=50)
voronoiColor<-voronoiMeans$centers[voronoiMeans$cluster,3:5]
with(rgbImage,plot(X2,X1,col=rgb(voronoiColor),asp=1,pch="."))

png(file="output/christopheP.png")
plot(rgbImage$X2,rgbImage$X1,col=rgb(voronoiColor), 
     asp=1,pch=".",ann=F,bty="n",xaxt="n",yaxt="n",
     usr=c(min(rgbImage$X2),max(rgbImage$X2),
           min(rgbImage$X1),max(rgbImage$X1)),
     mar=c(0,0,0,0),oma=c(0,0,0,0),omd=c(0,0,0,0),mgp=c(0,0,0))
dev.off()

#Marie-France
maface<-readPNG("data/mariefranceG.png")
longImage<-melt(maface)
rgbImage<-reshape(longImage,timevar="X3",idvar=c("X1","X2"),direction="wide")
rgbImage$X1<-(-rgbImage$X1)
voronoiMeans<-kmeans(rgbImage,centers=1000,iter.max=50)
voronoiColor<-voronoiMeans$centers[voronoiMeans$cluster,3:5]
with(rgbImage,plot(X2,X1,col=rgb(voronoiColor),asp=1,pch="."))

png(file="output/mariefranceG.png")
plot(rgbImage$X2,rgbImage$X1,col=rgb(voronoiColor), 
     asp=1,pch=".",ann=F,bty="n",xaxt="n",yaxt="n",
     usr=c(min(rgbImage$X2),max(rgbImage$X2),
           min(rgbImage$X1),max(rgbImage$X1)),
     mar=c(0,0,0,0),oma=c(0,0,0,0),omd=c(0,0,0,0),mgp=c(0,0,0))
dev.off()

#Alizée
maface<-readPNG("data/alizeeT.png")
longImage<-melt(maface)
rgbImage<-reshape(longImage,timevar="X3",idvar=c("X1","X2"),direction="wide")
rgbImage$X1<-(-rgbImage$X1)
voronoiMeans<-kmeans(rgbImage,centers=1000,iter.max=50)
voronoiColor<-voronoiMeans$centers[voronoiMeans$cluster,3:5]
with(rgbImage,plot(X2,X1,col=rgb(voronoiColor),asp=1,pch="."))

png(file="output/alizeeT.png")
plot(rgbImage$X2,rgbImage$X1,col=rgb(voronoiColor), 
     asp=1,pch=".",ann=F,bty="n",xaxt="n",yaxt="n",
     usr=c(min(rgbImage$X2),max(rgbImage$X2),
           min(rgbImage$X1),max(rgbImage$X1)),
     mar=c(0,0,0,0),oma=c(0,0,0,0),omd=c(0,0,0,0),mgp=c(0,0,0))
dev.off()

#Elorri
maface<-readPNG("data/elorriS.png")
longImage<-melt(maface)
rgbImage<-reshape(longImage,timevar="X3",idvar=c("X1","X2"),direction="wide")
rgbImage$X1<-(-rgbImage$X1)
voronoiMeans<-kmeans(rgbImage,centers=1000,iter.max=50)
voronoiColor<-voronoiMeans$centers[voronoiMeans$cluster,3:5]
with(rgbImage,plot(X2,X1,col=rgb(voronoiColor),asp=1,pch="."))

png(file="output/elorriS.png")
plot(rgbImage$X2,rgbImage$X1,col=rgb(voronoiColor), 
     asp=1,pch=".",ann=F,bty="n",xaxt="n",yaxt="n",
     usr=c(min(rgbImage$X2),max(rgbImage$X2),
           min(rgbImage$X1),max(rgbImage$X1)),
     mar=c(0,0,0,0),oma=c(0,0,0,0),omd=c(0,0,0,0),mgp=c(0,0,0))
dev.off()

#Isabelle
maface<-readPNG("data/isabelleP.png")
longImage<-melt(maface)
rgbImage<-reshape(longImage,timevar="X3",idvar=c("X1","X2"),direction="wide")
rgbImage$X1<-(-rgbImage$X1)
voronoiMeans<-kmeans(rgbImage,centers=1000,iter.max=50)
voronoiColor<-voronoiMeans$centers[voronoiMeans$cluster,3:5]
with(rgbImage,plot(X2,X1,col=rgb(voronoiColor),asp=1,pch="."))

png(file="output/isabelleP.png")
plot(rgbImage$X2,rgbImage$X1,col=rgb(voronoiColor), 
     asp=1,pch=".",ann=F,bty="n",xaxt="n",yaxt="n",
     usr=c(min(rgbImage$X2),max(rgbImage$X2),
           min(rgbImage$X1),max(rgbImage$X1)),
     mar=c(0,0,0,0),oma=c(0,0,0,0),omd=c(0,0,0,0),mgp=c(0,0,0))
dev.off()

#Ingvild
maface<-readPNG("data/ingvildL.png")
longImage<-melt(maface)
rgbImage<-reshape(longImage,timevar="X3",idvar=c("X1","X2"),direction="wide")
rgbImage$X1<-(-rgbImage$X1)
voronoiMeans<-kmeans(rgbImage,centers=1000,iter.max=50)
voronoiColor<-voronoiMeans$centers[voronoiMeans$cluster,3:5]
with(rgbImage,plot(X2,X1,col=rgb(voronoiColor),asp=1,pch="."))

png(file="output/ingvildL.png")
plot(rgbImage$X2,rgbImage$X1,col=rgb(voronoiColor), 
     asp=1,pch=".",ann=F,bty="n",xaxt="n",yaxt="n",
     usr=c(min(rgbImage$X2),max(rgbImage$X2),
           min(rgbImage$X1),max(rgbImage$X1)),
     mar=c(0,0,0,0),oma=c(0,0,0,0),omd=c(0,0,0,0),mgp=c(0,0,0))
dev.off()

#Mathilda
maface<-readPNG("data/mathildaI.png")
longImage<-melt(maface)
rgbImage<-reshape(longImage,timevar="X3",idvar=c("X1","X2"),direction="wide")
rgbImage$X1<-(-rgbImage$X1)
voronoiMeans<-kmeans(rgbImage,centers=1000,iter.max=50)
voronoiColor<-voronoiMeans$centers[voronoiMeans$cluster,3:5]
with(rgbImage,plot(X2,X1,col=rgb(voronoiColor),asp=1,pch="."))

png(file="output/mathildaI.png")
plot(rgbImage$X2,rgbImage$X1,col=rgb(voronoiColor), 
     asp=1,pch=".",ann=F,bty="n",xaxt="n",yaxt="n",
     usr=c(min(rgbImage$X2),max(rgbImage$X2),
           min(rgbImage$X1),max(rgbImage$X1)),
     mar=c(0,0,0,0),oma=c(0,0,0,0),omd=c(0,0,0,0),mgp=c(0,0,0))
dev.off()


##############################################################################/
#END
##############################################################################/