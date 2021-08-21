library(RColorBrewer)
library(sp)
library(scales)
label<-read.csv("yinyang.csv",header=T)

radius1 <- 1.15
radius2 <- 0.90
theta <- seq(0, 2 * pi, length = 1000)
theta2 <- seq(0, 2 * pi, length = 10000)

xx<-data.frame(radius1 * cos(theta),radius1 * sin(theta))
yy<-SpatialPolygons(list(Polygons(list(Polygon(xx)),"y")))

set.seed(1234)
HexPts <-spsample(yy,type="hexagonal",cellsize=0.2)
HexPols <- HexPoints2SpatialPolygons(HexPts)

par(mar=c(0,0,0,0))
plot(c(-1, 1), c(-1, 1), type = "n",axes=F,xlab="",ylab="")

for (i in 1:dim(label)[1]) {
plot(HexPols[label[i,1]], add=TRUE,col=alpha(brewer.pal(as.numeric(label[i,3]),
as.character(label[i,4]))[as.numeric(label[i,5])],as.numeric(label[i,9])),border="white",lwd=4)
}

lines(x = radius2/2 * cos(theta2)[1:(10000*0.25)], y = (radius2/2 * sin(theta2)-0.45)[1:(10000*0.25)],col=alpha("white",0.6),lwd=33,lend=2)
lines(x = radius2/2 * cos(theta2)[(10000*0.75):10000], y = (radius2/2 * sin(theta2)-0.45)[(10000*0.75):10000],col=alpha("white",0.6),lwd=33,lend=2)
lines(x = radius2/2 * cos(theta2)[(10000*0.25):(10000*0.75)], y = (radius2/2 * sin(theta2)+0.45)[(10000*0.25):(10000*0.75)],col=alpha("white",0.6),lwd=33,lend=2)
polygon(c(radius1*2 * cos(theta),radius2 * cos(theta)), c(radius1*2 * sin(theta),rev(radius2 * sin(theta))),
        border=alpha("white",0.8),col=alpha("white",0.8))

