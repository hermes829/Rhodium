# Clearing workspace
rm(list=ls())

# Setting working directory
if(Sys.info()["user"]=="janus829")
{pathMain="~/Desktop/Research/Rhodium/R";
 pathGraphics="~/Dropbox/Research/Rhodium/Graphics";
 pathData="~/Dropbox/Research/Rhodium/Data"
}

 if(Sys.info()["user"]=="s7m")
{pathMain="~/Research/Rhodium/R";
 pathGraphics="~/Dropbox/Research/Rhodium/Graphics";
 pathFunctions="~/Research/Rhodium/R";
 pathData="~/Dropbox/Research/Rhodium/Data"
}

# Setting working directory
if(Sys.info()["user"]=="Ben")
{pathMain="C:/Users/Ben/Dropbox/Rhodium/R";
 pathGraphics="C:/Users/Ben/Dropbox/Rhodium/Graphics";
 pathFunctions="C:/Users/Ben/Documents/Github/Rhodium/R";
 pathData="C:/Users/Ben/Dropbox/Rhodium/Data"
}

library(maptools)
library(RColorBrewer)
library(raster)
library(rgdal)
source(pathFunctions+"/geodistance.R")

prio <- read.csv(pathData+"/PRIO - Conflict Site Data/ConflictSite 4-2010_v3 Dataset.csv")
#eigdp <- readAsciiGrid(pathData+"/Earth Institute - GDP Grids/gdp90_15mi.asc")
eigdp <- raster(pathData+"/Earth Institute - GDP Grids/gdp90_15m.tif")
oilgas <- readShapeSpatial(pathData+"/Horn - Giant Fields Data/Giant_Fields_Data.shp")
world <- readShapeSpatial(pathData+"/TM_WORLD_BORDERS_SIMPL-0.2.shp")

mypalatte <- colorRampPalette(c("#C7E9C0","#005A32"))
arg <- list(at=c(0,9), labels=c("Low","High"))
par(mar=c(1,1,1,1))
plot(log(eigdp),col=mypalatte(12),add=F,interpolate=T,breaks=0:9,axis.args=arg,frame=F,bty="n",xaxt="n",yaxt="n")
rect(-180,-90,180,90, col="gray", border=NA)
plot(world,col="white",border="gray",add=T)
plot(log(eigdp),col=mypalatte(12),add=T,interpolate=T,breaks=0:9,axis.args=arg,frame=F,bty="n",xaxt="n",yaxt="n")
plot(world,border="gray",add=T)
points(oilgas,pch=16,col="#0000FF55",cex=.5)
points(prio$Longitude,prio$Latitude,pch=16,cex=prio$Radius/700,col="#FF000055")
