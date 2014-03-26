if(Sys.info()["user"]=="janus829"){source('/Users/janus829/Desktop/Research/Rhodium/R/setup.R')}
if(Sys.info()["user"]=="Ben"){source('/Users/Ben/Github/Rhodium/R/setup.R')}

library(cshapes)

# Load conflict country year data
setwd(pathData)
load('combinedData.rda')
load("cityTotPopLatLongvFinal.rda")
prio <- read.csv("PRIO - Conflict Site Data/ConflictSite 4-2010_v3 Dataset.csv",stringsAsFactors=F)
source(paste0(pathMain,"/geodistance.R"))

fYrCty$cname <- toupper(countrycode(fYrCty$cname,"country.name","country.name"))
prio$Conflict.territory <- toupper(countrycode(prio$Conflict.territory,"country.name","country.name"))

plotConflict <- function(cname, threshold=1)
{
  worldmap <- cshp(date=as.Date("1990-01-01"),useGW=F)

  ccode <- countrycode(cname,"country.name","cown")
  country.shape <- worldmap[worldmap$COWCODE==ccode,]
  # gradient <- colorRampPalette(c("#ffffcc","#c7e9b4","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#0c2c84"))
  # years <- sort(prio$Year[prio$Conflict.territory==cname])
  # years <- years-min(years)
  
  growth <- data.frame(low=ifelse(yData$NY.GDP.PCAP.KD.ZG_l0[yData$ccode==ccode]<=1,"#d53e4f","#3288bd"),year=yData$year[yData$ccode==ccode])
  newprio <- na.omit(prio[prio$Conflict.territory==cname,])
  newprio <- merge(newprio,growth,by.x="Year",by.y="year",all.x=T,all.y=F,sort=F)
  
  par(mfrow=c(1,2))
  plot(country.shape)
  points(fYrCty$cleanLong[fYrCty$cname==cname], fYrCty$cleanLat[fYrCty$cname==cname], pch=16)
  points(newprio$Longitude,newprio$Latitude, col=as.character(newprio$low), pch=16)
  plot(yData$year[yData$ccode==ccode],yData$NY.GDP.PCAP.KD.ZG_l0[yData$ccode==ccode],ylab="GDP Growth",xlab="Year",las=1,frame=F,type="l")
  abline(h=0,lty=2)
  lines(yData$year[yData$ccode==ccode],yData$NY.GDP.PCAP.KD.ZG_l0[yData$ccode==ccode])
  points(yData$year[yData$ccode==ccode],yData$NY.GDP.PCAP.KD.ZG_l0[yData$ccode==ccode],col=as.character(growth$low),pch=16)
}

plotConflict("SIERRA LEONE")
