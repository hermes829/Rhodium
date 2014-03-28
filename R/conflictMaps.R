if(Sys.info()["user"]=="janus829"){source('/Users/janus829/Desktop/Research/Rhodium/R/setup.R')}
if(Sys.info()["user"]=="Ben"){source('/Users/Ben/Github/Rhodium/R/setup.R')}

# Load conflict country year data
setwd(pathData)
load('combinedData.rda')
load("cityTotPopLatLongvFinal.rda")
prio <- read.csv("PRIO - Conflict Site Data/ConflictSite 4-2010_v3 Dataset.csv",stringsAsFactors=F)
source(paste0(pathMain,"/geodistance.R"))

fYrCty$cname <- toupper(countrycode(fYrCty$cname,"country.name","country.name"))
prio$Conflict.territory <- toupper(countrycode(prio$Conflict.territory,"country.name","country.name"))

######################################################################
# Plot conflict spread and gdp
# plotConflict <- function(cname, threshold=1)
# {
#   worldmap <- cshp(date=as.Date("1990-01-01"),useGW=F)

#   ccode <- countrycode(cname,"country.name","cown")
#   country.shape <- worldmap[worldmap$COWCODE==ccode,]
#   # gradient <- colorRampPalette(c("#ffffcc","#c7e9b4","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#0c2c84"))
#   # years <- sort(prio$Year[prio$Conflict.territory==cname])
#   # years <- years-min(years)
  
#   growth <- data.frame(low=ifelse(yData$NY.GDP.PCAP.KD.ZG_l0[yData$ccode==ccode]<=1,"#d53e4f","#3288bd"),year=yData$year[yData$ccode==ccode])
#   newprio <- na.omit(prio[prio$Conflict.territory==cname,])
#   newprio <- merge(newprio,growth,by.x="Year",by.y="year",all.x=T,all.y=F,sort=F)
  
#   par(mfrow=c(1,2))
#   plot(country.shape)
#   points(fYrCty$cleanLong[fYrCty$cname==cname], fYrCty$cleanLat[fYrCty$cname==cname], pch=16)
#   points(newprio$Longitude,newprio$Latitude, col=as.character(newprio$low), pch=16)
#   plot(yData$year[yData$ccode==ccode],yData$NY.GDP.PCAP.KD.ZG_l0[yData$ccode==ccode],ylab="GDP Growth",xlab="Year",las=1,frame=F,type="l")
#   abline(h=0,lty=2)
#   lines(yData$year[yData$ccode==ccode],yData$NY.GDP.PCAP.KD.ZG_l0[yData$ccode==ccode])
#   points(yData$year[yData$ccode==ccode],yData$NY.GDP.PCAP.KD.ZG_l0[yData$ccode==ccode],col=as.character(growth$low),pch=16)
# }

# plotConflict("SIERRA LEONE")
# plotConflict("INDIA")
######################################################################

######################################################################
# Trace conflict over time
gradientLegend <- function(palette,lab1,lab2)
{
  xlim <- grconvertX(c(.15,.85),from="npc",to="user")
  ylim <- grconvertY(c(0,1),from="npc",to="user")
  xvals <- seq(xlim[1],xlim[2],length.out=11)[1:11]
  yvals <- seq(ylim[1],ylim[2],length.out=22)[1:2]
  rect(xvals[1:10],rep(yvals[1],10),xvals[2:11],rep(yvals[2],10),col=palette(10),border=palette(10))
  text(mean(xvals[1:2]),mean(yvals[1:2]),labels=lab1,col="black")
  text(mean(xvals[10:11]),mean(yvals[1:2]),labels=lab2,col="white")
}

plotConflict2 <- function(cname, threshold=1)
{
  worldmap <- cshp(date=as.Date("1990-01-01"),useGW=F)

  ccode <- countrycode(cname,"country.name","cown")
  country.shape <- worldmap[worldmap$COWCODE==ccode,]
  
  newprio <- na.omit(prio[prio$Conflict.territory==cname,])
  mypal <- colorRampPalette(c("#FEE5D9",'#FB6A4A',"#99000D"))
  newprio$col= mypal(nrow(newprio))
  
  par(mfrow=c(1,1))
  plot(country.shape)
  points(fYrCty$cleanLong[fYrCty$cname==cname], fYrCty$cleanLat[fYrCty$cname==cname], col='blue',pch=18, cex=1)
  points(newprio$Longitude,newprio$Latitude, col=as.character(newprio$col), pch=16, cex=1)

  gradientLegend(mypal,charSM(min(newprio$Year)),charSM(max(newprio$Year)))
}

plotConflict2("COLOMBIA")
######################################################################

cname="COLOMBIA"
worldmap=cshp(date=as.Date("1990-01-01"),useGW=F)
ccode <- countrycode(cname,"country.name","cown")
country.shape <- worldmap[worldmap$COWCODE==ccode,]

newprio <- na.omit(prio[prio$Conflict.territory==cname,])
mypal <- colorRampPalette(c("#FEE5D9",'#FB6A4A',"#99000D"))
newprio$col= mypal(nrow(newprio))

gpclibPermit()
ggmap = fortify(country.shape, region = "COWCODE")
ggmapData = data.frame("id" = unique(ggmap$id))
ggmapData$id = as.numeric(as.character(ggmapData$id))

temp = ggplot(ggmapData, aes(map_id = id))
temp = temp + geom_map(map=ggmap, fill='white', linetype=1, colour='black') + expand_limits(x = ggmap$long, y = ggmap$lat) 
temp = temp + geom_point(aes(x=fYrCty$cleanLong[fYrCty$cname==cname], y=fYrCty$cleanLat[fYrCty$cname==cname]))
temp = temp + geom_point(aes(x=newprio$Longitude, y=newprio$Latitude, color=newprio$Year))
temp = temp + scale_colour_gradient()
temp = temp + theme(legend.position='top', legend.key.width=unit(4,"line"))
temp