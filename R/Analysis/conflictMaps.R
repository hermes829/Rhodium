if(Sys.info()["user"]=="janus829"){source('~/Desktop/Research/Rhodium/R/setup.R')}
if(Sys.info()["user"]=="s7m"){source('~/Research/Rhodium/R/setup.R')}
if(Sys.info()["user"]=="Ben"){source('/Users/Ben/Github/Rhodium/R/setup.R')}

# Load conflict country year data
setwd(pathData)
load('combinedData.rda')
load("cityTotPopLatLongvFinal.rda")
prio = read.csv("PRIO - Conflict Site Data/ConflictSite 4-2010_v3 Dataset.csv",
	stringsAsFactors=F)
source(paste0(pathMain,"/geodistance.R"))

fYrCty$cname = toupper( countrycode(
	fYrCty$cname,"country.name","country.name") )
prio$Conflict.territory = toupper( countrycode(
	prio$Conflict.territory,"country.name","country.name") )

######################################################################
# Trace conflict over time
cname="INDIA"
worldmap=cshp(date=as.Date("1990-01-01"),useGW=F)
ccode = countrycode(cname,"country.name","cown")
cntryShape = worldmap[worldmap$COWCODE==ccode,]
newprio = na.omit(prio[prio$Conflict.territory==cname,])

gpclibPermit()
ggmap = fortify(cntryShape, region = "COWCODE")
ggmapData = data.frame("id" = unique(ggmap$id))
ggmapData$id = as.numeric(as.character(ggmapData$id))

temp = ggplot(ggmapData, aes(map_id = id))
temp = temp + geom_map(map=ggmap, fill='white', 
	linetype=1, colour='black') + expand_limits(x = ggmap$long, y = ggmap$lat) 
temp = temp + geom_point(aes(
	x=fYrCty$cleanLong[fYrCty$cname==cname & fYrCty$Capital==0], 
	y=fYrCty$cleanLat[fYrCty$cname==cname & fYrCty$Capital==0]), 
	pch=17,size=4,col='darkgrey')
temp = temp + geom_point(aes(
	x=fYrCty$cleanLong[fYrCty$cname==cname & fYrCty$Capital!=0], 
	y=fYrCty$cleanLat[fYrCty$cname==cname & fYrCty$Capital!=0]), 
	pch=18,size=5,col='darkgrey')
temp = temp + geom_point(aes(x=newprio$Longitude, y=newprio$Latitude,
	color=newprio$Year),size=4)
temp = temp + scale_colour_gradient('',
	low=brewer.pal(9,'Blues')[2],high=brewer.pal(9,'Blues')[9],
	breaks=seq(min(newprio$Year),max(newprio$Year),6))
temp = temp + theme(
  line=element_blank(),title=element_blank(),
  axis.text.x=element_blank(),axis.text.y=element_blank(),
  legend.position='top', legend.key.width=unit(4,"line"),
  panel.grid.major=element_blank(), 
  panel.grid.minor=element_blank(), panel.border=element_blank())
temp
setwd(pathTex)
# pdf(file=paste0(tolower(cname),'Map.pdf'), width=5, height=7)
# temp
# dev.off()
######################################################################