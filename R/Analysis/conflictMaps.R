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

######################################################################
# Aggregate GDP Growth and Distance
setwd(pathData)
load('combinedData.rda')
yData$lnminDist.min = log(yData$minDist.min+1)
yData$lncapDist.min = log(yData$capDist.min+1)

# Create bucketed version of mindist and find averag GDP gr
yData$cityDistCat=contToCat(yData$lnminDist.min, .25)
yData$capDistCat=contToCat(yData$lncapDist.min, .25)

# Conf intervals using summarySE:
gdpByCityDist=na.omit( summarySE(data=yData, measurevar='gdpGr_l0', 
	groupvars='cityDistCat', na.rm=TRUE) )
gdpByCapDist=na.omit( summarySE(data=yData, measurevar='gdpGr_l0', 
	groupvars='capDistCat', na.rm=TRUE) )

names(gdpByCityDist)[1]='distCat'; names(gdpByCapDist)[1]='distCat'
ggData=cbind(
	rbind(gdpByCityDist, gdpByCapDist),
	type=c(rep('Ln(Min. City Dist.)$_{t-1}$',nrow(gdpByCityDist)),
		rep('Ln(Min. Cap Dist.)$_{t-1}$',nrow(gdpByCapDist)) ),
	cut=rep( c('0-25$^{th}$','26-50$^{th}$','51-75$^{th}$','76-100$^{th}$'), 2 )
	)

tmp=ggplot(ggData, aes(x=cut, y=gdpGr_l0))
tmp=tmp + geom_bar(stat='identity', fill='grey')
tmp=tmp + geom_errorbar(aes(ymin=gdpGr_l0-se, ymax=gdpGr_l0+se),width=.2)
tmp=tmp + ylab("\\% $\\Delta$ Ln(GDP)$_{t}$")+xlab('')
tmp=tmp + facet_wrap(~type)
tmp=tmp + theme( axis.title.y=element_text(vjust=1),
  axis.ticks=element_blank(),legend.position='none',
  panel.grid.major=element_blank(), panel.grid.minor=element_blank() )
tmp
setwd(pathTex)
tikz(file='distGdp.tex', width=7, height=4, standAlone=FALSE)
tmp
dev.off()
######################################################################