# Workspace
if(Sys.info()["user"]=="janus829"){source('/Users/janus829/Desktop/Research/Rhodium/R/setup.R')}
if(Sys.info()["user"]=="Ben"){source('/Users/Ben/Github/Rhodium/R/setup.R')}


######################################################################
# Aggregate GDP Growth and Distance
setwd(pathData)
load('combinedData.rda')

# Create bucketed version of mindist and find averag GDP gr
yData$cityDistCat=contToCat(yData$minDist.min, .25)
yData$capDistCat=contToCat(yData$capDist.min, .25)

# Conf intervals using summarySE:
gdpByCityDist=na.omit( summarySE(data=yData, measurevar='gdpGr_l0', 
	groupvars='cityDistCat', na.rm=TRUE) )
gdpByCapDist=na.omit( summarySE(data=yData, measurevar='gdpGr_l0', 
	groupvars='capDistCat', na.rm=TRUE) )

names(gdpByCityDist)[1]='distCat'; names(gdpByCapDist)[1]='distCat'
ggData=cbind(
	rbind(gdpByCityDist, gdpByCapDist),
	type=c(rep('Min. City Dist.$_{t-1}$',nrow(gdpByCityDist)),
		rep('Min. Capital Dist.$_{t-1}$',nrow(gdpByCapDist)) ),
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
setwd(pathGraphics)
# tikz(file='distGdp.tex', width=7, height=4, standAlone=FALSE)
# tmp
# dev.off()
######################################################################

########################################################################
# World map of cities and conflicts
setwd(pathData)
load("cityTotPopLatLongvFinal.rda")
setwd(paste0(pathData,'/PRIO - Conflict Site Data'))
prioData=read.csv("ConflictSite 4-2010_v3 Dataset.csv")
prioData$Conflict.territory=charSM(prioData$Conflict.territory)
prioData$Conflict.territory[prioData$Conflict.territory=='Yugoslavia']='Serbia'
prioData$Conflict.territory[prioData$Conflict.territory=='DRC']='Democratic Republic of Congo'
prioData$cname=countrycode(prioData$Conflict.territory, 'country.name','country.name')
cntries=unique(prioData$cname)

# Color Non-Conflict countries
worldmap=cshp(as.Date('2000-1-1'))
worldmap$CNTRY_NAME=charSM(worldmap$CNTRY_NAME)
worldmap$CNTRY_NAME[worldmap$CNTRY_NAME=='Congo, DRC']='Congo, Democratic Republic of'
Wcntries=worldmap$CNTRY_NAME
Wcntries=panel$cname[match(Wcntries, panel$CNTRY_NAME)]
noConfCntries=setdiff(Wcntries, cntries)
mapColors=rep('white',length(Wcntries))
mapColors[which(Wcntries %in% noConfCntries)] = 'grey'

setwd(pathGraphics)
# pdf(file='CityConfMap.pdf', width=12, height=6)
plot(worldmap, col=mapColors)
points(fYrCty$cleanLong, fYrCty$cleanLat, col='blue', pch=18, cex=0.5)
points(prioData$Longitude,prioData$Latitude, col='red', pch=16,cex=0.5)
# dev.off()
########################################################################

########################################################################
# Some stats on the city data
setwd(pathData)
load("cityTotPopLatLongvFinal.rda")

# Average cities listed by cntry and year
fYrCty$temp=1
cityStats=summaryBy(temp ~ Country + YearAlmanac, data=fYrCty, FUN=sum)
cityGraph=summaryBy(temp.sum ~ YearAlmanac, data=cityStats, FUN=mean)
temp=ggplot(cityGraph, aes(x=YearAlmanac, y=temp.sum.mean)) + geom_line()
temp
########################################################################