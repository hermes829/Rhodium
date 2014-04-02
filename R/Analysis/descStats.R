# Workspace
if(Sys.info()["user"]=="janus829"){source('/Users/janus829/Desktop/Research/Rhodium/R/setup.R')}
if(Sys.info()["user"]=="Ben"){source('/Users/Ben/Github/Rhodium/R/setup.R')}


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

setwd(pathTex)
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

########################################################################
# Conflict and Growth country by country
setwd(pathMain)
source('vizResults.R')
setwd(paste0(pathData,'/PRIO - Conflict Site Data'))
prioAC=read.csv('ucdp.prio.armed.conflict.v4.2013.csv')
prioAC=prioAC[which(prioAC$Type %in% c(3,4)),]
prioAC=prioAC[which(prioAC$YEAR >= 1980),]
# Add cnames to this shit
prioAC$cname = countrycode(prioAC$SideA, 'country.name', 'country.name')
prioAC = prioAC[!is.na(prioAC$cname),]
prioAC$ccode = panel$ccode[match(prioAC$cname, panel$cname)]
prioAC = prioAC[!is.na(prioAC$ccode),]

# [94,] "NY.GDP.PCAP.KD"                 "GDP per capita (constant 2000 US$)"
# [95,] "NY.GDP.PCAP.KD.ZG"              "GDP per capita growth (annual %)"
# [86,] "NY.GDP.MKTP.KD"                 "GDP (constant 2000 US$)"  
# [87,] "NY.GDP.MKTP.KD.ZG"              "GDP growth (annual %)"

wbData=WDI(country='all', 
	indicator=c(
		"NY.GDP.PCAP.KD", "NY.GDP.PCAP.KD.ZG",
		"NY.GDP.MKTP.KD", "NY.GDP.MKTP.KD.ZG"), 
	start=1980, end=2010, extra=T)
# Add cnames to this shit
wbData$cname = countrycode(wbData$iso2c, 'iso2c', 'country.name')
wbData = wbData[!is.na(wbData$cname),]
wbData$ccode = panel$ccode[match(wbData$cname, panel$cname)]
wbData = wbData[!is.na(wbData$ccode),]

# Plotting
setwd(pathGraphics)
cntries=unique(prioAC$cname)
ggPlots=list()
for(ii in 1:length(cntries)){
	wbSlice=wbData[which(wbData$cname %in% cntries[ii]),c('year','NY.GDP.MKTP.KD.ZG')]
	colnames(wbSlice)[2]='econ'
	prioSlice=data.frame(numSM(prioAC[which(prioAC$cname %in% cntries[ii]),c('YEAR')]))
	colnames(prioSlice)='YEAR'
	prioSlice$min=min(wbSlice[,2],na.rm=T); prioSlice$max=max(wbSlice[,2],na.rm=T)

	temp=ggplot()+geom_line(data=wbSlice, aes(x=year, y=econ)) + ggtitle(cntries[ii])
	# temp=temp+geom_ribbon(data=prioSlice, aes(x=YEAR, ymin=min,ymax=max), alpha=0.3, fill='darkred')
	temp=temp+geom_vline(data=prioSlice,aes(xintercept=YEAR),col='darkred', size=1, alpha=0.3)

	ggPlots[[ii]]=temp
}

pdf(file='conflictGrowth.pdf')
for(ii in seq(1,84,9)){
	beg=ii; end=ii+8
	ggSlices=ggPlots[beg:end]
	multiplot(ggSlices, 3)
}
dev.off()
########################################################################