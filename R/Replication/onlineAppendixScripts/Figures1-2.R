# Workspace
source('onlineAppendixScripts/setup.R')

# Load conflict country year data
load(paste0(pathData,'combinedData.rda'))
load(paste0(pathData,"cityTotPopLatLongvFinal.rda"))
prio = read.csv(paste0(pathData,"ConflictSite 4-2010_v3 Dataset.csv"), stringsAsFactors=F)

fYrCty$cname = toupper( countrycode( fYrCty$cname,"country.name","country.name") )
prio$Conflict.territory = toupper( countrycode( prio$Conflict.territory,"country.name","country.name") )
######################################################################
# Trace conflict over time
conflictMap = function(cname){
	worldmap=cshp(date=as.Date("1990-01-01"),useGW=F)
	ccode = countrycode(cname,"country.name","cown")
	cntryShape = worldmap[worldmap$COWCODE==ccode,]
	newprio = na.omit(prio[prio$Conflict.territory==cname,])

	gpclibPermit()
	ggmap = fortify(cntryShape, region = "COWCODE")
	ggmapData = data.frame("id" = unique(ggmap$id))
	ggmapData$id = as.numeric(as.character(ggmapData$id))

	tmp = ggplot()
	tmp = tmp + geom_map(data=ggmapData, aes(map_id = id), map=ggmap, fill='white', 
		linetype=1, colour='black') + expand_limits(x = ggmap$long, y = ggmap$lat) 
	tmp = tmp + geom_point(aes(
		x=fYrCty$cleanLong[fYrCty$cname==cname & fYrCty$Capital==0], 
		y=fYrCty$cleanLat[fYrCty$cname==cname & fYrCty$Capital==0]), 
		pch=17,size=4,col='darkgrey')
	tmp = tmp + geom_point(aes(
		x=fYrCty$cleanLong[fYrCty$cname==cname & fYrCty$Capital!=0], 
		y=fYrCty$cleanLat[fYrCty$cname==cname & fYrCty$Capital!=0]), 
		pch=18,size=5,col='darkgrey')
	tmp = tmp + geom_point(aes(x=newprio$Longitude, y=newprio$Latitude,
		color=newprio$Year),size=4)
	tmp = tmp + scale_colour_gradient('',
		low=brewer.pal(9,'Blues')[2],high=brewer.pal(9,'Blues')[9],
		breaks=seq(min(newprio$Year),max(newprio$Year),6))
	tmp = tmp + theme(
	  line=element_blank(),title=element_blank(),
	  axis.text.x=element_blank(),axis.text.y=element_blank(),
	  legend.position='top', legend.key.width=unit(4,"line"),
	  panel.grid.major=element_blank(), 
	  panel.grid.minor=element_blank(), panel.border=element_blank())
	return( tmp	)
}

## FIGURE 1 ##
pdf(file=paste0(pathGraphics, 'OnlineAppendix_Figure1.pdf'), width=5, height=7)
conflictMap("COLOMBIA")
dev.off()

## FIGURE 2 ##
pdf(file=paste0(pathGraphics, 'OnlineAppendix_Figure2.pdf'), width=5, height=7)
conflictMap("INDIA")
dev.off()
######################################################################