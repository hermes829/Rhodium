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
# Trace conflict over time
cname="MEXICO"
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

# Trace conflict over time UGLY
tracePrio=data.frame(matrix(NA, nrow=nrow(newprio), ncol=4, 
	dimnames=list(NULL,c('lon1','lat1','lon2','lat2'))))
pvars=c('Longitude', 'Latitude')
for(ii in 1:nrow(newprio)){
	t1 = newprio$Year[ii]; t2=t1+1; id = newprio$ID[ii]
	toPull=newprio[which(newprio$Year==t2 & newprio$ID==id),pvars]
	if(nrow(toPull)!=0){ tracePrio[ii,] = as.matrix(cbind(newprio[ii,pvars], toPull)) }
}

temp = ggplot(ggmapData, aes(map_id = id))
temp = temp + geom_map(map=ggmap, fill='white', 
	linetype=1, colour='black') + expand_limits(x = ggmap$long, y = ggmap$lat) 
temp = temp + geom_point(aes(
	x=fYrCty$cleanLong[fYrCty$cname==cname & fYrCty$Capital==0], 
	y=fYrCty$cleanLat[fYrCty$cname==cname & fYrCty$Capital==0]), 
	pch=17,size=4,col='black')
temp = temp + geom_point(aes(
	x=fYrCty$cleanLong[fYrCty$cname==cname & fYrCty$Capital!=0], 
	y=fYrCty$cleanLat[fYrCty$cname==cname & fYrCty$Capital!=0]), 
	pch=18,size=5,col='black')
temp = temp + geom_point(aes(x=newprio$Longitude, y=newprio$Latitude,
	color=newprio$Year),size=4)
# temp = temp + geom_segment(
# 	aes(x=tracePrio$lon1, xend=tracePrio$lon2, 
# 		y=tracePrio$lat1, yend=tracePrio$lat2), 
# 	color='black', arrow=arrow(length = unit(0.3,"cm")))
temp = temp + scale_colour_gradient('',
	low=brewer.pal(9,'Blues')[2],high=brewer.pal(9,'Blues')[9],
	breaks=newprio$Year[c(1,5,10,15,20)])
temp = temp + theme(
  line=element_blank(),title=element_blank(),
  axis.text.x=element_blank(),axis.text.y=element_blank(),
  legend.position='top', legend.key.width=unit(4,"line"),
  panel.grid.major=element_blank(), 
  panel.grid.minor=element_blank(), panel.border=element_blank())
temp
setwd(pathTex)
pdf(file=paste0(tolower(cname),'Map.pdf'), width=5, height=6)
temp
dev.off()
######################################################################