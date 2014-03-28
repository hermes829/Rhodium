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
temp = temp + scale_colour_gradient('',breaks=newprio$Year[c(1,5,10,15,20)])
temp = temp + theme(
  line=element_blank(),title=element_blank(),
  axis.text.x=element_blank(),axis.text.y=element_blank(),
  legend.position='top', legend.key.width=unit(4,"line"),
  panel.grid.major=element_blank(), 
  panel.grid.minor=element_blank(), panel.border=element_blank())
setwd(pathTex)
pdf(file='colombiaMap.pdf', width=4, height=5)
temp
dev.off()
######################################################################