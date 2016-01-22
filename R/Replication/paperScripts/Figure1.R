# Workspace
source('paperScripts/setup.R')

########################################################################
# World map of cities and conflicts
load(paste0(pathData, "cityTotPopLatLongvFinal.rda"))
prioData=read.csv(paste0(pathData, "ConflictSite 4-2010_v3 Dataset.csv"))
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

## FIGURE 1 ##
pdf(file=paste0(pathGraphics,'Figure1.pdf'), width=12, height=6)
plot(worldmap, col=mapColors)
points(fYrCty$cleanLong, fYrCty$cleanLat, col='blue', pch=18, cex=0.5)
points(prioData$Longitude,prioData$Latitude, col='red', pch=16,cex=0.5)
dev.off()
########################################################################