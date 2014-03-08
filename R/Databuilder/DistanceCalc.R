if(Sys.info()["user"]=="janus829"){source('/Users/janus829/Desktop/Research/Rhodium/R/setup.R')}
if(Sys.info()["user"]=="Ben"){source('/Users/Ben/Github/Rhodium/R/setup.R')}

# Load City pop data
setwd(pathData)
load("cityTotPopLatLongvFinal.rda")

# Load PRIO data and clean
# Clean PRIO data
setwd(paste0(pathData,'/PRIO - Conflict Site Data'))
prioData=read.csv("ConflictSite 4-2010_v3 Dataset.csv")
prioData$Conflict.territory=charSM(prioData$Conflict.territory)
prioData$Conflict.territory[prioData$Conflict.territory=='Yugoslavia']='Serbia'
prioData$Conflict.territory[prioData$Conflict.territory=='DRC']='Democratic Republic of Congo'
prioData$cname=countrycode(prioData$Conflict.territory, 'country.name','country.name')

prioAC=read.csv('ucdp.prio.armed.conflict.v4.2013.csv')

prioData$idyear=paste0(prioData$ID, prioData$Year)
prioAC$idyear=paste0(prioAC$ID, prioAC$YEAR)
prioAC=merge(prioAC, prioData[,3:ncol(prioData)], by='idyear', all.x=T, all.y=T)
prioAC = prioAC[which(prioAC$ID %in% unique(prioData$ID)),]
prioAC=prioAC[which(prioAC$YEAR %in%  1989:2008),]

prioAC[which(prioAC$ID %in% 6),]
prioData[which(prioData$ID %in% 6),]
##################################################################

##################################################################
# Calc distances from conflict sites

##################################################################