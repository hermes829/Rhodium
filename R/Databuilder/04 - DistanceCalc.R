if(Sys.info()["user"]=="janus829"){source('/Users/janus829/Desktop/Research/Rhodium/R/setup.R')}
if(Sys.info()["user"]=="Ben"){source('/Users/Ben/Github/Rhodium/R/setup.R')}

# Load City pop data
setwd(pathData)
load("cityTotPopLatLongvFinal.rda")
source(paste0(pathMain,"/geodistance.R"))

# Load PRIO data and clean
# Clean PRIO data
setwd(paste0(pathData,'/PRIO - Conflict Site Data'))
prioData=read.csv("ConflictSite 4-2010_v3 Dataset.csv")
prioData$Conflict.territory=charSM(prioData$Conflict.territory)
prioData$Conflict.territory[prioData$Conflict.territory=='Yugoslavia']='Serbia'
prioData$Conflict.territory[prioData$Conflict.territory=='DRC']='Democratic Republic of Congo'
prioData$cname=toupper(countrycode(prioData$Conflict.territory, 'country.name','country.name'))

prioAC=read.csv('ucdp.prio.armed.conflict.v4.2013.csv')

prioData$idyear=paste0(prioData$ID, prioData$Year)
prioAC$idyear=paste0(prioAC$ID, prioAC$YEAR)
prioAC=merge(prioAC, prioData[,3:ncol(prioData)],
  by='idyear', all.x=T, all.y=T)
prioAC = prioAC[which(prioAC$ID %in% unique(prioData$ID)),]
prioAC=prioAC[which(prioAC$YEAR %in%  1989:2008),]
##################################################################

##################################################################
# calc duration
prioAC$startYr1=numSM(substr(charSM(prioAC$Startdate),1,4))
prioAC$startYr2=numSM(substr(charSM(prioAC$StartDate2),1,4))
##################################################################

##################################################################
# Calc distances from conflict sites
prioAC$cname[prioAC$Location=="Ethiopia"] <- "ETHIOPIA"
prioAC$cname[prioAC$Location=="Rwanda"] <- "RWANDA"
prioAC$cname[prioAC$Location=="Sierra Leone"] <- "SIERRA LEONE"
prioAC$cname[prioAC$Location=="Chad"] <- "CHAD"
prioAC <- prioAC[!is.na(prioAC$Latitude),]
prioAC <- prioAC[!prioAC$Latitude<(-360),]

# Calculate min dist of confict sites from cities in country
prioAC$minDist <- minDist(
  prioAC$Latitude, prioAC$Longitude, prioAC$cname, prioAC$YEAR,
  fYrCty$cleanLat, fYrCty$cleanLong, fYrCty$cname, fYrCty$YearAlmanac)

# Calculate number of cities within radius of conflict
prioAC$inRadius <- inRadius(
  prioAC$Latitude, prioAC$Longitude, prioAC$cname, prioAC$YEAR,
  fYrCty$cleanLat, fYrCty$cleanLong, fYrCty$cname, fYrCty$YearAlmanac,
  prioAC$Radius)$value

# Calculate distance from capital
prioAC$capDist <- minDist(
  prioAC$Latitude, prioAC$Longitude, prioAC$cname, prioAC$YEAR,
  fYrCty$cleanLat[fYrCty$Capital==1],
  fYrCty$cleanLong[fYrCty$Capital==1],
  fYrCty$cname[fYrCty$Capital==1],
  fYrCty$YearAlmanac[fYrCty$Capital==1])
##################################################################

# ##################################################################
# # Calc distances from natural resources
# # setwd(paste0(pathData,'/Horn - Giant Fields Data'))
# setwd(pathData)
# oil <- read.shapefile("Horn - Giant Fields Data/Giant_Fields_Data")
# oil <- as.data.frame(oil)
# newoil <- data.frame(stringsAsFactors=F)
# for(i in 1:nrow(oil))
# {
#   j=j+1
#   row <- data.frame(oil[i,])
#   dates <- oil$dbf.dbf.DISC_YR[i]:2014
#   newrows <- row[rep(1,length(dates)),]
#   newrows$dbf.dbf.DISC_YR <- dates
#   # newoil <- rbind(newoil,newrows)
#   # cat("\r",i)
#   if(i%%((nrow(oil)+4)/10)==0){cat(100*i/(nrow(oil)+4),"%  ", sep="")}
# }
# newoil <- newoil[,c("dbf.dbf.LAT_DD","dbf.dbf.LON_DD","dbf.dbf.FIELD_TYPE","dbf.dbf.SIZE_CLASS","dbf.dbf.COUNTRY","dbf.dbf.DISC_YR")]
# names(newoil) <- c("lat","long","type","size","country","year")
# newoil <- newoil[newoil$year%in%1988:2009,]
# newoil$country <- as.character(newoil$country)
# newoil$country[newoil$country=="Sierre Leone"] <- "Sierra Leone"
# newoil$country[which(newoil$country=="UAE")] <- "United Arab Emirates"
# newoil$country <- countrycode(newoil$country,"country.name","country.name")
# prioAC$oilRadius <- inRadius(prioAC$Latitude, prioAC$Longitude, prioAC$cname, prioAC$YEAR, newoil$lat, newoil$long, newoil$country, newoil$year, prioAC$Radius)
# # prioAC$oilRadius[is.na(prioAC$oilRadius)] <- 0
# ##################################################################

##################################################################
# Aggregate to the country-year
prioAC$territorial <- as.numeric(prioAC$Incomp%in%c(1,3))
prioAC <- prioAC[,c("ID","Incomp","Int","CumInt","territorial",
  "Conflict.area","Type","Region","minDist","inRadius","capDist",
	"cname","YEAR", 'startYr1', 'startYr2')]
prioAC$ccode=panel$ccode[match(prioAC$cname,panel$cname)]
prioAC$cyear=paste0(prioAC$ccode, prioAC$YEAR)
prioAC=prioAC[prioAC$Type!=2,]

# Aggregation options
aggAll=summaryBy(. ~ cyear, data=prioAC, FUN=c(mean,sum,min,max))

# Create country year
yData=aggAll[ ,c('cyear', 'YEAR.mean', 'ccode.mean',
                    'Int.mean', 'Int.max', 'CumInt.mean',
                    'CumInt.max', 'Type.mean',
                    'territorial.max','territorial.mean',
                    'Conflict.area.mean',
                    'Conflict.area.max','Conflict.area.sum',
                    'Region.mean', 'minDist.mean',
                    'minDist.min', 'inRadius.sum',
                    'inRadius.max', 'capDist.min',
                    'capDist.mean',
                    'startYr1.min','startYr1.max',
                    'startYr2.min', 'startYr2.max') ]
colnames(yData)[2:3] = c('year', 'ccode')

# Add duration variables
yData$durSt1max=yData$year - yData$startYr1.max
yData$durSt1min=yData$year - yData$startYr1.min
yData$durSt2max=yData$year - yData$startYr2.max
yData$durSt2min=yData$year - yData$startYr2.min

# Number of conflicts in country-year
numConf=data.frame(cbind(
  numSM( names(table(prioAC$cyear)) ),
  numSM( table(prioAC$cyear) ) ) )
yData$nconf=numConf$X2[match(yData$cyear, numConf$X1)]

# finding prio characteristics for conflicts that are mindist from major cities
prioMIN=NULL
for(ii in unique(prioAC$cyear)){
  a=prioAC[ which(prioAC$cyear %in% ii), ]

  if( nrow(a)!=1  ){
    b=a[ which(a$minDist==min(a$minDist)),  ]
  } else {  b=a  }

  prioMIN=rbind(prioMIN,b)
}

# Subsetting to relevant vars and renaming
prioMIN=prioMIN[,c('ID','Incomp','territorial','Int',
  'CumInt','Type', 'Conflict.area')]
colnames(prioMIN)=paste0(colnames(prioMIN),'_min')

# Merging into ydata
yData=cbind(yData, prioMIN)
##################################################################

##################################################################
# Saving aggregation of conflict data to country-year level
setwd(pathData)
save(yData, file='countryYear_ConflictData.rda')
##################################################################