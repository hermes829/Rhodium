source("../setup.R")

load(paste0(pathData,"/cityTotPopLatLong.rda"))

# Year to year contiguity of numbers
cntry='UNITED STATES'
slice=cityPop[which(cityPop$cname %in% cntry), c('cleanCity','YearAlmanac','Year','Population', 'TotPop', 'cleanLat', 'cleanLong')]
slice[order(slice$cleanCity,slice$YearAlmanac),]

## too many discrepancies for now deciding to only focus on capital cities

######
# Subset to only countries listed in PRIO conflict site data

# Clean PRIO data
setwd(paste0(pathData,'/PRIO - Conflict Site Data'))
prioData=read.csv("ConflictSite 4-2010_v3 Dataset.csv")

prioData$Conflict.territory=charSM(prioData$Conflict.territory)
prioData$Conflict.territory[prioData$Conflict.territory=='Yugoslavia']='Serbia'
prioData$Conflict.territory[prioData$Conflict.territory=='DRC']='Democratic Republic of Congo'
prioData$cname=countrycode(prioData$Conflict.territory, 'country.name','country.name')


# Subset to prio countries only
prioCntries=unique(prioData$cname)
cityPopWar=cityPop[cityPop$cname %in% prioCntries,]

######

# Cities listed per year
cityPop$temp=1
cityStats=summaryBy(temp ~ YearAlmanac + Country, data=cityPop, FUN=sum)
cityStats[order(cityStats$Country, cityStats$YearAlmanac), 1:3]