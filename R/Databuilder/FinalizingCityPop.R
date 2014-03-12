if(Sys.info()["user"]=="janus829"){source('/Users/janus829/Desktop/Research/Rhodium/R/setup.R')}
if(Sys.info()["user"]=="Ben"){source('/Users/Ben/Github/Rhodium/R/setup.R')}

load(paste0(pathData,"/cityTotPopLatLongv2.rda"))

##################################################################
# # Year to year contiguity of numbers
# cntry='UNITED STATES'
# slice=cityPop[which(cityPop$cname %in% cntry), c('cleanCity','YearAlmanac','Year','Population', 'TotPop', 'cleanLat', 'cleanLong')]
# slice[order(slice$cleanCity,slice$YearAlmanac),]
## too many discrepancies for now deciding to only focus on capital cities
##################################################################

##################################################################
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
##################################################################

##################################################################
# # Cntries listed per year
# check=lapply( unique(cityPopWar$Country), function(x) FUN=length(unique(cityPopWar[which(cityPopWar$Country %in% x),c('YearAlmanac')]) ) )
# cbind(unique(cityPopWar$Country),numSM(unlist(check)))

# slice=unique(cityPopWar[which(cityPopWar$cname %in% 'ALGERIA'), c('cname','YearAlmanac')])
# setdiff(1989:2008,slice[order(slice$YearAlmanac),'YearAlmanac'])

# # Cities listed per year
# cityStats=summaryBy(temp ~ YearAlmanac + Country, data=cityPopWar, FUN=sum)
# cityStats[order(cityStats$Country, cityStats$YearAlmanac), 1:3][1:100,]
##################################################################

##################################################################
# Create five year frame
fYrCty <- data.frame(Country=NULL,City=NULL,YearAlmanac=NULL,cleanCity=NULL,cleanCountry=NULL,cname=NULL,cleanLat=NULL,cleanLong=NULL,Capital=NULL)
# Make rolling-avg of cities (5 year)
for(ii in 1989:2008)
{
	citiesYear <- cityPopWar[cityPopWar$YearAlmanac%in%(ii-2):(ii+2),c("Country","City","YearAlmanac","cleanCity","cleanCountry","cname","cleanLat","cleanLong","Capital")]
	citiesYear <- citiesYear[!duplicated(citiesYear$cleanCity),]
	citiesYear$YearAlmanac <- ii
	fYrCty <- rbind(fYrCty,citiesYear)
}

# Quick fixes
fYrCty[which(fYrCty$cleanCity=='Lagos' & fYrCty$YearAlmanac %in% 1989:1990), 'Capital']=1
fYrCty[which(fYrCty$cleanCity=='Lagos' & fYrCty$YearAlmanac %in% 1991:2008), 'Capital']=0
fYrCty[which(fYrCty$cleanCity=='Colombo'), 'Capital']=1
fYrCty[which(fYrCty$cleanCity=='Yangon' & fYrCty$YearAlmanac %in% 2006:2008), 'Capital']=0
fYrCty[which(fYrCty$cleanCity=='Jakarta'), 'Capital']=1
fYrCty[which(fYrCty$cleanCity=='Kabul'), 'Capital']=1
fYrCty[which(fYrCty$cleanCity=='New Delhi'), 'Capital']=1
fYrCty[which(fYrCty$cleanCity=='Ankara'), 'Capital']=1
fYrCty[which(fYrCty$cleanCity=='Tbilisi'), 'Capital']=0
fYrCty[which(fYrCty$cleanCity=='Tbilisi') & fYrCty$cname=='GEORGIA', 'Capital']=1
tbilisi <- fYrCty[which(fYrCty$cleanCity=='Tbilisi' & fYrCty$YearAlmanac==2000),]
tbilisi <- rbind(tbilisi,tbilisi,tbilisi,tbilisi)
tbilisi$YearAlmanac <- 1991:1994
fYrCty <- rbind(fYrCty,tbilisi)
baku <- fYrCty[which(fYrCty$cleanCity=='Baku' & fYrCty$YearAlmanac==1997),]
baku <- rbind(baku, baku, baku, baku)
baku$YearAlmanac <- 1991:1994
fYrCty <- rbind(fYrCty,baku)
sarajevo <- fYrCty[which(fYrCty$cleanCity=='Sarajevo' & fYrCty$YearAlmanac==2000),]
sarajevo <- rbind(sarajevo, sarajevo, sarajevo, sarajevo)
sarajevo$YearAlmanac <- 1991:1994
fYrCty <- rbind(fYrCty,sarajevo)
zagreb <- fYrCty[which(fYrCty$cleanCity=='Zagreb' & fYrCty$YearAlmanac==2000),]
zagreb <- rbind(zagreb, zagreb, zagreb, zagreb)
zagreb$YearAlmanac <- 1991:1994
fYrCty <- rbind(fYrCty,zagreb)
# # Check on number of capitals per cntry
# stat=lapply( unique(fYrCty$cname), function(x) FUN=length(unique(fYrCty[which(fYrCty$cname %in% x & fYrCty$Capital %in% 1), c('cleanCity')])) )
# statM=cbind(unique(fYrCty$cname), stat); statM[statM[,2]>1,]
##################################################################

##################################################################
# Dealing with NAs
fYrCty=fYrCty[!is.na(fYrCty$Capital),]
##################################################################

##################################################################
# Saving cleaned final city data
save(fYrCty, file=paste0(pathData,"/cityTotPopLatLongvFinal.rda"))
##################################################################