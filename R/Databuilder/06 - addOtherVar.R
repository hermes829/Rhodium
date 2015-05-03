####################
if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){source('~/Research/Rhodium/R/setup.R')}
if(Sys.info()["user"]=="Ben"){source('/Users/Ben/Github/Rhodium/R/setup.R')}

# Load conflict country year and WB data
setwd(pathData)
all=TRUE # all intensity events
lo=FALSE # low intensity events
hi=FALSE # high intensity events
if(all){load('countryYear_ConflictData.rda')}
if(lo){load('countryYear_ConflictData_loInt.rda'); yData = yData_loInt}
if(hi){load('countryYear_ConflictData_hiInt.rda'); yData = yData_hiInt}

conVars=c('nconf', 'Int.max',
	'Conflict.area.sum',
	'Conflict.area.mean',
	'territorial.max', 'territorial.mean',
	'minDist.min', 'minDistACLED.min', 'inRadius.sum', 'capDist.min',
	'durSt1max')

yData=yData[,c('cyear','year','ccode',conVars)]
####################

####################
# Mods to WBdata
load('wbData.rda')

# Add upperincome dummy
wbData$upperincome=0
wbData$upperincome[which(wbData$income %in%
  c('High income: OECD', 'High income: nonOECD'  ) )] = 1

# Add world gdp growth as a covariate
gdpGrYr=summaryBy(gdpGr + gdpCapGr ~ year,
  data=wbData, FUN=mean, na.rm=T)
wbData = merge(wbData, gdpGrYr, by='year', all.x=T, all.y=F)

# Merging
wbData$cyear = paste0(wbData$ccode, wbData$year)
yData = merge(yData, wbData[,c(4:11,20:ncol(wbData))], 
	by='cyear', all.x=T, all.y=F)

wbData$cyear = paste0(wbData$ccode, wbData$year-1)
yData = merge(yData, wbData[,c(4:11,20:ncol(wbData))], 
	by='cyear', all.x=T, all.y=F, suffixes=c("_l0",""))

wbData$cyear = paste0(wbData$ccode, wbData$year+1)
yData = merge(yData, wbData[,c(4:11,20:ncol(wbData))], 
	by='cyear', all.x=T, all.y=F, suffixes=c("_l1",""))

# Create conflict area/land area var
yData$confAreaProp = yData$Conflict.area.sum/yData$landArea
yData$confAreaPropHi = ifelse(yData$confAreaProp>0.5, 1, 0)
####################

####################
# Polity data
setwd(pathData)
polity=read.csv('p4v2013.csv')
pvars=c('country','year','scode','polity2')
polity=polity[which(polity$year %in% 1989:2008),pvars]

# Remove duplicates
polity$drop = 0
polity[polity$scode=='ETH' & polity$year==1993, 'drop'] = 1
polity[polity$scode=='GDR' & polity$year>1988, 'drop'] = 1
polity[polity$scode=='GMY' & polity$year==1990, 'drop'] = 1
polity[polity$scode=='YGS' & polity$year==2006, 'drop'] = 1
polity[polity$scode=='YUG' & polity$year==1991, 'drop'] = 1
polity[polity$scode=='YAR' & polity$year>1988, 'drop'] = 1
polity[polity$scode=='YPR' & polity$year==1990, 'drop'] = 1
polity = polity[polity$drop==0,]
polity = polity[,1:(ncol(polity)-1)]

# Add country codes
polity$country=charSM(polity$country)
polity$country[polity$country=='Congo Brazzaville']='Republic Congo'
polity$country[polity$country=='Congo Kinshasa']='Democratic Congo'
polity$country[polity$country=='UAE']='United Arab Emirates'
polity$cname=toupper(countrycode(polity$country,'country.name','country.name'))

polity$cname[polity$cname=="CZECHOSLOVAKIA"]='CZECH REPUBLIC'
polity$cname[polity$cname=="YUGOSLAVIA"]="SERBIA"
polity$ccode=panel$ccode[match(polity$cname, panel$cname)]
polity$cyear=paste0(polity$ccode,polity$year)

names(table(polity$cyear)[table(polity$cyear)>1])

# Combining datasets
yData=merge(yData, polity[,c('polity2','cyear')],
	by='cyear', all.x=T, all.y=F)
####################

####################
# Save
setwd(pathData)
if(all){save(yData, file='combinedData.rda')}
if(lo){save(yData, file='combinedData_loInt.rda')}
if(hi){save(yData, file='combinedData_hiInt.rda')}
####################

####################
# Create full panel dataset
fullData = wbData

# Add lags
wbData$cyear = paste0(wbData$ccode, wbData$year-1)
fullData = merge(fullData, wbData[,c(4:11,20:ncol(wbData))], 
	by='cyear', all.x=T, all.y=F, suffixes=c("_l0",""))

wbData$cyear = paste0(wbData$ccode, wbData$year+1)
fullData = merge(fullData, wbData[,c(4:11,20:ncol(wbData))], 
	by='cyear', all.x=T, all.y=F, suffixes=c("_l1",""))

# Add in polity
fullData$polity2 = polity$polity2[match(fullData$cyear, polity$cyear)]

# Add in all conflict vars
setwd(pathData);load('countryYear_ConflictData.rda');yData=yData[,c('cyear',conVars)]
fullData = merge(fullData, yData, by='cyear', all.x=T, all.y=F)

# Correct NAs for some vars
fullData$nconf[is.na(fullData$nconf)] = 0

# Add dummy for civwar
fullData$civwar = 0
fullData$civwar[fullData$nconf>0] = 1

# Save
if(all){save(fullData, file='fullData.rda')}
if(lo){save(fullData, file='fullData_loInt.rda')}
if(hi){save(fullData, file='fullData_hiInt.rda')}
####################