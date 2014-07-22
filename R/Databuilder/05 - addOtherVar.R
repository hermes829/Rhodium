####################
if(Sys.info()["user"]=="janus829"){source('/Users/janus829/Desktop/Research/Rhodium/R/setup.R')}
if(Sys.info()["user"]=="Ben"){source('/Users/Ben/Github/Rhodium/R/setup.R')}

# Load conflict country year data
setwd(pathData)
load('countryYear_ConflictData.rda')
# keeping original version for country year dataset
conData=yData 
####################

####################
# Bring in WB data
# WDIsearch()
# [5,] "BX.KLT.DINV.CD.WD"    			 "Foreign direct investment, net inflows (BoP, current US$)" 
# [12,] "AG.LND.TOTL.K2"    			 "Land area (sq. km)"
# [76,] "NY.GDP.DEFL.KD.ZG"              "Inflation, GDP deflator (annual %)"
# [94,] "NY.GDP.PCAP.KD"                 "GDP per capita (constant 2000 US$)"
# [95,] "NY.GDP.PCAP.KD.ZG"              "GDP per capita growth (annual %)"
# [86,] "NY.GDP.MKTP.KD"                 "GDP (constant 2000 US$)"  
# [87,] "NY.GDP.MKTP.KD.ZG"              "GDP growth (annual %)"

wbVars=c("BX.KLT.DINV.CD.WD","AG.LND.TOTL.K2", 
		"NY.GDP.DEFL.KD.ZG", 
		"NY.GDP.PCAP.KD", "NY.GDP.PCAP.KD.ZG", 
		"NY.GDP.MKTP.KD", "NY.GDP.MKTP.KD.ZG")
wbData=WDI(country='all', indicator=wbVars, 
	start=1988, end=2010, extra=T)

# Add cnames
wbData$cname = countrycode(wbData$iso2c, 'iso2c', 'country.name')
wbData = wbData[!is.na(wbData$cname),]
wbData$ccode = panel$ccode[match(wbData$cname, panel$cname)]
wbData = wbData[!is.na(wbData$ccode),]
####################

####################
# Merge into country conflict data
# Lagging all data
wbData$cyear = paste0(wbData$ccode, wbData$year-1)
yData = merge(yData, wbData[,c(4:11,17,ncol(wbData))], 
	by='cyear', all.x=T, all.y=F, suffixes=c("_l0",""))
wbData$cyear = paste0(wbData$ccode, wbData$year+1)
yData = merge(yData, wbData[,c(4:11,17,ncol(wbData))], 
	by='cyear', all.x=T, all.y=F, suffixes=c("_l1","_p1"))
wbData$cyear = paste0(wbData$ccode, wbData$year)
yData = merge(yData, wbData[,c(4:11,17,ncol(wbData))], 
	by='cyear', all.x=T, all.y=F)
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
polity$cname=countrycode(polity$country,'country.name','country.name')

polity$cname[polity$cname=="Czechoslovakia"]='CZECH REPUBLIC'
polity$cname[polity$cname=="Yugoslavia"]="SERBIA"
polity$ccode=panel$ccode[match(polity$cname, panel$cname)]
polity$cyear=paste0(polity$ccode,polity$year)

check=names(table(polity$cyear)[table(polity$cyear)>1])
####################

####################
# Combining datasets
base=panel[which(panel$year %in% 1989:2008),]
cyData=merge(base, polity[,c('polity2','cyear')], 
	by.x='ccodeYear', by.y='cyear', all.x=T, all.y=F)
cyData=merge(cyData, wbData[,c(wbVars,'cyear')],
	by.x='ccodeYear', by.y='cyear', all.x=T, all.y=F)

# Set up inverse distance var in conflict data
conData$IminDist.mean=1/(conData$minDist.mean)
conData$IminDist.min=1/(conData$minDist.min)
conData$IcapDist.mean=1/(conData$capDist.mean)
conData$IcapDist.min=1/(conData$capDist.min)

# Merge select conflict vars
conVars=c('nconf', 'Int.mean', 'Int.max', 
	'Conflict.area.mean', 'Conflict.area.max',
	'IminDist.mean', 'IminDist.min', 'inRadius.sum', 
	'IcapDist.min', 'IcapDist.mean','durSt2max')
cyData=merge(cyData, conData[,c('cyear',conVars)],
	by.x='ccodeYear', by.y='cyear', all.x=T, all.y=F)

# Turn all NAs in merged conflict vars to zero
for(var in conVars){
	cyData[,var][is.na(cyData[,var])]=0
	cyData$nconf[is.na(cyData$nconf)]
}
####################

####################
# Imputation
# Checks for lagdata function
mdl=cyData
mdl$ccodeYear=numSM(mdl$ccodeYear)
lagVars=names(mdl)[9:16]
otherVars=setdiff(colnames(cyData),lagVars)[9:19]

# Set up lags for sbgcop
mdl=lagDataSM(data=mdl,country_year='ccodeYear',
	country='ccode',varsTOlag=lagVars,lag=1)
mdl=lagDataSM(data=mdl,country_year='ccodeYear',
	country='ccode',
	varsTOlag=paste0('lag1_',lagVars),lag=1)
mdl=lagDataSM(data=mdl,country_year='ccodeYear',
	country='ccode',
	varsTOlag=paste0('lag1_lag1_',lagVars),lag=1)
mdl=lagDataSM(data=mdl,country_year='ccodeYear',
	country='ccode',
	varsTOlag=paste0('lag1_lag1_lag1_',lagVars),lag=1)
mdl=lagDataSM(data=mdl,country_year='ccodeYear',
	country='ccode',
	varsTOlag=paste0('lag1_lag1_lag1_lag1_',lagVars),lag=1)
lagVarsAll=setdiff(colnames(mdl), colnames(cyData))

# Impute missing value
# This takes time, set it and go for a run
sbgcopTimeSR = system.time(
  sbgData <- sbgcop.mcmc(
  	mdl[,c('ccode','year',lagVars,lagVarsAll)], nsamp=6000,
  	seed=123455, verb=TRUE) 
  )

# Clean
impData=data.frame(
	cbind(
		ccodeYear=mdl[,'ccodeYear'],
		sbgData$Y.pmean[,c('ccode','year',lagVars)],
		mdl[,otherVars]
	)
)
####################

####################
# Save
setwd(pathData)
save(yData, cyData, impData, file='combinedData.rda')
####################