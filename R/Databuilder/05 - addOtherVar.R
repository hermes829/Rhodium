####################
if(Sys.info()["user"]=="janus829"){source('/Users/janus829/Desktop/Research/Rhodium/R/setup.R')}
if(Sys.info()["user"]=="s7m"){source('/Users/s7m/Research/Rhodium/R/setup.R')}
if(Sys.info()["user"]=="Ben"){source('/Users/Ben/Github/Rhodium/R/setup.R')}

# Load conflict country year data
setwd(pathData)
load('countryYear_ConflictData.rda')

conVars=c('nconf', 'Int.max',
	'Conflict.area.sum',
	'territorial.max', 'territorial.mean',
	'minDist.min',  'inRadius.sum', 'capDist.min',
	'durSt1max')

yData=yData[,c('cyear','year','ccode',conVars)]
####################

####################
# Bring in WB data
# [5,] "BX.KLT.DINV.CD.WD"    		"Foreign direct investment, net inflows (BoP, current US$)"
# [12,] "AG.LND.TOTL.K2"    		"Land area (sq. km)"
# [76,] "NY.GDP.DEFL.KD.ZG"             "Inflation, GDP deflator (annual %)"
# [94,] "NY.GDP.PCAP.KD"                 "GDP per capita (constant 2000 US$)"
# [95,] "NY.GDP.PCAP.KD.ZG"             "GDP per capita growth (annual %)"
# [86,] "NY.GDP.MKTP.KD"                 "GDP (constant 2000 US$)"
# [87,] "NY.GDP.MKTP.KD.ZG"             "GDP growth (annual %)"
wbVars=c("BX.KLT.DINV.CD.WD","AG.LND.TOTL.K2", "NY.GDP.DEFL.KD.ZG",
	"NY.GDP.PCAP.KD", "NY.GDP.PCAP.KD.ZG",
	"NY.GDP.MKTP.KD", "NY.GDP.MKTP.KD.ZG")
wbData=WDI(country='all', indicator=wbVars,
	start=1988, end=2010, extra=T)
names(wbData)[4:10]=c('fdi', 'landArea', 'inflation', 'gdpCap',
	'gdpCapGr', 'gdp', 'gdpGr')

# Add cnames
wbData$cname = toupper(countrycode(wbData$iso2c, 'iso2c', 'country.name'))
wbData = wbData[!is.na(wbData$cname),]
wbData$ccode = panel$ccode[match(wbData$cname, panel$cname)]
wbData = wbData[!is.na(wbData$ccode),]

# Add upperincome dummy
wbData$upperincome=0
wbData$upperincome[which(wbData$income %in%
	c('High income: OECD', 'High income: nonOECD'  ) )] = 1

# Add world gdp growth as a covariate
gdpGrYr=summaryBy(gdpGr + gdpCapGr ~ year,
	data=wbData, FUN=mean, na.rm=T)
wbData = merge(wbData, gdpGrYr, by='year', all.x=T, all.y=F)

# Merging in wbData
# DVs
wbData$cyear = paste0(wbData$ccode, wbData$year-1)
yData = merge(yData, wbData[,c('cyear', 'gdpGr', 'gdpCapGr')],
	by='cyear', all.x=T, all.y=F )
colnames(yData)[13:14]=paste0(colnames(yData)[13:14], '_l0')
wbData$cyear = paste0(wbData$ccode, wbData$year)
yData = merge(yData, wbData[,c(4:7,9, 20:23 ) ] ,
	by='cyear', all.x=T, all.y=F)

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

polity$cname[polity$cname=="Czechoslovakia"]='CZECH REPUBLIC'
polity$cname[polity$cname=="Yugoslavia"]="SERBIA"
polity$ccode=panel$ccode[match(polity$cname, panel$cname)]
polity$cyear=paste0(polity$ccode,polity$year)

names(table(polity$cyear)[table(polity$cyear)>1])

# Combining datasets
yData=merge(yData, polity[,c('polity2','cyear')],
	by='cyear', all.x=T, all.y=F)
####################

####################
# Imputation
# Checks for lagdata function
mdl=yData
mdl$cyear=numSM(mdl$cyear)
lagVars=names(mdl)[4:25]

# Set up lags for sbgcop
mdl=lagDataSM(data=mdl,country_year='cyear',
	country='ccode',varsTOlag=lagVars,lag=1)
mdl=lagDataSM(data=mdl,country_year='cyear',
	country='ccode',
	varsTOlag=paste0('lag1_',lagVars),lag=1)
mdl=lagDataSM(data=mdl,country_year='cyear',
	country='ccode',
	varsTOlag=paste0('lag1_lag1_',lagVars),lag=1)
mdl=lagDataSM(data=mdl,country_year='cyear',
	country='ccode',
	varsTOlag=paste0('lag1_lag1_lag1_',lagVars),lag=1)
mdl=lagDataSM(data=mdl,country_year='cyear',
	country='ccode',
	varsTOlag=paste0('lag1_lag1_lag1_lag1_',lagVars),lag=1)
lagVarsAll=setdiff(colnames(mdl), colnames(yData))

# Impute missing value
# This takes time, set it and go for a run
sbgcopTimeSR = system.time(
  sbgData <- sbgcop.mcmc(
  	mdl[,c('ccode','year',lagVars,lagVarsAll)],
  	nsamp=5000,seed=123456, verb=TRUE)
  )

# Clean
impData=data.frame(
	cbind(
		cyear=mdl[,'cyear'],
		sbgData$Y.pmean[,c('ccode','year',lagVars)]
	)
)
####################

####################
# Save
setwd(pathData)
save(yData, impData, sbgcopTimeSR, file='combinedData.rda')
####################