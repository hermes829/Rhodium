if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){ source('~/Research/Rhodium/R/setup.R')}
if(Sys.info()["user"]=="Ben"){source('/Users/Ben/Github/Rhodium/R/setup.R')}

###################################################################
# Functions and parameters
# Gen tikz?
genTikz=FALSE
# CREATE APPROPRIATE VARIABLES FOR REGRESSIONS
prepData = function(modData){
	modData$lngdp_l0 = log(modData$gdp_l0)
	modData$lngdp = log(modData$gdp)
	modData$lngdpGr_l0 = (modData$lngdp_l0-modData$lngdp)/modData$lngdp_l0
	modData$lngdpGr_l0 = modData$gdpGr_l0

	# Transformations for conflict variables
	modData$lnminDist.min <- log(modData$minDist.min+1)
	modData$lncapDist.min <- log(modData$capDist.min+1)

	# Transformations for other controls
	modData$lngdpCap = log(modData$gdpCap)
	modData$lninflation_l1 = logTrans(modData$inflation_l1)
	modData$democ = as.numeric(modData$polity2>=6)
	modData$polity2 = modData$polity2 + 11
	return(modData)
}
# Model formulas
modForm = function(dv='gdpGr_l0', ivs, id='ccode', type='random'){
  base = paste(dv, paste(ivs, collapse=' + '), sep=' ~ ')
  if(type=='random'){
    eff = paste0('(1 |', id, ')')
    base = paste(base, eff, sep=' + ')
  }

  if(type=='fixed'){
    eff = paste0('factor(', id, ') - 1')
    base = paste(base, eff, sep=' + ')
  }
  return(formula(base))
}
# Log transformations
logTrans=function(x){ log( x + abs(min(x, na.rm=T)) + 1) }
# Model specifications
dv = 'gdpGr_l0'
kivs = c('lnminDist.min', 'lncapDist.min')
cntrls = c(
  'durSt1max',  'confAreaProp', 'nconf', 
  'upperincome', 'lninflation_l1',  'polity2', 'resourceGDP',  'gdpGr.mean_l0')

# Run random effect models
ctyForm=modForm(ivs=c(kivs[1], cntrls), type='random')
capForm=modForm(ivs=c(kivs[2], cntrls), type='random')
###################################################################

###################################################################
# Low intensity analysis
setwd(pathData)
load('combinedData_loInt.rda'); modData=yData
modData = prepData(modData)

mCityLo = lmer(ctyForm, data = modData ); summary(mCityLo)$'coefficients'
mCapLo = lmer(capForm, data = modData ); summary(mCapLo)$'coefficients'
###################################################################

###################################################################
# High intensity analysis
setwd(pathData)
load('combinedData_hiInt.rda'); modData=yData

modData = prepData(modData)

mCityHi = lmer(ctyForm, data = modData ); summary(mCityHi)$'coefficients'
mCapHi = lmer(capForm, data = modData ); summary(mCapHi)$'coefficients'
###################################################################

###################################################################
# Regression tables
library(stargazer)
stargazer(mCityLo, mCityHi, mCapLo, mCapHi)
###################################################################