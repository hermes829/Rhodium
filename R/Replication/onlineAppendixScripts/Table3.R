# Workspace
source('onlineAppendixScripts/setup.R')

###################################################################
# Functions and parameters
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
load(paste0(pathData,'combinedData_loInt.rda')); modData=yData
modData = prepData(modData)

mCityLo = lmer(ctyForm, data = modData )
mCapLo = lmer(capForm, data = modData )
###################################################################

###################################################################
# High intensity analysis
load(paste0(pathData,'combinedData_hiInt.rda')); modData=yData

modData = prepData(modData)

mCityHi = lmer(ctyForm, data = modData )
mCapHi = lmer(capForm, data = modData )
###################################################################

###################################################################
## TABLE 3 ##
starOut(paste0(pathGraphics,"OnlineAppendix_Table3.tex"), stargazer(mCityLo, mCityHi, mCapLo, mCapHi))
###################################################################