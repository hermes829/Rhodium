###################################################################
# Load setup
source('paperScripts/setup.R')

# Load conflict country year data
load(paste0(pathData,'fullData.rda'))
load(paste0(pathData,'combinedData.rda'))
modData = fullData
###################################################################

###################################################################
# CREATE APPROPRIATE VARIABLES FOR REGRESSIONS
# Log transforming DVs
modData$lngdp_l0 = log(modData$gdp_l0)
modData$lngdp = log(modData$gdp)
modData$lngdpGr_l0 = (modData$lngdp_l0-modData$lngdp)/modData$lngdp_l0
modData$lngdpGr_l0 = modData$gdpGr_l0

# Transformations for other controls
modData$lngdpCap = log(modData$gdpCap)
modData$lninflation_l1 = logTrans(modData$inflation_l1)
modData$democ = as.numeric(modData$polity2>=6)
modData$polity2 = modData$polity2 + 11

# Transformations for conflict variables
modData$lnminDist.min <- log(modData$minDist.min+1)
modData$lnminDist.min[is.na(modData$lnminDist.min)] <- 0
modData$lncapDist.min <- log(modData$capDist.min+1)
modData$lncapDist.min[is.na(modData$lncapDist.min)] <- 0
modData$lnminDist.mean <- log(modData$minDist.mean+1)
modData$lncapDist.mean <- log(modData$capDist.mean+1)
modData$lnminDistACLED.min <- log(modData$minDistACLED.min+1)
modData$lnminDistACLED.mean <- log(modData$minDistACLED.mean+1)
modData$lncapDistACLED.min <- log(modData$acledCapDist.min+1)
modData$lncapDistACLED.mean <- log(modData$acledCapDist.mean+1)
modData$Int.max <- modData$Int.max-1
###################################################################

###################################################################
# Full model piecewise
regData <- modData[,c("gdpGr_l0","civwar","lnminDist.min","lncapDist.min","ccode","upperincome","lninflation_l1","polity2","resourceGDP","gdpGr.mean_l0",'year')]
regData <- regData[complete.cases(regData),]

## TABLE 2 ##
# Run first for any city distance
# Pooled Model
min1 = lm(
	gdpGr_l0 ~ civwar + lnminDist.min +
	lninflation_l1 + polity2 + resourceGDP + gdpGr.mean_l0, 
	data=regData)
# Country fixed effects
min2 = plm(
	gdpGr_l0 ~ civwar + lnminDist.min +
	lninflation_l1 + polity2 + resourceGDP + gdpGr.mean_l0, 
	data=regData, model='within', effect='individual', index=c('ccode','year'))
# Country + year fixed effects
min3 = plm(
	gdpGr_l0 ~ civwar + lnminDist.min +
	lninflation_l1 + polity2 + resourceGDP, 
	data=regData, model='within', effect='twoways', index=c('ccode','year'))
# Create stargazer tables
starOut(paste0(pathGraphics,"Table2.tex"), stargazer(min1, min2, min3))
# Number of countries
length(unique(regData$ccode))

## TABLE 3 ##
# Same analysis for capital distance
# Pooled Model
cap1 = lm(
	gdpGr_l0 ~ civwar + lncapDist.min +
	lninflation_l1 + polity2 + resourceGDP + gdpGr.mean_l0, 
	data=regData)
# Country Fixed Effects
cap2 = plm(
	gdpGr_l0 ~ civwar + lncapDist.min +
	lninflation_l1 + polity2 + resourceGDP + gdpGr.mean_l0, 
	data=regData, model='within', effect='individual', index=c('ccode','year'))
# Year Fixed Effects
cap3 = plm(
	gdpGr_l0 ~ civwar + lncapDist.min +
	lninflation_l1 + polity2 + resourceGDP, 
	data=regData, model='within', effect='twoways', index=c('ccode','year'))
# Create stargazer tables
starOut(paste0(pathGraphics,"Table3.tex"), stargazer(cap1, cap2, cap3))
# Number of countries
length(unique(regData$ccode))
###################################################################