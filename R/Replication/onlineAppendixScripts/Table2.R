# Workspace
source('onlineAppendixScripts/setup.R')

# Load conflict country year data
load(paste0(pathData,'combinedData.rda')); modData=yData

# CREATE APPROPRIATE VARIABLES FOR REGRESSIONS
###################################################################
# Log transforming DVs
modData$lngdp_l0 = log(modData$gdp_l0)
modData$lngdp = log(modData$gdp)
modData$lngdpGr_l0 = (modData$lngdp_l0-modData$lngdp)/modData$lngdp_l0
modData$lngdpGr_l0 = modData$gdpGr_l0

# Transformations for conflict variables
modData$lnminDistACLED.mean <- log(modData$minDistACLED.mean+1)
modData$lncapDistACLED.mean <- log(modData$acledCapDist.mean+1)

# Transformations for other controls
modData$lngdpCap = log(modData$gdpCap)
modData$lninflation_l1 = logTrans(modData$inflation_l1)
modData$democ = as.numeric(modData$polity2>=6)
modData$polity2 = modData$polity2 + 11
###################################################################

## MODELS FOR GDP GROWTH (ANNUAL %)
###################################################################
# Model parameters
cntrls = c(
  'nconf','lninflation_l1', 
  'polity2', 'resourceGDP',  'gdpGr.mean_l0')

# Model specifications
acledForm = modForm(ivs=c('lnminDistACLED.mean', cntrls))
acledFormCap = modForm(ivs=c('lncapDistACLED.mean', cntrls))

# Run models
mAcled = lmer(acledForm, data = modData )
mAcledCap = lmer(acledFormCap, data=modData)

## TABLE 2 ##
starOut(paste0(pathGraphics,"OnlineAppendix_Table2.tex"), stargazer(mAcled, mAcledCap) )
###################################################################