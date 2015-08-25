if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){ source('~/Research/Rhodium/R/setup.R')}
if(Sys.info()["user"]=="Ben"){source('/Users/Ben/Github/Rhodium/R/setup.R')}
# Load conflict country year data
setwd(pathData)
load('combinedData_Orig.rda'); modData=yData

# Gen tikz?
genTikz=FALSE

# CREATE APPROPRIATE VARIABLES FOR REGRESSIONS
###################################################################
# Log transforming DVs
modData$lngdp_l0 = log(modData$gdp_l0)
modData$lngdp = log(modData$gdp)
modData$lngdpGr_l0 = (modData$lngdp_l0-modData$lngdp)/modData$lngdp_l0
modData$lngdpGr_l0 = modData$gdpGr_l0

# Transformations for conflict variables
modData$lnminDist.min <- log(modData$minDist.min+1)
modData$Int.max <- modData$Int.max-1

# Transformations for other controls
modData$lngdpCap = log(modData$gdpCap)
modData$lninflation_l1 = logTrans(modData$inflation_l1)
modData$democ = as.numeric(modData$polity2>=6)
modData$polity2 = modData$polity2 + 11
###################################################################


## MODELS FOR GDP GROWTH (ANNUAL %)
###################################################################
# Model parameters
dv = 'gdpGr_l0'
kivs = 'lnminDist.min'
cntrls = c(
  'Int.max',
  'durSt1max',  'confAreaProp', 'nconf', 
  'upperincome', 'lninflation_l1',  'polity2', 'resourceGDP',  'gdpGr.mean_l0')

# Run random effect models
modData = na.omit( modData[,c(dv, kivs, cntrls, 'ccode','year')] )
length( unique(modData$ccode6) )
ctyForm=modForm(ivs=c(kivs, cntrls), type='random')
mCity = lmer(ctyForm, data = modData ); summary(mCity)$'coefficients'
###################################################################

###################################################################
library(stargazer)
stargazer(mCity)
###################################################################