if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){ source('~/Research/Rhodium/R/setup.R')}
if(Sys.info()["user"]=="Ben"){source('/Users/Ben/Github/Rhodium/R/setup.R')}
# Load conflict country year data
setwd(pathData)
load('combinedData.rda'); modData=yData

# Gen tikz?
genTikz=FALSE

# CREATE APPROPRIATE VARIABLES FOR REGRESSIONS
###################################################################
logTrans=function(x){ log( x + abs(min(x, na.rm=T)) + 1) }

# Log transforming DVs
modData$lngdp_l0 = log(modData$gdp_l0)
modData$lngdp = log(modData$gdp)
modData$lngdpGr_l0 = (modData$lngdp_l0-modData$lngdp)/modData$lngdp_l0
modData$lngdpGr_l0 = modData$gdpGr_l0

# Transformations for conflict variables
modData$lnminDist.min <- log(modData$minDist.min+1)
modData$lncapDist.min <- log(modData$capDist.min+1)
modData$Int.max <- modData$Int.max-1

# Transformations for other controls
modData$lngdpCap = log(modData$gdpCap)
modData$lninflation_l1 = logTrans(modData$inflation_l1)
modData$democ = as.numeric(modData$polity2>=6)
modData$polity2 = modData$polity2 + 11

# Interaction term with distance and area
modData$minAreaSum = modData$lnminDist.min * modData$Conflict.area.sum
modData$minAreaMean = modData$lnminDist.min * modData$Conflict.area.mean
modData$capAreaSum = modData$lncapDist.min * modData$Conflict.area.sum
modData$capAreaMean = modData$lncapDist.min * modData$Conflict.area.mean

modData$minArea = modData$lnminDist.min * modData$confAreaProp
modData$capArea = modData$lncapDist.min * modData$confAreaProp
###################################################################

## MODELS FOR GDP GROWTH (ANNUAL %)
###################################################################
# Model parameters
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

dv = 'gdpGr_l0'
kivs = c('lnminDist.min', 'minArea', 'lncapDist.min', 'capArea')
# kivs = c('lnminDist.min', 'minAreaSum', 'lncapDist.min', 'capAreaSum')
# kivs = c('lnminDist.min', 'minAreaMean', 'lncapDist.min', 'capAreaMean')
cntrls = c(
  'Int.max',
  'durSt1max',  'confAreaProp', 'nconf', 
  'upperincome', 'lninflation_l1',  'polity2', 'resourceGDP',  'gdpGr.mean_l0')

# Run random effect models
ctyForm=modForm(ivs=c(kivs[1:2], cntrls), type='random')
capForm=modForm(ivs=c(kivs[3:4], cntrls), type='random')
mCity = lmer(ctyForm, data = modData ); summary(mCity)$'coefficients'
mCap = lmer(capForm, data = modData ); summary(mCap)$'coefficients'
###################################################################

###################################################################
# Create regression tables
library(stargazer)
stargazer(mCity, mCap)
###################################################################
