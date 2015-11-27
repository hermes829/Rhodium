if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){ source('~/Research/Rhodium/R/setup.R')}
if(Sys.info()["user"]=="Ben"){source('/Users/Ben/Github/Rhodium/R/setup.R')}
# Load conflict country year data
setwd(pathData)
load('combinedData.rda'); modData=yData

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
modData$lncapDist.min <- log(modData$capDist.min+1)
modData$lnminDist.mean <- log(modData$minDist.mean+1)
modData$lncapDist.mean <- log(modData$capDist.mean+1)
modData$lnminDistACLED.min <- log(modData$minDistACLED.min+1)
modData$lnminDistACLED.mean <- log(modData$minDistACLED.mean+1)
modData$lncapDistACLED.min <- log(modData$acledCapDist.min+1)
modData$lncapDistACLED.mean <- log(modData$acledCapDist.mean+1)
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
kivs = c('lnminDist.min', 'lncapDist.min')
cntrls = c(
  'Int.max',
  'durSt1max',  'confAreaProp', 'nconf', 
  'upperincome', 'lninflation_l1',  'polity2', 'resourceGDP',  'gdpGr.mean_l0')

# Run dist models in pieces
loadPkg('magrittr')
mod1Any = modForm(ivs=c(kivs[1]), type='none') %>% lm(., data=modData)
mod2Any = modForm(ivs=c(kivs[1], cntrls), type='none') %>% lm(., data=modData)
mod3Any = modForm(ivs=c(kivs[1], cntrls), type='random', id=c('ccode')) %>% lmer(., data=modData)
mod4Any = modForm(ivs=c(kivs[1], cntrls), type='random', id=c('year')) %>% lmer(., data=modData)
mod5Any = modForm(ivs=c(kivs[1], cntrls), type='random', id=c('ccode', 'year')) %>% lmer(., data=modData)

mod1Cap = modForm(ivs=c(kivs[2]), type='none') %>% lm(., data=modData)
mod2Cap = modForm(ivs=c(kivs[2], cntrls), type='none') %>% lm(., data=modData)
mod3Cap = modForm(ivs=c(kivs[2], cntrls), type='random', id=c('ccode')) %>% lmer(., data=modData)
mod4Cap = modForm(ivs=c(kivs[2], cntrls), type='random', id=c('year')) %>% lmer(., data=modData)
mod5Cap = modForm(ivs=c(kivs[2], cntrls), type='random', id=c('ccode', 'year')) %>% lmer(., data=modData)
###################################################################

###################################################################
# Regression Tables
loadPkg('stargazer')
# Tabular depictions for PRIO city models
stargazer(mod1Any, mod2Any, mod3Any, mod4Any, mod5Any)

# Tabular depictions for ACLED city models
stargazer(mod1Cap, mod2Cap, mod3Cap, mod4Cap, mod5Cap)

# Tabular depictions for combo city models
stargazer(mod3Cap, mod3Any)

# Count number of observations
lapply(list(mod1Any, mod2Any, mod3Any, mod4Any, mod5Any), function(x) length(residuals(x)))
# Count number of countries
modData[,c(dv, kivs[1], 'ccode')] %>% na.omit(.) %>% .[,'ccode'] %>% unique() %>% length()
modData[,c(dv, kivs[1], cntrls, 'ccode')] %>% na.omit(.) %>% .[,'ccode'] %>% unique() %>% length()
###################################################################