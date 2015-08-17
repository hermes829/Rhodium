if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
  source('~/Research/Rhodium/R/setup.R')}
if(Sys.info()["user"]=="Ben"){source('/Users/Ben/Github/Rhodium/R/setup.R')}

# Load conflict country year data
setwd(pathData)
load('fullData.rda')
load('combinedData.rda')
modData = fullData

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

# Transformations for other controls
modData$lngdpCap = log(modData$gdpCap)
modData$lninflation_l1 = logTrans(modData$inflation_l1)
modData$democ = as.numeric(modData$polity2>=6)
modData$polity2 = modData$polity2 + 11
###################################################################

###################################################################
# R1: low GDP correlateds with periods of no conflict
# T-test between GDP and civwar
## we use logged measure of GDP to deal with outliers
# Across full panel
with(modData, t.test(lngdp~civwar, alternative='two.sided') )
###################################################################