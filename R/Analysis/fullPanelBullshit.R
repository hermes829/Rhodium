if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
  source('~/Research/Rhodium/R/setup.R')}
if(Sys.info()["user"]=="Ben"){source('/Users/Ben/Github/Rhodium/R/setup.R')}

# Load conflict country year data
setwd(pathData)
load('fullData.rda')
load('combinedData.rda')
modData = fullData

# Gen tikz?
genTikz=TRUE

# CREATE APPROPRIATE VARIABLES FOR REGRESSIONS
###################################################################
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

## MODELS FOR GDP GROWTH (ANNUAL %)
###################################################################

# Full model
completeData <- modData[,c("gdpGr_l0","civwar","lnminDist.min","lncapDist.min","ccode","upperincome","lninflation_l1","polity2","resourceGDP","gdpGr.mean_l0")]
completeData <- completeData[complete.cases(completeData),]
summary(lmer(gdpGr_l0 ~ civwar + lnminDist.min + upperincome + lninflation_l1 + polity2 + resourceGDP + gdpGr.mean_l0 + (1|ccode), data=completeData))
summary(lmer(gdpGr_l0 ~ civwar + lncapDist.min + upperincome + lninflation_l1 + polity2 + resourceGDP + gdpGr.mean_l0 + (1|ccode), data=completeData))
summary(lm(gdpGr_l0 ~ civwar + lnminDist.min + upperincome + lninflation_l1 + polity2 + resourceGDP + gdpGr.mean_l0 + as.factor(ccode), data=completeData))
summary(lm(gdpGr_l0 ~ civwar + lncapDist.min + upperincome + lninflation_l1 + polity2 + resourceGDP + gdpGr.mean_l0 + as.factor(ccode), data=completeData))
