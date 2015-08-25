if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
  source('~/Research/Rhodium/R/setup.R')}
if(Sys.info()["user"]=="Ben"){source('/Users/Ben/Github/Rhodium/R/setup.R')}

# Load conflict country year data
setwd(pathData)
# load('fullData.rda')
# modData = fullData # For full panel extensions
load('combinedData.rda')
modData=yData # For country-conflict-year extensions

# Gen tikz?
genTikz=FALSE

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
###################################################################

###################################################################
# R1: low GDP correlateds with periods of no conflict
# T-test between GDP and civwar
## we use logged measure of GDP to deal with outliers
# Across full panel
with(modData, t.test(lngdp~civwar, alternative='two.sided') )
###################################################################

###################################################################
# Interaction wont work
set.seed(6886)
fakeData = data.frame(y = rnorm(100), bin=rbinom(100,1,0.5), x=0)
fakeData$x[fakeData$bin==1] = rnorm(sum(fakeData$bin), .23, .25)
fakeData$i = fakeData$x*fakeData$bin
###################################################################

###################################################################
# Number of t observations by country
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

dv = 'lngdpGr_l0'
kivs = c('lnminDist.mean', 'lncapDist.mean')
cntrls = c(
  'Int.max',
  'durSt1max',  'confAreaProp', 'nconf', 
  'upperincome', 'lninflation_l1',  'polity2', 'resourceGDP',  'gdpGr.mean_l0')

vars = c(dv, kivs, cntrls, 'ccode', 'year')

slice = na.omit( modData[,vars] )

cntryCnts = table(slice$ccode)
length(cntryCnts[cntryCnts>5]) / length(cntryCnts)
length(cntryCnts[cntryCnts<=5]) / length(cntryCnts)
# ~53% of data has fewer than five observations per unit
###################################################################