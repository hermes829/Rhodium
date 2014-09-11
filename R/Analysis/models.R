if(Sys.info()["user"]=="janus829"){source('/Users/janus829/Desktop/Research/Rhodium/R/setup.R')}
if(Sys.info()["user"]=="s7m"){source('/Users/s7m/Research/Rhodium/R/setup.R')}
if(Sys.info()["user"]=="Ben"){source('/Users/Ben/Github/Rhodium/R/setup.R')}

# Load conflict country year data
setwd(pathData)
load('combinedData.rda')
library(plm)

# Dataset to run analysis on (impData or yData)
modData=yData
# modData=impData

# CREATE APPROPRIATE VARIABLES FOR REGRESSIONS
###################################################################
logTrans=function(x){ log( x + abs(min(x, na.rm=T)) + 1) }

# Log transforming DVs
modData$lngdpGr_l0 = logTrans(modData$gdpGr_l0)
modData$lngdpCapGr_l0 = logTrans(modData$gdpCapGr_l0)

# Transformations for conflict variables
modData$lnminDist.min <- log(modData$minDist.min+1)
modData$lncapDist.min <- log(modData$capDist.min+1)
modData$Int.max <- modData$Int.max-1
modData$lnConflict.area.sum = log(modData$Conflict.area.sum)

# Transformations for other controls
modData$lninflation_l1 = logTrans(modData$inflation_l1)
modData$lngdp = logTrans(modData$gdp)
modData$lngdpCap = logTrans(modData$gdpCap)
modData$democ = as.numeric(modData$polity>=6)

# Useful dummies
modData$USA <- as.numeric(modData$ccode==2)
modData$coldwar <- as.numeric(modData$year<1991)
###################################################################

## MODELS FOR GDP GROWTH (ANNUAL %)
###################################################################
mCity = lmer( lngdpGr_l0 ~ 
  lnminDist.min +
  Int.max + territorial.max + durSt1max + 
  # lnConflict.area.sum + log(landArea) +  
  confAreaPropHi +
  upperincome + lninflation_l1 + gdpGr.mean_l0 +
  democ + 
  (1|ccode), data = modData
 )
summary(mCity)

mCap = lmer( lngdpGr_l0 ~ 
  lncapDist.min +
  Int.max + territorial.max + durSt1max + 
  # lnConflict.area.sum + log(landArea) +  
  confAreaPropHi +  
  upperincome + lninflation_l1 + gdpGr.mean_l0 +
  democ + 
  (1|ccode), data = modData
 )
summary(mCap)
summary(modData$lngdpGr_l0); rmse(mCity); rmse(mCap)
###################################################################

###################################################################
# Divide intro training and test

###################################################################

###################################################################
# Coefficient Plots
setwd(pathMain)
source('vizResults.R')

coefs=c('upperincome', 'Int.max', 'lnArea', 'lnminDist.min',
	'territorial.max', 'durSt1max', 'NY.GDP.DEFL.KD.ZG_l1', 'lnAG.LND.TOTL.K2_l0')

vnames=c('Upper Income', 'Conflict Intensity$_{t}$', 'Ln(Conflict Area)$_{t}$', 'Ln(Min. Conflict Dist.)$_{t}$',
	'Conflict Type$_{t}$', 'Conflict Duration$_{t}$', 'Inflation$_{t-1}$', 'Ln(Land Area)')

temp <- ggcoefplot(coefData=summary(model3)@coefs,
    vars=coefs, varNames=vnames, Noylabel=FALSE, coordFlip=TRUE,
    specY=TRUE, ggylims=c(-7,5), ggybreaks=seq(-7,5,2),
    colorGrey=FALSE, grSTA=0.5, grEND=0.1)
temp
setwd(pathTex)
# tikz(file='mod3CoefPlot.tex', width=4, height=4, standAlone=F)
temp
# dev.off()
###################################################################

###################################################################
# Simulations
coefs=c('upperincome', 'Int.max', 'lnArea', 'lnminDist.min',
	'territorial.max', 'durSt1max', 'NY.GDP.DEFL.KD.ZG_l1', 'lnAG.LND.TOTL.K2_l0')
data=na.omit(modData[,coefs])
results=model3
estimates = results@fixef
varcov = vcov(results)
rownames(varcov) = names(estimates); colnames(varcov) = names(estimates)
RSS = sum(results@resid^2)
dfResid = nrow(data)-length(results@fixef) - length(results@ranef) + 1
# error = sqrt(RSS/dfResid)
error=0 # Set to zero to get rid of fundamental uncertainty
toTest = 'lnminDist.min'
tRange=seq(quantile(modData[,toTest])[1],quantile(modData[,toTest])[length(quantile(modData[,toTest]))], 0.1)

temp = ggsimplot(sims=10000, simData=data, vars=coefs,
  vi=toTest, vRange=tRange, ostat=median,
  betas=estimates, vcov=varcov, sigma=error, intercept=TRUE,
  ylabel="\\% $\\Delta$ GDP$_{t}$", xlabel="Ln(Min. Conflict Dist.)$_{t}$",
  specX=TRUE, ggxbreaks=seq(-1,7,1), plotType='ribbon'
  # specY=TRUE, ggybreaks=seq(0,12,2), ggylims=c(2,12)
  )
temp
setwd(pathTex)
# tikz(file='mod3SimPlot.tex', width=6, height=4, standAlone=F)
temp
# dev.off()
###################################################################