source('paperScripts/setup.R')

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
modData$lnminDist.min <- log(modData$minDist.min+1)
modData$lncapDist.min <- log(modData$capDist.min+1)
modData$lnminDist.mean <- log(modData$minDist.mean+1)
modData$lncapDist.mean <- log(modData$capDist.mean+1)
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

# Model specifications
ctyForm=modForm(ivs=c(kivs[1], cntrls), type='random')
capForm=modForm(ivs=c(kivs[2], cntrls), type='random')

# Run models
mCity = lmer(ctyForm, data = modData ); summary(mCity)$'coefficients'
mCap = lmer(capForm, data = modData ); summary(mCap)$'coefficients'
###################################################################

###################################################################
# Coefficient Plots
source('paperScripts/vizResults.R')

otherCovars=c(
  'Intensity$_{t-1}$', 
  'Duration$_{t-1}$', 'Area$_{t-1}$',
  'Number of conflicts$_{t-1}$',  
  'Upper Income', 'Ln(Inflation)$_{t-1}$', 'Democracy$_{t-1}$',
  'Resource Rents/GDP$_{t-1}$', 'World GDP Growth$_{t}$')

## FIGURE 4a ##
vnames=c('Ln(Min. City Dist.)$_{t-1}$', otherCovars)
tmp <- ggcoefplot(coefData=summary(mCity)$'coefficients',
    vars=na.omit(rownames(summary(mCity)$'coefficients')[2:100]), 
    varNames=vnames, Noylabel=FALSE, coordFlip=TRUE,
    colorGrey=FALSE, grSTA=0.5, grEND=0.1)
ggsave(file=paste0(pathGraphics,'Figure4a.pdf'), plot=tmp, width=4, height=6)

## FIGURE 4b ##
vnames=c('Ln(Min. Capital Dist.)$_{t-1}$', otherCovars)
tmp <- ggcoefplot(coefData=summary(mCap)$'coefficients',
    vars=na.omit(rownames(summary(mCap)$'coefficients')[2:100]), 
    varNames=vnames, Noylabel=FALSE, coordFlip=TRUE,
    colorGrey=FALSE, grSTA=0.5, grEND=0.1)
ggsave(file=paste0(pathGraphics,'Figure4b.pdf'), plot=tmp, width=4, height=6)
###################################################################

###################################################################
# Simulations
## FIGURE 5a ##
tmp = ggsimplot(modelResults=mCity, sims=10000, simData=modData, 
  vars=charSM(na.omit(rownames(summary(mCity)$'coefficients')[2:100])),
  vi='lnminDist.min', ostat=median, sigma=FALSE, intercept=TRUE,
  ylabel="\\% $\\Delta$ GDP$_{t}$", xlabel="Ln(Min. City Dist.)$_{t}$",
  specX=TRUE, ggxbreaks=seq(-1,7,1), plotType='ribbon'
  )
tmp=tmp + theme(axis.title.y=element_text(vjust=1))
ggsave(file=paste0(pathGraphics,'Figure5a.pdf'), plot=tmp, width=6, height=4)

## FIGURE 5b ##
tmp = ggsimplot(modelResults=mCap, sims=10000, simData=modData, 
  vars=na.omit(rownames(summary(mCap)$'coefficients')[2:100]),
  vi='lncapDist.min', ostat=median, sigma=FALSE, intercept=TRUE,
  ylabel="\\% $\\Delta$ GDP$_{t}$", xlabel="Ln(Min. Capital Dist.)$_{t}$",
  specX=TRUE, ggxbreaks=seq(-1,7,1), plotType='ribbon'
  )
tmp=tmp + theme(axis.title.y=element_text(vjust=1))
ggsave(file=paste0(pathGraphics,'Figure5b.pdf'), plot=tmp, width=6, height=4)
###################################################################