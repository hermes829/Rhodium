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

# Model specifications
ctyForm=modForm(ivs=c(kivs[1], cntrls), type='random')
capForm=modForm(ivs=c(kivs[2], cntrls), type='random')
acledForm = modForm(ivs=c('lnminDistACLED.mean', cntrls[4:length(cntrls)]))
acledFormCap = modForm(ivs=c('lncapDistACLED.mean', cntrls[4:length(cntrls)]))

# Run models
mCity = lmer(ctyForm, data = modData ); summary(mCity)$'coefficients'
mCap = lmer(capForm, data = modData ); summary(mCap)$'coefficients'
mAcled = lmer(acledForm, data = modData ); summary(mAcled)$'coefficients'
mAcledCap = lmer(acledFormCap, data=modData); summary(mAcledCap)$'coefficients'

# Basic model diags
rmse=function(x){sqrt( mean( (residuals(x)^2) ) )}

Reduce('-',quantile(modData$lngdpGr_l0,c(0.75,0.25),na.rm=T))
rmse(mCity); rmse(mCap)
###################################################################

###################################################################
# Coefficient Plots
setwd(pathMain)
source('vizResults.R')

otherCovars=c(
  'Intensity$_{t-1}$', 
  'Duration$_{t-1}$', 'Area$_{t-1}$',
  'Number of conflicts$_{t-1}$',  
  'Upper Income', 'Ln(Inflation)$_{t-1}$', 'Democracy$_{t-1}$',
  'Resource Rents/GDP$_{t-1}$', 'World GDP Growth$_{t}$')

vnames=c('Ln(Min. City Dist.)$_{t-1}$', otherCovars)
temp <- ggcoefplot(coefData=summary(mCity)$'coefficients',
    vars=na.omit(rownames(summary(mCity)$'coefficients')[2:100]), 
    varNames=vnames, Noylabel=FALSE, coordFlip=TRUE,
    colorGrey=FALSE, grSTA=0.5, grEND=0.1)
setwd(pathGraphics)
if(genTikz){ tikz(file='mCityCoefPlot.tex', width=4, height=6, standAlone=F)}
temp
if(genTikz){ dev.off() }

vnames=c('Ln(Min. Capital Dist.)$_{t-1}$', otherCovars)
temp <- ggcoefplot(coefData=summary(mCap)$'coefficients',
    vars=na.omit(rownames(summary(mCap)$'coefficients')[2:100]), 
    varNames=vnames, Noylabel=FALSE, coordFlip=TRUE,
    colorGrey=FALSE, grSTA=0.5, grEND=0.1)
setwd(pathGraphics)
if(genTikz){ tikz(file='mCapCoefPlot.tex', width=4, height=6, standAlone=F)}
temp
if(genTikz){ dev.off() }

# Create tables for ACLED results
library(stargazer)
stargazer(mAcled, mAcledCap)
# Number of countries included
slice = na.omit( modData[,c('ccode','lnminDistACLED.mean', cntrls[4:length(cntrls)])] )
length(unique(slice$ccode))
###################################################################

###################################################################
# Simulations
temp = ggsimplot(modelResults=mCity, sims=10000, simData=modData, 
  vars=charSM(na.omit(rownames(summary(mCity)$'coefficients')[2:100])),
  vi='lnminDist.min', ostat=median, sigma=FALSE, intercept=TRUE,
  ylabel="\\% $\\Delta$ GDP$_{t}$", xlabel="Ln(Min. City Dist.)$_{t}$",
  specX=TRUE, ggxbreaks=seq(-1,7,1), plotType='ribbon'
  )
temp=temp + theme(axis.title.y=element_text(vjust=1))
setwd(pathGraphics)
if(genTikz){ tikz(file='mCitySimPlot.tex', width=6, height=4, standAlone=F)}
temp
if(genTikz){ dev.off() }

temp = ggsimplot(modelResults=mCap, sims=10000, simData=modData, 
  vars=na.omit(rownames(summary(mCap)$'coefficients')[2:100]),
  vi='lncapDist.min', ostat=median, sigma=FALSE, intercept=TRUE,
  ylabel="\\% $\\Delta$ GDP$_{t}$", xlabel="Ln(Min. Capital Dist.)$_{t}$",
  specX=TRUE, ggxbreaks=seq(-1,7,1), plotType='ribbon'
  )
temp=temp + theme(axis.title.y=element_text(vjust=1))
setwd(pathGraphics)
if(genTikz){ tikz(file='mCapSimPlot.tex', width=6, height=4, standAlone=F)}
temp
if(genTikz){ dev.off() }
###################################################################

#### Begin Robustness checks
###################################################################
# Crossval: Testing for heterogeneous effects across subsets
vars=unique(na.omit(c( 'ccode','year','gdpGr_l0',
  rownames(summary(mCity)$'coefficients')[2:100],
  rownames(summary(mCap)$'coefficients')[2:100] ) ) )
crossData=na.omit( modData[,vars] )

cntries=unique(modData$ccode)
set.seed(6886)
nF=6
folds=data.frame(
  cbind(ccode=cntries, fold=sample(1:nF, length(cntries), replace=TRUE) ) )
table(folds$fold)
crossData$fold=folds$fold[match(crossData$ccode, folds$ccode)]
table(crossData$fold)

# Running models
crossResults=list(NULL, NULL)
for(f in 1:nF ){
  # Subset data by fold
  cData=crossData[which(crossData$fold!=f), ]
  
  # Run models 
  ctyMod=lmer(ctyForm, cData)
  capMod=lmer(capForm, cData)
  
  # Save coefficient estimates
  ctyCR=cbind(summary(ctyMod)$'coefficients',fold=f)
  capCR=cbind(summary(capMod)$'coefficients',fold=f)
  crossResults[[1]]=rbind(crossResults[[1]],  ctyCR)
  crossResults[[2]]=rbind(crossResults[[2]],  capCR)
}

# Plotting cross-validation to test model heterogeneity
ccityCross=rbind(
  crossResults[[1]][which(rownames(crossResults[[1]])%in%'lnminDist.min'),],
  crossResults[[2]][which(rownames(crossResults[[2]])%in%'lncapDist.min'),])
ccCoefs=c('lncapDist.min', 'lnminDist.min')
ccNames=c('Ln(Min. Capital Dist.)$_{t-1}$','Ln(Min. City Dist.)$_{t-1}$')

temp <- ggcoefplot(coefData=ccityCross, vars=ccCoefs, varNames=ccNames, 
  Noylabel=FALSE, coordFlip=FALSE, revVar=TRUE, xAngle=45,
  facet=TRUE, facetName='fold', facetBreaks=1:nF, 
  facetLabs=paste0('Fold ',LETTERS[1:nF])
  )
temp=temp+facet_wrap(~Variable, scales='fixed')
setwd(pathGraphics)
if(genTikz){ tikz(efile='crossValPlot.tex', width=7, height=4, standAlone=FALSE)}
temp
if(genTikz){ dev.off() }
###################################################################