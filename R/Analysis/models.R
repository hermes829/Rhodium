if(Sys.info()["user"]=="janus829"){source('~/Desktop/Research/Rhodium/R/setup.R')}
if(Sys.info()["user"]=="s7m"){source('~/Research/Rhodium/R/setup.R')}
if(Sys.info()["user"]=="Ben"){source('/Users/Ben/Github/Rhodium/R/setup.R')}

# Load conflict country year data
setwd(pathData)
load('combinedData.rda')

# Dataset to run analysis on (impData or yData)
modData=yData
# modData=impData

# CREATE APPROPRIATE VARIABLES FOR REGRESSIONS
###################################################################
logTrans=function(x){ log( x + abs(min(x, na.rm=T)) + 1) }

# Log transforming DVs
modData$lngdpGr_l0 = logTrans(modData$gdpGr_l0)

# Transformations for conflict variables
modData$lnminDist.min <- log(modData$minDist.min+1)
modData$lncapDist.min <- log(modData$capDist.min+1)
modData$Int.max <- modData$Int.max-1

# Transformations for other controls
modData$lninflation_l1 = logTrans(modData$inflation_l1)
modData$democ = as.numeric(modData$polity>=6)
###################################################################

## MODELS FOR GDP GROWTH (ANNUAL %)
###################################################################
modForm=function(x){
  formula( paste0('lngdpGr_l0 ~', x, '+ 
  Int.max + territorial.max + durSt1max + confAreaPropHi +
  upperincome + lninflation_l1 + gdpGr.mean_l0 +
  democ + (1|ccode)' ) )
}
ctyForm=modForm('lnminDist.min')
capForm=modForm('lncapDist.min')

mCity = lmer(ctyForm, data = modData ); summary(mCity)$'coefficients'
mCap = lmer(capForm, data = modData ); summary(mCap)$'coefficients'

# Basic model diags
summary(modData$lngdpGr_l0); rmse(mCity); rmse(mCap)
###################################################################

###################################################################
# Divide intro training and test
train=modData[which(modData$year<=2000),]
test=modData[which(modData$year>2000),]

# Run model on train
mCityTr=lmer(ctyForm, data=train)
mCapTr=lmer(capForm, data=train)

# Performance stats on test
outSampPerf = function(mod){
  res=summary(mod)$'coefficients'
  tData=na.omit(
    cbind(1, test[,c('lngdpGr_l0', rownames(res)[2:nrow(res)] ) ] ) 
    )
  y=tData[,2]; tData=data.matrix(tData[,c(1,3:ncol(tData))])
  tY = tData %*% cbind(res[,1])
  diff=y-tY
  return( sqrt(mean(diff^2)) )
}

rmse(mCityTr); rmse(mCapTr)
outSampPerf(mCityTr); outSampPerf(mCapTr)
###################################################################

###################################################################
# Crossval: Testing for heterogeneous effects across subsets
vars=unique(na.omit(c( 'ccode','year','lngdpGr_l0',
  rownames(summary(mCity)$'coefficients')[2:100],
  rownames(summary(mCap)$'coefficients')[2:100] ) ) )
crossData=na.omit( modData[,vars] )

cntries=unique(modData$ccode)
set.seed(543543)
nF=8
folds=data.frame(
  cbind(ccode=cntries, fold=sample(1:nF, length(cntries), replace=TRUE) ) )

crossData$fold=folds$fold[match(crossData$ccode, folds$ccode)]
table(crossData$fold)

# Runnign models
crossResults=list(NULL, NULL)
for(f in 1:nF ){
  cData=crossData[which(crossData$fold!=f), ]
  ctyCR=cbind(summary(lmer(ctyForm, cData))$'coefficients',fold=f)
  capCR=cbind(summary(lmer(capForm, cData))$'coefficients',fold=f)
  crossResults[[1]]=rbind(crossResults[[1]],  ctyCR)
  crossResults[[2]]=rbind(crossResults[[2]],  capCR)
}

crossResults[[1]][which(rownames(crossResults[[1]])%in%'lnminDist.min'),]
crossResults[[2]][which(rownames(crossResults[[2]])%in%'lncapDist.min'),]
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