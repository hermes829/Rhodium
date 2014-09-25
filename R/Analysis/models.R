if(Sys.info()["user"]=="janus829"){source('~/Desktop/Research/Rhodium/R/setup.R')}
if(Sys.info()["user"]=="s7m"){source('~/Research/Rhodium/R/setup.R')}
if(Sys.info()["user"]=="Ben"){source('/Users/Ben/Github/Rhodium/R/setup.R')}

# Load conflict country year data
setwd(pathData)
load('combinedData.rda'); modData=yData

# CREATE APPROPRIATE VARIABLES FOR REGRESSIONS
###################################################################
logTrans=function(x){ log( x + abs(min(x, na.rm=T)) + 1) }

# Log transforming DVs
modData$lngdp_l0 = log(modData$gdp_l0)
modData$lngdp = log(modData$gdp)
modData$lngdpGr_l0 = (modData$lngdp_l0-modData$lngdp)/modData$lngdp_l0

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
  upperincome + lninflation_l1 + democ +
  gdpGr.mean_l0 + (1|ccode)' ) )
}
ctyForm=modForm('lnminDist.min')
capForm=modForm('lncapDist.min')

mCity = lmer(ctyForm, data = modData ); summary(mCity)$'coefficients'
mCap = lmer(capForm, data = modData ); summary(mCap)$'coefficients'

# Basic model diags
summary(modData$lngdpGr_l0); rmse(mCity); rmse(mCap)
###################################################################

###################################################################
# Coefficient Plots
setwd(pathMain)
source('vizResults.R')

coefs=na.omit(rownames(summary(mCity)$'coefficients')[2:100])
vnames=c('Ln(Min. City Dist.)$_{t-1}$', 
  'Intensity$_{t-1}$', 'Type$_{t-1}$', 'Duration$_{t-1}$', 'Area$_{t-1}$',
  'Upper Income', 'Ln(Inflation)$_{t-1}$', 'Democracy$_{t-1}$',
  'World GDP Growth$_{t}$')
temp <- ggcoefplot(coefData=summary(mCity)$'coefficients',
    vars=coefs, varNames=vnames, Noylabel=FALSE, coordFlip=TRUE,
    specY=TRUE, ggylims=c(-.004,.004), ggybreaks=seq(-.004,.004,.002),
    colorGrey=FALSE, grSTA=0.5, grEND=0.1)
temp
setwd(pathTex)
# tikz(file='mCityCoefPlot.tex', width=4, height=4, standAlone=F)
# temp
# dev.off()

coefs=na.omit(rownames(summary(mCap)$'coefficients')[2:100])
vnames=c('Ln(Min. Capital Dist.)$_{t-1}$', 
  'Intensity$_{t-1}$', 'Type$_{t-1}$', 'Duration$_{t-1}$', 'Area$_{t-1}$',
  'Upper Income', 'Ln(Inflation)$_{t-1}$', 'Democracy$_{t-1}$',
  'World GDP Growth$_{t}$')
temp <- ggcoefplot(coefData=summary(mCap)$'coefficients',
    vars=coefs, varNames=vnames, Noylabel=FALSE, coordFlip=TRUE,
    specY=TRUE, ggylims=c(-.004,.004), ggybreaks=seq(-.004,.004,.002),
    colorGrey=FALSE, grSTA=0.5, grEND=0.1)
temp
setwd(pathTex)
# tikz(file='mCapCoefPlot.tex', width=4, height=4, standAlone=F)
# temp
# dev.off()
###################################################################

###################################################################
# Simulations
temp = ggsimplot(modelResults=mCity, sims=10000, simData=modData, 
  vars=charSM(na.omit(rownames(summary(mCity)$'coefficients')[2:100])),
  vi='lnminDist.min', ostat=median, sigma=FALSE, intercept=TRUE,
  ylabel="\\% $\\Delta$ Ln(GDP)$_{t}$", xlabel="Ln(Min. City Dist.)$_{t}$",
  specX=TRUE, ggxbreaks=seq(-1,7,1), plotType='ribbon'
  )
temp
# setwd(pathTex)
# tikz(file='mCitySimPlot.tex', width=6, height=4, standAlone=F)
# temp
# dev.off()

temp = ggsimplot(modelResults=mCap, sims=10000, simData=modData, 
  vars=na.omit(rownames(summary(mCap)$'coefficients')[2:100]),
  vi='lncapDist.min', ostat=median, sigma=FALSE, intercept=TRUE,
  ylabel="\\% $\\Delta$ Ln(GDP)$_{t}$", xlabel="Ln(Min. Cap Dist.)$_{t}$",
  specX=TRUE, ggxbreaks=seq(-1,7,1), plotType='ribbon'
  )
temp
# setwd(pathTex)
# tikz(file='mCitySimPlot.tex', width=6, height=4, standAlone=F)
# tikz(file='mCapSimPlot.tex', width=6, height=4, standAlone=F)
# temp
# dev.off()
###################################################################


#### Begin Robustness checks

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

ggRMSE=data.frame(rbind(
  cbind(var="Ln(Min. City Dist.)$_{t}$", type='In-Sample \n n=375 \n N=66', stat=rmse(mCityTr) ),
  cbind("Ln(Min. City Dist.)$_{t}$", 'Out-Sample \n n=186 \n N=42', outSampPerf(mCityTr)),
  cbind("Ln(Min. Cap Dist.)$_{t}$", 'In-Sample \n n=375 \n N=66', rmse(mCapTr) ),
  cbind("Ln(Min. Cap Dist.)$_{t}$", 'Out-Sample \n n=186 \n N=42', outSampPerf(mCapTr))
  ))
ggRMSE$stat=numSM(ggRMSE$stat)

# Bar plot
temp=ggplot(ggRMSE, aes(x=type, y=stat))
temp=temp + geom_bar(stat='identity') + ylab('RMSE') + xlab('')
temp=temp + facet_wrap(~var)
temp=temp + theme(legend.position='none', legend.title=element_blank(),
      axis.ticks=element_blank(), panel.grid.major=element_blank(),
      panel.grid.minor=element_blank())
temp
setwd(pathTex)
# tikz(file='rmseInOut.tex', width=6, height=4, standAlone=F)
# temp
# dev.off()
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

# Plotting cross-validation to test model heterogeneity
ccityCross=rbind(
  crossResults[[1]][which(rownames(crossResults[[1]])%in%'lnminDist.min'),],
  crossResults[[2]][which(rownames(crossResults[[2]])%in%'lncapDist.min'),])
ccCoefs=c('lncapDist.min', 'lnminDist.min')
ccNames=c('Ln(Min. Capital Dist.)$_{t-1}$','Ln(Min. City Dist.)$_{t-1}$')

temp <- ggcoefplot(coefData=ccityCross, vars=ccCoefs, varNames=ccNames, 
  Noylabel=FALSE, coordFlip=FALSE, revVar=TRUE, xAngle=45,
  facet=TRUE, facetName='fold', facetBreaks=1:nF, 
  facetLabs=paste0('Fold ',LETTERS[1:nF]),
  colorGrey=TRUE, , grSTA=0.4, grEND=0.4
  )
temp  
setwd(pathTex)
# tikz(file='crossValPlot.tex', width=6, height=4, standAlone=F)
# temp
# dev.off()
###################################################################