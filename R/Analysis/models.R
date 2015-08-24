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
# Regression Tables
library(stargazer)
# Tabular depictions for PRIO city models
stargazer(mCity, mCap)
# Tabular depictions for ACLED city models
stargazer(mAcled, mAcledCap)
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
  'Resource Rents/GDP$_{t}$', 'World GDP Growth$_{t}$')

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

vnames=c('Ln(Min. City Dist.)$_{t-1}$', "Ln(Inflation)$_{t-1}$","Democracy$_{t-1}$","Resource Rents/GDP$_{t}$","World GDP Growth$_{t}$")
temp <- ggcoefplot(coefData=summary(mAcled)$'coefficients'[c(2,4,5,6,7),],
                   vars=na.omit(rownames(summary(mAcled)$'coefficients')[c(2,4,5,6,7)]), 
                   varNames=vnames, Noylabel=FALSE, coordFlip=TRUE,
                   colorGrey=FALSE, grSTA=0.5, grEND=0.1)
setwd(pathGraphics)
if(genTikz){ tikz(file='mAcledCityCoefPlot.tex', width=4, height=6, standAlone=F)}
temp
if(genTikz){ dev.off() }

vnames=c('Ln(Min. Capital Dist.)$_{t-1}$', "Ln(Inflation)$_{t-1}$","Democracy$_{t-1}$","Resource Rents/GDP$_{t}$","World GDP Growth$_{t}$")
temp <- ggcoefplot(coefData=summary(mAcledCap)$'coefficients'[c(2,4,5,6,7),],
                   vars=na.omit(rownames(summary(mAcledCap)$'coefficients')[c(2,4,5,6,7)]), 
                   varNames=vnames, Noylabel=FALSE, coordFlip=TRUE,
                   colorGrey=FALSE, grSTA=0.5, grEND=0.1)
setwd(pathGraphics)
if(genTikz){ tikz(file='mAcledCapCoefPlot.tex', width=4, height=6, standAlone=F)}
temp
if(genTikz){ dev.off() }
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
# Divide intro training and test
train=modData[which(modData$year<=2000),]
test=modData[which(modData$year>2000),]

# Run model on train
mCityTr=lmer(ctyForm, data=train)
mCapTr=lmer(capForm, data=train)

# Performance stats on test
outSampPerf = function(dv, mod, testData){
  res=summary(mod)$'coefficients'
  tData=na.omit(
    cbind(1, testData[,c(dv, rownames(res)[2:nrow(res)] ) ] ) 
    )
  y=tData[,2]; tData=data.matrix(tData[,c(1,3:ncol(tData))])
  tY = tData %*% cbind(res[,1])
  diff=y-tY
  return( sqrt(mean(diff^2)) )
}

ggRMSE=data.frame(rbind(
  cbind(var="Ln(Min. City Dist.)$_{t}$", type='In-Sample \n n=375 \n N=66', 
    stat=rmse(mCityTr) ),
  cbind("Ln(Min. City Dist.)$_{t}$", 'Out-Sample \n n=186 \n N=42', 
    outSampPerf('lngdpGr_l0',mCityTr,test)),
  cbind("Ln(Min. Capital Dist.)$_{t}$", 'In-Sample \n n=375 \n N=66', 
    rmse(mCapTr) ),
  cbind("Ln(Min. Capital Dist.)$_{t}$", 'Out-Sample \n n=186 \n N=42', 
    outSampPerf('lngdpGr_l0',mCapTr,test))
  ))
ggRMSE$stat=numSM(ggRMSE$stat)

# Bar plot
temp=ggplot(ggRMSE, aes(x=type, y=stat))
temp=temp + geom_bar(stat='identity') + ylab('RMSE') + xlab('')
temp=temp + facet_wrap(~var)
temp=temp + theme(axis.title.y=element_text(vjust=1),
  legend.position='none', legend.title=element_blank(),
  axis.ticks=element_blank(), panel.grid.major=element_blank(),
  panel.grid.minor=element_blank())
setwd(pathGraphics)
if(genTikz){ tikz(file='rmseInOut.tex', width=7, height=4, standAlone=FALSE)}
temp
if(genTikz){ dev.off() }
###################################################################

###################################################################
# Crossval: Testing for heterogeneous effects across subsets
vars=c( 'ccode','year',dv,kivs,cntrls)
crossData=na.omit( modData[,vars] )

cntries=unique(crossData$ccode)
set.seed(6886)
nF=length(cntries)
folds=data.frame( ccode=cntries, fold=1:nF )
table(folds$fold)
crossData$fold=folds$fold[match(crossData$ccode, folds$ccode)]
table(crossData$fold)

# Running models
crossResults=list(NULL, NULL)
perfMat=matrix(NA, nrow=nF,ncol=3,
  dimnames=list(NULL,c('Fold','inRMSE','outRMSE')))
crossPerf=list(perfMat, perfMat)
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

  # Save performance stats
  test=crossData[which(crossData$fold==f), ]
  crossPerf[[1]][f,]=c(f,rmse(ctyMod), outSampPerf('gdpGr_l0',ctyMod,test) ) 
  crossPerf[[2]][f,]=c(f,rmse(capMod), outSampPerf('gdpGr_l0',capMod,test) ) 
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
  facetLabs=folds$ccode
  )
temp=temp+facet_wrap(~Variable, scales='fixed')
setwd(pathGraphics)
temp
if(genTikz){ tikz(efile='crossValPlot.tex', width=7, height=4, standAlone=FALSE)}
temp
if(genTikz){ dev.off() }

# Cross val performance stats
crossPerfData=rbind(data.frame(crossPerf[[1]]), data.frame(crossPerf[[2]]))
crossPerfData$Variable=c( rep('Ln(Min. City Dist.)$_{t-1}$',nF),
  rep('Ln(Min. Capital Dist.)$_{t-1}$',nF))
ggRMSE=melt(crossPerfData, id=c('Fold','Variable'))

temp=ggplot(ggRMSE, aes(x=Fold, y=value, fill=variable))
temp=temp + geom_bar(stat='identity',position=position_dodge())
# temp=temp + ylab('RMSE') + xlab('') + ylim(0, 0.006) 
temp=temp + scale_x_continuous(breaks=1:nF,labels=folds$ccode)
temp=temp + scale_fill_manual(values=c('inRMSE'='black', 'outRMSE'='grey'))
temp=temp + facet_wrap(~Variable)
temp=temp + theme(legend.position='top', legend.title=element_blank(),
      axis.ticks=element_blank(), panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(), axis.text.x=element_text(angle=45,hjust=1))
temp
###################################################################