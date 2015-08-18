if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){ source('~/Research/Rhodium/R/setup.R')}
if(Sys.info()["user"]=="Ben"){source('/Users/Ben/Github/Rhodium/R/setup.R')}
library(Hmisc)
# Load conflict country year data
setwd(pathData)
# load('combinedData_Orig.rda'); modData=yData
load('combinedData.rda'); modData=yData
# load('combinedData_loInt.rda'); modData=yData
# load('combinedData_hiInt.rda'); modData=yData

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
kivs = c('lnminDist.mean', 'lncapDist.mean')
cntrls = c(
  'Int.max',
  'durSt1max',  'confAreaProp', 'nconf', 
  'upperincome', 'lninflation_l1',  'polity2', 'resourceGDP',  'gdpGr.mean_l0')

  # Run random effect models
ctyForm=modForm(ivs=c(kivs[1], cntrls), type='random')
capForm=modForm(ivs=c(kivs[2], cntrls), type='random')
# We can't use the modForm() function here because Int.max, durSt1max, confAreaProp are not defined for ACLED data:
acledForm = lngdpGr_l0 ~ lnminDistACLED.mean + nconf + upperincome + lninflation_l1 + polity2 + resourceGDP + gdpGr.mean_l0 + (1 | ccode)
acledFormCap = lngdpGr_l0 ~ lncapDistACLED.mean + nconf + upperincome + lninflation_l1 + polity2 + resourceGDP + gdpGr.mean_l0 + (1 | ccode)
mCity = lmer(ctyForm, data = modData ); summary(mCity)$'coefficients'
mCap = lmer(capForm, data = modData ); summary(mCap)$'coefficients'
mAcled = lmer(acledForm, data = modData ); summary(mAcled)$'coefficients'
mAcledCap = lmer(acledFormCap, data=modData); summary(mAcledCap)$'coefficients'

# Fixef Robustness Checks
ctyFormFE=modForm(ivs=c(kivs[1], cntrls), type='fixed')
capFormFE=modForm(ivs=c(kivs[2], cntrls), type='fixed')
# We can't use the modForm() function here because Int.max, durSt1max, confAreaProp are not defined for ACLED data:
acledFormFE = lngdpGr_l0 ~ lnminDistACLED.mean + nconf + upperincome + lninflation_l1 + polity2 + resourceGDP + gdpGr.mean_l0 + factor(ccode) -1
mCityFixefCntry = lm(ctyFormFE, data = modData ); summary(mCityFixefCntry)$'coefficients'[1:(length(cntrls)+1),]
mCapFixefCntry = lm(capFormFE, data = modData ); summary(mCapFixefCntry)$'coefficients'[1:(length(cntrls)+1),]
mAcledFixefCntry = lm(acledFormFE, data = modData); summary(mAcledFixefCntry)$'coefficients'[1:6,]

# RE versus FE
fixedEff <- mCityFixefCntry$effects[grepl("factor",names(mCityFixefCntry$effects))]
library(doBy)
ctryAvs <- summaryBy(lnminDist.min ~ ccode, data=modData, FUN=mean)
fixedEff <- data.frame(ccode=names(fixedEff),val=fixedEff)
fixedEff$ccode <- gsub("factor\\(ccode\\)","",fixedEff$ccode)
ctryAvs <- merge(ctryAvs,fixedEff,by="ccode",all.x=T,all.y=F)
cor(ctryAvs[,2:3], use="complete.obs")

# Run Hausman test on city and cap models
library(plm)
ctyFormBase=modForm(ivs=c(kivs[1], cntrls), type='none')
capFormBase=modForm(ivs=c(kivs[2], cntrls), type='none')

regData = modData[,c(dv, kivs, cntrls, 'ccode', 'year')]
plmFE = plm(ctyFormBase, data=regData, model='within', effect='individual', index=c('ccode','year'))
plmRE = plm(ctyFormBase, data=regData, model='random', effect='individual', index=c('ccode','year'))
phtest(plmFE, plmRE)

plmFE = plm(capFormBase, data=regData, model='within', effect='individual', index=c('ccode','year'))
plmRE = plm(capFormBase, data=regData, model='random', effect='individual', index=c('ccode','year'))
phtest(plmFE, plmRE)

# Basic model diags
rmse=function(x){sqrt( mean( (residuals(x)^2) ) )}

Reduce('-',quantile(modData$lngdpGr_l0,c(0.75,0.25),na.rm=T))
rmse(mCity); rmse(mCap)
###################################################################