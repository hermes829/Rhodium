if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){ source('~/Research/Rhodium/R/setup.R')}
if(Sys.info()["user"]=="Ben"){source('/Users/Ben/Github/Rhodium/R/setup.R')}
library(Hmisc)
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

###################################################################
# Number of observations per unit
prioDistData = na.omit( modData[,c('ccode','year','lnminDist.min')] )
acledDistData = na.omit( modData[,c('ccode','year','lnminDistACLED.min')] )
sort( table(prioDistData$ccode) )
sort( table(acledDistData$ccode) )

# Variation in distance measures by unit (RSD)
library(doBy)
varDist = summaryBy(lnminDist.min ~ ccode, data=modData, FUN=c(sd, mean), na.rm=TRUE)
varDist = na.omit( varDist )
varDist$sdMean = varDist[,2]/varDist[,3]
varDist[order(varDist[,4]),]

# Plot by unit
cnts = table( na.omit( modData[,c('ccode','year','minDist.min','lnminDist.min')] )$ccode )
slice = na.omit( modData[which(modData$ccode %in% names(cnts[cnts>1])), c('ccode','year','minDist.min','lnminDist.min')] )
ggplot(slice, aes(x=year, y=minDist.min)) + facet_wrap(~ccode, scales='free') + geom_bar(stat='identity')
###################################################################s

## MODELS FOR GDP GROWTH (ANNUAL %)
###################################################################
# Model parameters
dv = 'gdpGr_l0'
kivs = c('lnminDist.mean', 'lncapDist.mean')
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

# Fixed effect model specifications
ctyFormFE=modForm(ivs=c(kivs[1], cntrls), type='fixed')
capFormFE=modForm(ivs=c(kivs[2], cntrls), type='fixed')
acledFormFE = modForm(ivs=c('lnminDistACLED.mean', cntrls[4:length(cntrls)]), type='fixed')

# Run fixed effect models
mCityFixefCntry = lm(ctyFormFE, data = modData ); summary(mCityFixefCntry)$'coefficients'[1:(length(cntrls)+1),]
mCapFixefCntry = lm(capFormFE, data = modData ); summary(mCapFixefCntry)$'coefficients'[1:(length(cntrls)+1),]
mAcledFixefCntry = lm(acledFormFE, data = modData); summary(mAcledFixefCntry)$'coefficients'[1:6,]
###################################################################

###################################################################
# Compare RMSEs
rmse = function(x){ sqrt( mean( residuals(x)^2 ) ) }
unlist( lapply( list( mCity, mCityFixefCntry, mCap, mCapFixefCntry, mAcled, mAcledFixefCntry ), rmse ) )
quantile( modData$gdpGr_l0, probs=seq(0,1,.25), na.rm=TRUE )
sd( modData$gdpGr_l0, na.rm=TRUE )
###################################################################

###################################################################
# Run Hausman test on city and cap models
loadPkg('plm')
ctyFormBase=modForm(ivs=c(kivs[1], cntrls), type='none')
capFormBase=modForm(ivs=c(kivs[2], cntrls), type='none')

regData = modData[,c(dv, kivs, cntrls, 'ccode', 'year')]
plmFE = plm(ctyFormBase, data=regData, model='within', effect='individual', index=c('ccode','year'))
plmRE = plm(ctyFormBase, data=regData, model='random', effect='individual', index=c('ccode','year'))
phtest(plmFE, plmRE)

plmFE = plm(capFormBase, data=regData, model='within', effect='individual', index=c('ccode','year'))
plmRE = plm(capFormBase, data=regData, model='random', effect='individual', index=c('ccode','year'))
phtest(plmFE, plmRE)
###################################################################

###################################################################
# RE versus FE, Clark and Linzer (2015)
fixedEff <- mCityFixefCntry$effects[grepl("factor",names(mCityFixefCntry$effects))]
ctryAvs <- summaryBy(lncapDist.min ~ ccode, data=modData, FUN=mean)
fixedEff <- data.frame(ccode=names(fixedEff),val=fixedEff)
fixedEff$ccode <- gsub("factor\\(ccode\\)","",fixedEff$ccode)
ctryAvs <- merge(ctryAvs,fixedEff,by="ccode",all.x=T,all.y=F)
cor(ctryAvs[,2:3], use="complete.obs")
###################################################################