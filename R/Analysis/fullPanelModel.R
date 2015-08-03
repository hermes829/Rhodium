if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
  source('~/Research/Rhodium/R/setup.R')}
if(Sys.info()["user"]=="Ben"){source('/Users/Ben/Github/Rhodium/R/setup.R')}

# Load conflict country year data
setwd(pathData)
load('fullData.rda')
load('combinedData.rda')
modData = fullData

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

# Transformations for other controls
modData$lngdpCap = log(modData$gdpCap)
modData$lninflation_l1 = logTrans(modData$inflation_l1)
modData$democ = as.numeric(modData$polity2>=6)
modData$polity2 = modData$polity2 + 11

# Add in distance variables
distVars = c('minDist.min', 'capDist.min', 'minDist.mean', 'capDist.mean')
for(var in distVars){
	# Merge in distance data from yData
	modData$tmp = yData[,var][match(modData$cyear, yData$cyear)]
	names(modData)[ncol(modData)] = var

	# Invert variable
	modData[,var] = 1/modData[,var]

	# Convert NAs to zero
	modData[,var][is.na( modData[,var] )] = 0

	# Log transformed version
	lnVar = paste0('ln',var)
	modData$tmp = log(modData[,var] + 1)
	names(modData)[ncol(modData)] = lnVar

	# Create interaction variable
	modData$tmp = modData$civwar * modData[,lnVar]
	names(modData)[ncol(modData)] = paste0('civwar_', lnVar)
}
###################################################################

## MODELS FOR GDP GROWTH (ANNUAL %)
###################################################################
# Model parameters
modForm = function(dv='lngdpGr_l0', ivs, id='ccode', type='random'){
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

dv = 'lngdpGr_l0'
kivs = c(
	'lncapDist.min', 
	'lnminDist.min'
	)
cntrls = c('upperincome', 'lninflation_l1',  'polity2', 'resourceGDP',  'gdpGr.mean_l0'
	# , 'civwar'
	)

# Run random effect models
civwarForm=modForm(ivs=c(kivs[1], cntrls), type='random')
nconfForm=modForm(ivs=c(kivs[2], cntrls), type='random')
mCivWar = lmer(civwarForm, data = modData ); summary(mCivWar)$'coefficients'
mNConf = lmer(nconfForm, data = modData ); summary(mNConf)$'coefficients'

# Fixef Robustness Checks
civwarFormFE=modForm(ivs=c(kivs[1], cntrls), type='fixed')
nconfFormFE=modForm(ivs=c(kivs[2], cntrls), type='fixed')
mCivWar = lm(civwarFormFE, data = modData ); summary(mCivWar)$'coefficients'[1:(length(cntrls)+1),]
mNConfFixefCntry = lm(nconfFormFE, data = modData ); summary(mNConfFixefCntry)$'coefficients'[1:(length(cntrls)+1),]

# Run Hausman test on city and cap models
library(plm)
civwarFormBase=modForm(ivs=c(kivs[1], cntrls), type='none')
nconfFormBase=modForm(ivs=c(kivs[2], cntrls), type='none')

regData = modData[,c(dv, kivs, cntrls, 'ccode', 'year')]
plmFE = plm(civwarFormBase, data=regData, model='within', effect='individual', index=c('ccode','year'))
plmRE = plm(civwarFormBase, data=regData, model='random', effect='individual', index=c('ccode','year'))
phtest(plmFE, plmRE)

plmFE = plm(nconfFormBase, data=regData, model='within', effect='individual', index=c('ccode','year'))
plmRE = plm(nconfFormBase, data=regData, model='random', effect='individual', index=c('ccode','year'))
phtest(plmFE, plmRE)

# Basic model diags
rmse=function(x){sqrt( mean( (residuals(x)^2) ) )}

Reduce('-',quantile(modData$lngdpGr_l0,c(0.75,0.25),na.rm=T))
rmse(mCivWar); rmse(mNConf)
###################################################################