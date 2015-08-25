if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
  source('~/Research/Rhodium/R/setup.R')}
if(Sys.info()["user"]=="Ben"){source('/Users/Ben/Github/Rhodium/R/setup.R')}

# Load conflict country year data
setwd(pathData)
load('fullData.rda')
load('combinedData.rda')
modData = fullData

# Gen tikz?
genTikz=TRUE

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

# Add in distance variables
distVars = c('minDist.min', 'capDist.min', 'minDist.mean', 'capDist.mean')
for(var in distVars){
	# Merge in distance data from yData
	modData$tmp = yData[,var][match(modData$cyear, yData$cyear)]
	names(modData)[ncol(modData)] = var

	# Log transformed version
	lnVar = paste0('ln',var)
	modData$tmp = log(modData[,var] + 1)
	names(modData)[ncol(modData)] = lnVar

	# Invert variable
	modData[,lnVar] = 1/modData[,lnVar]

	# Convert NAs to zero
	modData[,lnVar][is.na( modData[,lnVar] )] = 0

	# Create interaction variable
	modData$tmp = modData$civwar * modData[,lnVar]
	names(modData)[ncol(modData)] = paste0('civwar_', lnVar)
}
###################################################################

## MODELS FOR GDP GROWTH (ANNUAL %)
###################################################################
# Model parameters
dv = 'gdpGr_l0'
# kivs = c(
# 	'lncapDist.min', 
# 	'lnminDist.min'
# 	)
kivs = c(
	'civwar', 
	'nconf'
	)
cntrls = c('upperincome', 'lninflation_l1',  'polity2', 'resourceGDP',  'gdpGr.mean_l0'
	)

# Run Hausman test on city and cap models
library(plm)
civwarFormBase=modForm(ivs=c(kivs[1], cntrls), type='none')

regData = modData[,c(dv, kivs, cntrls, 'ccode', 'year')]
plmFE = plm(civwarFormBase, data=regData, model='within', effect='individual', index=c('ccode','year'))
plmRE = plm(civwarFormBase, data=regData, model='random', effect='individual', index=c('ccode','year'))
phtest(plmFE, plmRE)

# Fixef model
civwarFormFE=modForm(ivs=c(kivs[1], cntrls), type='fixed')
mCivWar = lm(civwarFormFE, data = modData ); summary(mCivWar)$'coefficients'[1:(length(cntrls)+1),]
###################################################################

###################################################################
# Model results: Plot
setwd(pathMain)
source('vizResults.R')

otherCovars=c(
  'Upper Income', 'Ln(Inflation)$_{t-1}$', 'Democracy$_{t-1}$',
  'Resource Rents/GDP$_{t}$', 'World GDP Growth$_{t}$')

vnames=c('Civil War$_{t-1}$', otherCovars)
temp <- ggcoefplot(coefData=summary(mCivWar)$'coefficients'[1:(length(cntrls)+1),],
    vars=na.omit(rownames(summary(mCivWar)$'coefficients'[1:(length(cntrls)+1),])), 
    varNames=vnames, Noylabel=FALSE, coordFlip=TRUE,
    colorGrey=FALSE, grSTA=0.5, grEND=0.1)
setwd(pathGraphics)
if(genTikz){ tikz(file='civWarCoefPlot.tex', width=4, height=6, standAlone=F)}
temp
if(genTikz){ dev.off() }
###################################################################

###################################################################
# Model results: Table
library(stargazer)
stargazer(plmFE)
###################################################################

###################################################################
# Substantive effects
# Model results
mod = mCivWar
vars = c('factor(ccode)950', kivs[1], cntrls)
coef = mod$'coefficients'[vars]
varCov = vcov(mod)[vars,vars]

# Set up scenario matrix
data = na.omit( modData[,vars[2:length(vars)]] )
scen = rbind(
	c(1, 1, 0, apply(data[,vars[4:length(vars)]], 2, function(x){ mean(x) })),
	c(1, 0, 0, apply(data[,vars[4:length(vars)]], 2, function(x){ mean(x) }))
	)
colnames(scen) = vars

# Construct simulation, only inferential uncertainty
draws = mvrnorm(1000, coef, varCov)

# Calculate predicted values
predVals = draws %*% t(scen)

# Plot
predMelt=melt(predVals)[,-1]
ggMeans = ddply(predMelt, .(X2), summarise, sMean=mean(value))
ggDensity = ddply(predMelt, .(X2), .fun=function(x){
  tmp = density(x$value); x1 = tmp$x; y1 = tmp$y
  q95 = x1 >= quantile(x$value,0.025) & x1 <= quantile(x$value,0.975)
  q90 = x1 >= quantile(x$value,0.05) & x1 <= quantile(x$value,0.95)
  data.frame(x=x1,y=y1,q95=q95, q90=q90) } )
ggMeans$X2 = factor(ggMeans$X2)
ggDensity$X2 = factor(ggDensity$X2)


temp = ggplot()
temp = temp + geom_line(data=ggDensity, aes(x=x,y=y,color=X2))
temp = temp + geom_vline(data=ggMeans,
  aes(xintercept=sMean, color=X2),linetype='solid',size=1)
temp = temp + geom_ribbon(data=subset(ggDensity,q95),
  aes(x=x,ymax=y,fill=X2),ymin=0,alpha=0.5)
temp = temp + geom_ribbon(data=subset(ggDensity,q90),
  aes(x=x,ymax=y,fill=X2),ymin=0,alpha=0.9)
temp = temp + theme(legend.position='none')
temp = temp + xlab("\\% $\\Delta$ GDP$_{t}$") + ylab("Density")
temp = temp + scale_x_continuous(breaks=seq(-6,6,2), limits=c(-6,7))
temp = temp + theme(panel.border = element_blank(), 
	axis.line = element_line(), axis.ticks = element_blank(),
	panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
	axis.title.x = element_text(vjust=-0.2),
	axis.title.y = element_text(vjust=0.2))
temp
setwd(pathGraphics)
if(genTikz){ tikz(file='civWarEffect.tex', width=6, height=4, standAlone=F)}
temp
if(genTikz){ dev.off() }
###################################################################
