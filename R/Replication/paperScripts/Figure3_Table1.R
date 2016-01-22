source('paperScripts/setup.R')

# Load conflict country year data
load(paste0(pathData,'fullData.rda'))
load(paste0(pathData,'combinedData.rda'))
modData = fullData

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
kivs = c(
	'civwar', 
	'nconf'
	)
cntrls = c('upperincome', 'lninflation_l1',  'polity2', 'resourceGDP',  'gdpGr.mean_l0'
	)

# Fixef model
civwarFormFE=modForm(ivs=c(kivs[1], cntrls), type='fixed')
mCivWar = lm(civwarFormFE, data = modData ); summary(mCivWar)$'coefficients'[1:(length(cntrls)+1),]
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
set.seed(6886)
draws = mvrnorm(10000, coef, varCov)

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

## FIGURE 3 ##
tmp = ggplot()
tmp = tmp + geom_line(data=ggDensity, aes(x=x,y=y,color=X2))
tmp = tmp + geom_vline(data=ggMeans,
  aes(xintercept=sMean, color=X2),linetype='solid',size=1)
tmp = tmp + geom_ribbon(data=subset(ggDensity,q95),
  aes(x=x,ymax=y,fill=X2),ymin=0,alpha=0.5)
tmp = tmp + geom_ribbon(data=subset(ggDensity,q90),
  aes(x=x,ymax=y,fill=X2),ymin=0,alpha=0.9)
tmp = tmp + theme(legend.position='none')
tmp = tmp + xlab("\\% $\\Delta$ GDP$_{t}$") + ylab("Density")
tmp = tmp + scale_x_continuous(breaks=seq(-6,6,2), limits=c(-5,7))
tmp = tmp + theme(panel.border = element_blank(), 
	axis.line = element_line(), axis.ticks = element_blank(),
	panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
	axis.title.x = element_text(vjust=-0.2),
	axis.title.y = element_text(vjust=0.2))
ggsave(file=paste0(pathGraphics,'Figure3.pdf'), plot=tmp, width=6, height=4)
###################################################################

###################################################################
## TABLE 1 ##
# Model results
civwarFormBase=modForm(ivs=c(kivs[1], cntrls), type='none')
regData = modData[,c(dv, kivs, cntrls, 'ccode', 'year')]
plmFE = plm(civwarFormBase, data=regData, model='within', effect='individual', index=c('ccode','year'))
starOut(paste0(pathGraphics,"Table1.tex"), stargazer(plmFE))

# Number of countries
slice = na.omit( regData[,c('ccode','gdpGr_l0',kivs[1], cntrls)] )
length(unique(slice$ccode))
###################################################################