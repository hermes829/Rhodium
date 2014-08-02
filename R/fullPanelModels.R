if(Sys.info()["user"]=="janus829"){source('/Users/janus829/Desktop/Research/Rhodium/R/setup.R')}
if(Sys.info()["user"]=="Ben"){source('/Users/Ben/Github/Rhodium/R/setup.R')}

# Load conflict country year data
setwd(pathData)
load('combinedData.rda')

###################################################################
# CREATE APPROPRIATE VARIABLES FOR REGRESSIONS

# Correcting imp stuff
impData$ccode=round(impData$ccode,0)
impData$year=round(impData$year,0)

# Logging conflict distance variables
impData$lnIminDist.min = log(impData$IminDist.min+1)
impData$lnIcapDist.min = log(impData$IcapDist.min+1)
impData$lnIconfArea.mean = log(1/(impData$Conflict.area.mean+1))
impData$lnIconfArea.max = log(1/(impData$Conflict.area.max+1))

# Binaries
impData$USA = impData$ccode==2
impData$coldwar = impData$year<1991

# Log of other variables
impData$lnLandArea = log(impData$AG.LND.TOTL.K2)
impData$lnGDPgr = log(
	impData$NY.GDP.MKTP.KD.ZG +
	abs(min(impData$NY.GDP.MKTP.KD.ZG)) + 1 )
impData$lnGDPcap = log(impData$NY.GDP.PCAP.KD)
impData$lnInflation = log(
	impData$NY.GDP.DEFL.KD.ZG + 
	abs(min(impData$NY.GDP.DEFL.KD.ZG)) + 1 )

# Ordering
impData = impData[order(impData$year),]

# Creating lags
lagVars=names(impData)[4:ncol(impData)]
impData=lagDataSM(impData,'ccodeYear','ccode',lagVars,lag=1)
###################################################################

###################################################################
# Full model
cityMod = lmer(
  lnGDPgr ~ 
	lag1_lnGDPcap + lnLandArea
	+ lag1_lnInflation
	+ lag1_Int.mean + lag1_lnIconfArea.mean + durSt2max + nconf
	+ lag1_lnIminDist.min
	+ (1|ccode), data=impData)
summary(cityMod)
rmse(cityMod)

capMod = lmer(
  lnGDPgr ~ 
	lag1_lnGDPcap + lnLandArea
	+ lag1_lnInflation
	+ lag1_Int.mean + lag1_lnIconfArea.mean + durSt2max + nconf
	+ lag1_lnIcapDist.min
	+ (1|ccode), data=impData)
summary(capMod)
rmse(capMod)
###################################################################

###################################################################
# Divide into train and test
train=impData[which(impData$year %in% 1989:2005),]
test=na.omit(impData[which(impData$year %in% 2006:2008),])

# Run models on training data
cityModTrain = lmer(
  lnGDPgr ~ 
	lag1_lnGDPcap + lnLandArea
	+ lag1_lnInflation
	+ lag1_Int.mean + lag1_lnIconfArea.mean + durSt2max + nconf
	+ lag1_lnIminDist.min
	+ (1|ccode), data=train)
summary(cityModTrain)
rmse(cityModTrain)

capModTrain = lmer(
  lnGDPgr ~ 
	lag1_lnGDPcap + lnLandArea
	+ lag1_lnInflation
	+ lag1_Int.mean + lag1_lnIconfArea.mean + durSt2max + nconf
	+ lag1_lnIcapDist.min
	+ (1|ccode), data=train)
summary(capModTrain)
rmse(capModTrain)

# Calculate predicted values for test data

# results=summary(cityModTrain)$'coefficients'
results=summary(capModTrain)$'coefficients'

tData=cbind('(Intercept)'=1,test[,rownames(results)[2:nrow(results)]])
tPreds = data.matrix(tData) %*% cbind(results[,1])
tDiff = sqrt( mean( (test[,'lnGDPgr'] - tPreds)^2 ) )
tDiff
###################################################################

###################################################################
# Coefficient Plots
setwd(pathMain)
source('vizResults.R')

results=summary(cityMod)$'coefficients'
coefs=rownames(results)[2:nrow(results)]
vnames=c('Ln(GDP per capita)$_{t-1}$', 'Ln(Land Area)$_{t}$',
	'Ln(Inflation)$_{t-1}$', 'Conflict Intensity$_{t-1}$',
	'Ln(1/Conflict Area)$_{t-1}$','Conflict Duration$_{t}$',
	'Number of Conflicts$_{t}$', 
	'Ln(1/Min. City Conflict)$_{t-1}$')
	# 'Ln(1/Min. Cap. Conflict)$_{t-1}$')

temp = ggcoefplot(coefData=results, 
    vars=coefs, varNames=vnames, Noylabel=FALSE, coordFlip=TRUE,
    colorGrey=FALSE, grSTA=0.5, grEND=0.1)
temp
# setwd(pathTex)
# tikz(file='mod3CoefPlot.tex', width=4, height=4, standAlone=F)
# temp
# dev.off()
###################################################################

###################################################################
# Simulations
coefs=rownames(results)[2:nrow(results)]
data=na.omit(impData[,coefs])
results=capMod

estimates = results@beta
names(estimates)=rownames(summary(results)$'coefficients')
varcov = vcov(results)
RSS = sum(residuals(results)^2)
dfResid = nrow(data) - length(results@beta) - 
	nrow(coefficients(results)$'ccode') + 1
# error = sqrt(RSS/dfResid) 
error=0 # Set to zero to get rid of fundamental uncertainty
toTest = coefs[length(coefs)]
quants=quantile(data[,toTest], seq(0,1,.1))
tRange=seq(quants[1],quants[length(quants)], 0.1)

temp = ggsimplot(sims=10000, simData=data, vars=coefs, 
  vi=toTest, vRange=tRange, ostat=median,
  betas=estimates, vcov=varcov, sigma=error, intercept=TRUE,
  ylabel="\\% $\\Delta$ GDP$_{t}$", 
  xlabel="Ln(1/Min. City Conflict.)$_{t-1}$",
  # xlabel="Ln(1/Min. Cap. Conflict.)$_{t-1}$",  
  plotType='ribbon'
  )
temp
# setwd(pathTex)
# tikz(file='mod3SimPlot.tex', width=6, height=4, standAlone=F)
# temp
# dev.off()
###################################################################