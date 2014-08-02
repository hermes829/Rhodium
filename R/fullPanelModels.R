if(Sys.info()["user"]=="janus829"){source('/Users/janus829/Desktop/Research/Rhodium/R/setup.R')}
if(Sys.info()["user"]=="Ben"){source('/Users/Ben/Github/Rhodium/R/setup.R')}

# Load conflict country year data
setwd(pathData)
load('combinedData.rda')

###################################################################
# CREATE APPROPRIATE VARIABLES FOR REGRESSIONS

# Logging conflict distance variables
impData$lnIminDist.min = log(impData$IminDist.min+1)
impData$lnIcapDist.min = log(impData$IcapDist.min+1)
impData$lnIarea.mean = log(1/(impData$Conflict.area.mean+1))
impData$lnIarea.max = log(1/(impData$Conflict.area.max+1))

# Binaries
impData$USA = impData$ccode==2
impData$coldwar = impData$year<1991

# Log of other variables
impData$lnAG.LND.TOTL.K2 = log(impData$AG.LND.TOTL.K2)
impData$lnNY.GDP.MKTP.KD.ZG = log(
	impData$NY.GDP.MKTP.KD.ZG +
	abs(min(impData$NY.GDP.MKTP.KD.ZG)) + 1 )
impData$lnNY.GDP.PCAP.KD = log(impData$NY.GDP.PCAP.KD)
impData$lnNY.GDP.DEFL.KD.ZG = log(
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
  lnNY.GDP.MKTP.KD.ZG ~ 
	lag1_NY.GDP.PCAP.KD + lnAG.LND.TOTL.K2
	+ lag1_lnNY.GDP.DEFL.KD.ZG
	+ lag1_Int.mean + lag1_durSt2max + lag1_lnIarea.mean
	+ lag1_lnIminDist.min
	+ (1|ccode), data=impData)
summary(cityMod)
rmse(cityMod)

capMod = lmer(
  lnNY.GDP.MKTP.KD.ZG ~ 
	lnNY.GDP.PCAP.KD + lnAG.LND.TOTL.K2
	+ lag1_lnNY.GDP.DEFL.KD.ZG
	+ lag1_Int.mean + lag1_durSt2max + lag1_lnIarea.mean
	+ lag1_lnIcapDist.min
	+ (1|ccode), data=impData)
summary(capMod)
rmse(capMod)
###################################################################

###################################################################
# Divide into train and test

###################################################################

###################################################################
# Coefficient Plots
setwd(pathMain)
source('vizResults.R')

coefs=c('upperincome', 'Int.max', 'lnArea', 'lnminDist.min', 
	'territorial.max', 'durSt1max', 'NY.GDP.DEFL.KD.ZG_l1', 'lnAG.LND.TOTL.K2_l0')

vnames=c('Upper Income', 'Conflict Intensity$_{t}$', 'Ln(Conflict Area)$_{t}$', 'Ln(Min. Conflict Dist.)$_{t}$',
	'Conflict Type$_{t}$', 'Conflict Duration$_{t}$', 'Inflation$_{t-1}$', 'Ln(Land Area)')

temp = ggcoefplot(coefData=summary(model3)@coefs, 
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
data=na.omit(yData[,coefs])
results=model3
estimates = results@fixef
varcov = vcov(results)
rownames(varcov) = names(estimates); colnames(varcov) = names(estimates)
RSS = sum(results@resid^2)
dfResid = nrow(data)-length(results@fixef) - length(results@ranef) + 1
# error = sqrt(RSS/dfResid) 
error=0 # Set to zero to get rid of fundamental uncertainty
toTest = 'lnminDist.min'
tRange=seq(quantile(yData[,toTest])[1],quantile(yData[,toTest])[length(quantile(yData[,toTest]))], 0.1)

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