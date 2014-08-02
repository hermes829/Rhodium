if(Sys.info()["user"]=="janus829"){source('/Users/janus829/Desktop/Research/Rhodium/R/setup.R')}
if(Sys.info()["user"]=="Ben"){source('/Users/Ben/Github/Rhodium/R/setup.R')}

# Load conflict country year data
setwd(pathData)
load('combinedData.rda')

###################################################################
# CREATE APPROPRIATE VARIABLES FOR REGRESSIONS

# Creating upper income variable
impData$upperincome=0
impData$upperincome[which(impData$income_l0 %in% c('High income: OECD', 'High income: nonOECD'  ) )] = 1

# Logging conflict distance variables
impData$lnIminDist.min = log(impData$IminDist.min+1)
impData$lnIminDist.mean = log(impData$IminDist.mean+1)
impData$lnIcapDist.min = log(impData$IcapDist.min+1)
impData$lnIcapDist.mean = log(impData$IcapDist.mean+1)
impData$lnIarea = log(1/(impData$Conflict.area.mean+1))

# Binaries
impData$USA = impData$ccode==2
impData$coldwar = impData$year<1991

# Log of other variables
impData$lnAG.LND.TOTL.K2 = log(impData$AG.LND.TOTL.K2)
impData$lnNY.GDP.MKTP.KD.ZG = log(
	impData$NY.GDP.MKTP.KD.ZG+
	abs(min(impData$NY.GDP.MKTP.KD.ZG)) +1 )

# Ordering
impData = impData[order(impData$year),]

# Creating lags
lagVars=names(impData)[4:ncol(impData)]
impData=lagDataSM(impData,'ccodeYear','ccode',lagVars,lag=1)
###################################################################

###################################################################
model3 <- lmer(
  lnNY.GDP.MKTP.KD.ZG ~ upperincome + lag1_Int.mean + 
  # lag1_lnIarea +
  lag1_lnIminDist.min + 
  # lag1_lnIcapDist.mean +   
	lag1_durSt2max + lnAG.LND.TOTL.K2
	# + lag1_NY.GDP.DEFL.KD.ZG 
	+ (1|ccode) + (1|year), data=impData)
summary(model3)
###################################################################