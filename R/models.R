if(Sys.info()["user"]=="janus829"){source('/Users/janus829/Desktop/Research/Rhodium/R/setup.R')}
if(Sys.info()["user"]=="Ben"){source('/Users/Ben/Github/Rhodium/R/setup.R')}

# Load conflict country year data
setwd(pathData)
load('combinedData.rda')

yData$upperincome=0
yData$upperincome[which(yData$income_l0 %in% c('High income: OECD', 'High income: nonOECD'  ) )] = 1

##################################################################
# SAVE THIS JUST IN CASE EVERYTHING ELSE FALLS APART
yData$lnminDist.min <- log(yData$minDist.min)
yData$lnminDist.mean <- log(yData$minDist.mean)
yData$lncapDist.min <- log(yData$capDist.min)
yData$lnArea <- log(yData$Conflict.area.mean)
yData$Int.max <- yData$Int.max-1
yData$intPerKm <- yData$Int.max/yData$lnArea

yData <- yData[order(yData$year),]

model1 <- lmer(NY.GDP.PCAP.KD.ZG_l0 ~ upperincome + Int.max + lnArea + lnminDist.min + territorial.max + (1|ccode), data=yData)
summary(model1)
plot(resid(model1))

model2 <- lmer(NY.GDP.PCAP.KD.ZG_l0 ~ upperincome + Int.max + lnArea + lnminDist.min + territorial.max + Int.max*lnminDist.min + (1|ccode), data=yData)
summary(model2)
plot(resid(model2))
# This model shows us that: High intensity conflicts result in higher growth if they are farther from the nearest city.

##################################################################

