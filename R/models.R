if(Sys.info()["user"]=="janus829"){source('/Users/janus829/Desktop/Research/Rhodium/R/setup.R')}
if(Sys.info()["user"]=="Ben"){source('/Users/Ben/Github/Rhodium/R/setup.R')}

# Load conflict country year data
setwd(pathData)
load('combinedData.rda')

yData$upperincome=0
yData$upperincome[which(yData$income_l0 %in% c('High income: OECD', 'High income: nonOECD'  ) )] = 1


# CREATE APPROPRIATE VARIABLES FOR REGRESSIONS
####################################################################################################################
yData$lnminDist.min <- log(yData$minDist.min)
yData$lnminDist.mean <- log(yData$minDist.mean)
yData$lncapDist.min <- log(yData$capDist.min)
yData$lnArea <- log(yData$Conflict.area.mean)
yData$Int.max <- yData$Int.max-1
yData$intPerKm <- yData$Int.max/yData$lnArea
yData$USA <- yData$ccode==2
yData$coldwar <- yData$year<1991

yData <- yData[order(yData$year),]
####################################################################################################################


## MODELS FOR GDP GROWTH PER CAPITA (ANNUAL %)
####################################################################################################################
model1 <- lmer(NY.GDP.PCAP.KD.ZG_l0 ~ upperincome + Int.max + lnArea + lnminDist.min + territorial.max + (1|ccode), data=yData)
summary(model1)
plot(resid(model1))

# model2 <- lmer(NY.GDP.PCAP.KD.ZG_l1 ~ upperincome + Int.max + lnArea + lnminDist.min + territorial.max + Int.max*lnminDist.min + (1|ccode), data=yData)
# summary(model2)
# plot(resid(model2))
# This model is weird
####################################################################################################################


## MODELS FOR GDP GROWTH (ANNUAL %)
####################################################################################################################
plot(density(yData$NY.GDP.MKTP.KD.ZG_l0,na.rm=T))

# yData$GDP_transform_l0 <- sqrt(abs(yData$NY.GDP.MKTP.KD.ZG_l0))
# yData$GDP_transform_l0 <- yData$GDP_transform_l0 * ifelse(yData$NY.GDP.MKTP.KD.ZG_l0<0,-1,1)
# plot(density(yData$GDP_transform_l0,na.rm=T))

model3 <- lmer(NY.GDP.MKTP.KD.ZG_l0 ~ upperincome + Int.max + lnArea + lnminDist.min + territorial.max + (1|ccode), data=yData)
summary(model3)
plot(resid(model3),col=model3@frame$ccode)
text(1:length(resid(model3)),resid(model3),model3@frame$ccode,cex=.5)
# model4 <- lmer(NY.GDP.MKTP.KD.ZG_l1 ~ upperincome + Int.max + lnArea + lnminDist.min + territorial.max + Int.max*lnminDist.min + (1|ccode), data=yData)
# summary(model4)
# plot(resid(model4))
####################################################################################################################


