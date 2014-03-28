if(Sys.info()["user"]=="janus829"){source('/Users/janus829/Desktop/Research/Rhodium/R/setup.R')}
if(Sys.info()["user"]=="Ben"){source('/Users/Ben/Github/Rhodium/R/setup.R')}

# Load conflict country year data
setwd(pathData)
load('combinedData.rda')
library(plm)

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
<<<<<<< HEAD
# yData$BX.KLT.DINV.CD.WD <- 
yData$lnAG.LND.TOTL.K2_l0 = yData$AG.LND.TOTL.K2_l0
=======
>>>>>>> dc3b0130ebbc5c87314ccb6558121a0ea74f4fd8

yData <- yData[order(yData$year),]
####################################################################################################################


## MODELS FOR GDP GROWTH PER CAPITA (ANNUAL %)
####################################################################################################################
<<<<<<< HEAD
model1 <- lmer(NY.GDP.PCAP.KD.ZG_l0 ~ upperincome + Int.max + lnArea + lnminDist.min + territorial.max + durSt1max + NY.GDP.DEFL.KD.ZG_l1 + lnAG.LND.TOTL.K2_l0 + (1|ccode) + (1|year), data=yData)
=======
plot(density(yData$NY.GDP.PCAP.KD.ZG_l0,na.rm=T), frame=F, las=1, ylab="", xlab="GDP Growth per Capita (Annual %)", main="")

model1 <- lmer(NY.GDP.PCAP.KD.ZG_l0 ~ upperincome + Int.max + lnArea + lnminDist.min + territorial.max + durSt1max + NY.GDP.DEFL.KD.ZG_l1 + log(AG.LND.TOTL.K2_l0) + (1|ccode) + (1|year), data=yData)
>>>>>>> dc3b0130ebbc5c87314ccb6558121a0ea74f4fd8
summary(model1)
model1FE <- lm(NY.GDP.PCAP.KD.ZG_l0 ~ upperincome + Int.max + lnArea + lnminDist.min + territorial.max + durSt1max + NY.GDP.DEFL.KD.ZG_l1 + lnAG.LND.TOTL.K2_l0 + as.factor(ccode), data=yData)
summary(model1FE)

resid <- resid(model1)
resid <- resid+(model1@frame$lnminDist.min*1.3566)
par(mar=c(5.1,4.1,2.1,2.1))
plot(model1@frame$lnminDist.min,resid, las=1, ylab="Model Residuals without Distance Measure", xlab="Distance from Major City")
abline(h=0,lty=2,lwd=3)
abline(lm(resid~model1@frame$lnminDist.min),lty=1,col="red",lwd=3)
# plot(resid(model1))

# model2 <- lmer(NY.GDP.PCAP.KD.ZG_l1 ~ upperincome + Int.max + lnArea + lnminDist.min + territorial.max + Int.max*lnminDist.min + (1|ccode), data=yData)
# summary(model2)
# plot(resid(model2))
# This model is weird
####################################################################################################################


## MODELS FOR GDP GROWTH (ANNUAL %)
####################################################################################################################
plot(density(yData$NY.GDP.MKTP.KD.ZG_l0,na.rm=T), frame=F, las=1, ylab="", xlab="GDP Growth (Annual %)", main="")
names(yData)
# yData$GDP_transform_l0 <- sqrt(abs(yData$NY.GDP.MKTP.KD.ZG_l0))
# yData$GDP_transform_l0 <- yData$GDP_transform_l0 * ifelse(yData$NY.GDP.MKTP.KD.ZG_l0<0,-1,1)
# plot(density(yData$GDP_transform_l0,na.rm=T))

model3 <- lmer(NY.GDP.MKTP.KD.ZG_l0 ~ upperincome + Int.max + lnArea + lnminDist.min + territorial.max + durSt1max + NY.GDP.DEFL.KD.ZG_l1 + lnAG.LND.TOTL.K2_l0 + (1|ccode) + (1|year), data=yData)
summary(model3)

model3FE <- lm(NY.GDP.MKTP.KD.ZG_l0 ~ upperincome + Int.max + lnArea + lnminDist.min + territorial.max + durSt1max + NY.GDP.DEFL.KD.ZG_l1 + lnAG.LND.TOTL.K2_l0 + as.factor(ccode), data=yData)
summary(model3FE)


resid <- resid(model3)
resid <- resid+(model3@frame$lnminDist.min*1.3566)
par(mar=c(5.1,4.1,2.1,2.1))
plot(model3@frame$lnminDist.min,resid, las=1, ylab="Model Residuals without Distance Measure", xlab="Distance from Major City")
abline(h=0,lty=2,lwd=3)
abline(lm(resid~model3@frame$lnminDist.min),lty=1,col="red",lwd=3)

# model4 <- lmer(NY.GDP.MKTP.KD.ZG_l1 ~ upperincome + Int.max + lnArea + lnminDist.min + territorial.max + Int.max*lnminDist.min + (1|ccode), data=yData)
# summary(model4)
# plot(resid(model4))
####################################################################################################################


####################################################################################################################
# Coefficient Plots
setwd(pathMain)
source('vizResults.R')

coefs=c('upperincome', 'Int.max', 'lnArea', 'lnminDist.min', 
	'territorial.max', 'durSt1max', 'NY.GDP.DEFL.KD.ZG_l1', 'lnAG.LND.TOTL.K2_l0')
vnames=coefs

temp <- ggcoefplot(coefData=summary(model3)@coefs, 
    vars=coefs, varNames=vnames, Noylabel=FALSE, coordFlip=TRUE,
    specY=TRUE, ggylims=c(-7,5), ggybreaks=seq(-7,5,1),    
    colorGrey=FALSE, grSTA=0.5, grEND=0.1)
temp
####################################################################################################################

####################################################################################################################
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
vRange=seq(-1,7,.25)

temp = ggsimplot(sims=10000, simData=data, vars=coefs, 
  vi=toTest, vRange=-1:7, ostat=median,
  betas=estimates, vcov=varcov, sigma=error, intercept=TRUE,
  ylabel="GDP Growth$_{t}$", xlabel="Ln(Min Dist)$_{t}$",
  specX=TRUE, ggxbreaks=seq(-1,7,1), plotType='ribbon'
  # specY=TRUE, ggybreaks=seq(0,12,2), ggylims=c(2,12)
  )
temp
####################################################################################################################
