if(Sys.info()["user"]=="janus829"){source('/Users/janus829/Desktop/Research/Rhodium/R/setup.R')}
if(Sys.info()["user"]=="Ben"){source('/Users/Ben/Github/Rhodium/R/setup.R')}

# Load conflict country year data
setwd(pathData)
load('combinedData.rda')
library(plm)

yData$upperincome=0
yData$upperincome[which(yData$income_l0 %in% c('High income: OECD', 'High income: nonOECD'  ) )] = 1


# CREATE APPROPRIATE VARIABLES FOR REGRESSIONS
###################################################################
yData$lnminDist.min <- log(yData$minDist.min+1)
yData$lnminDist.mean <- log(yData$minDist.mean)
yData$lncapDist.min <- log(yData$capDist.min)
yData$lnArea <- log(yData$Conflict.area.mean)
yData$lnArea_min <- log(yData$Conflict.area_min)
yData$Int.max <- yData$Int.max-1
yData$Int_min <- yData$Int_min-1
yData$intPerKm <- yData$Int.max/yData$lnArea
yData$USA <- yData$ccode==2
yData$coldwar <- yData$year<1991
# yData$BX.KLT.DINV.CD.WD <- 
yData$lnAG.LND.TOTL.K2_l0 = yData$AG.LND.TOTL.K2_l0

yData <- yData[order(yData$year),]
###################################################################


## MODELS FOR GDP GROWTH PER CAPITA (ANNUAL %)
###################################################################
# model1 <- lmer(NY.GDP.PCAP.KD.ZG_l0 ~ upperincome + Int.max + lnArea + lnminDist.min + territorial.max + durSt1max + NY.GDP.DEFL.KD.ZG_l1 + lnAG.LND.TOTL.K2_l0 + (1|ccode) + (1|year), data=yData)
# plot(density(yData$NY.GDP.PCAP.KD.ZG_l0,na.rm=T), frame=F, las=1, ylab="", xlab="GDP Growth per Capita (Annual %)", main="")
# summary(model1)
# model1FE <- lm(NY.GDP.PCAP.KD.ZG_l0 ~ upperincome + Int.max + lnArea + lnminDist.min + territorial.max + durSt1max + NY.GDP.DEFL.KD.ZG_l1 + lnAG.LND.TOTL.K2_l0 + as.factor(ccode), data=yData)
# summary(model1FE)

# resid <- resid(model1)
# resid <- resid+(model1@frame$lnminDist.min*1.3566)
# par(mar=c(5.1,4.1,2.1,2.1))
# plot(model1@frame$lnminDist.min,resid, las=1, ylab="Model Residuals without Distance Measure", xlab="Distance from Major City")
# abline(h=0,lty=2,lwd=3)
# abline(lm(resid~model1@frame$lnminDist.min),lty=1,col="red",lwd=3)
# plot(resid(model1))

# model2 <- lmer(NY.GDP.PCAP.KD.ZG_l1 ~ upperincome + Int.max + lnArea + lnminDist.min + territorial.max + Int.max*lnminDist.min + (1|ccode), data=yData)
# summary(model2)
# plot(resid(model2))
# This model is weird
###################################################################


## MODELS FOR GDP GROWTH (ANNUAL %)
###################################################################
# plot(density(yData$NY.GDP.MKTP.KD.ZG_l0,na.rm=T), frame=F, las=1, ylab="", xlab="GDP Growth (Annual %)", main="")
# names(yData)
# yData$GDP_transform_l0 <- sqrt(abs(yData$NY.GDP.MKTP.KD.ZG_l0))
# yData$GDP_transform_l0 <- yData$GDP_transform_l0 * ifelse(yData$NY.GDP.MKTP.KD.ZG_l0<0,-1,1)
# plot(density(yData$GDP_transform_l0,na.rm=T))

model3 <- lmer(
  NY.GDP.MKTP.KD.ZG_l0 ~ upperincome + Int.max + lnArea +
  lnminDist.min + territorial.max + 
	durSt1max + NY.GDP.DEFL.KD.ZG_l1 + lnAG.LND.TOTL.K2_l0
	+ (1|ccode) + (1|year), data=yData)
summary(model3)

model4 <- lmer(
  NY.GDP.MKTP.KD.ZG_l0 ~ upperincome + Int_min + lnArea_min + 
  lnminDist.min + territorial.max + 
  durSt1max + NY.GDP.DEFL.KD.ZG_l1 + lnAG.LND.TOTL.K2_l0
  + (1|ccode) + (1|year), data=yData)
summary(model4)

model5 = lmer(
  NY.GDP.MKTP.KD.ZG_l0 ~ 
  NY.GDP.MKTP.KD.ZG_l1 + NY.GDP.DEFL.KD.ZG_l1 + lnAG.LND.TOTL.K2_l0 + 
  Int_min + lnArea_min + territorial.max + durSt1max +
  lnminDist.min
  + (1|ccode) + (1|year), data=yData)
summary(model5)

model6 = lmer(
  NY.GDP.MKTP.KD.ZG_l0 ~ 
  NY.GDP.MKTP.KD.ZG_l1 + NY.GDP.DEFL.KD.ZG_l1 + lnAG.LND.TOTL.K2_l0 + 
  Int.mean + lnArea + territorial.max + durSt1max +
  lnminDist.mean
  + (1|ccode) + (1|year), data=yData)
summary(model6)

# model3FE <- lm(NY.GDP.MKTP.KD.ZG_l0 ~ upperincome + Int.max + lnArea + lnminDist.min + territorial.max + durSt1max + NY.GDP.DEFL.KD.ZG_l1 + lnAG.LND.TOTL.K2_l0 + as.factor(ccode), data=yData)
# summary(model3FE)


# resid <- resid(model3)
# resid <- resid+(model3@frame$lnminDist.min*1.3566)
# par(mar=c(5.1,4.1,2.1,2.1))
# plot(model3@frame$lnminDist.min,resid, las=1, ylab="Model Residuals without Distance Measure", xlab="Distance from Major City")
# abline(h=0,lty=2,lwd=3)
# abline(lm(resid~model3@frame$lnminDist.min),lty=1,col="red",lwd=3)

# model4 <- lmer(NY.GDP.MKTP.KD.ZG_l1 ~ upperincome + Int.max + lnArea + lnminDist.min + territorial.max + Int.max*lnminDist.min + (1|ccode), data=yData)
# summary(model4)
# plot(resid(model4))
###################################################################

###################################################################
# Performance

# RMSE
rmse(model3); rmse(model4); rmse(model5); rmse(model6)

# Divide intro training and test

###################################################################

###################################################################
# Coefficient Plots
setwd(pathMain)
source('vizResults.R')

coefs=c('upperincome', 'Int.max', 'lnArea', 'lnminDist.min', 
	'territorial.max', 'durSt1max', 'NY.GDP.DEFL.KD.ZG_l1', 'lnAG.LND.TOTL.K2_l0')

vnames=c('Upper Income', 'Conflict Intensity$_{t}$', 'Ln(Conflict Area)$_{t}$', 'Ln(Min. Conflict Dist.)$_{t}$',
	'Conflict Type$_{t}$', 'Conflict Duration$_{t}$', 'Inflation$_{t-1}$', 'Ln(Land Area)')

temp <- ggcoefplot(coefData=summary(model3)@coefs, 
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