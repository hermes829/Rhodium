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

model1 <- lmer(BX.KLT.DINV.CD.WD_l0 ~ upperincome + Int.max + lnminDist.min + (1|ccode), data=yData)
summary(model1)
##################################################################

