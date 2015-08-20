if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
  source('~/Research/Rhodium/R/setup.R')}
if(Sys.info()["user"]=="Ben"){source('/Users/Ben/Github/Rhodium/R/setup.R')}

# Load conflict country year data
setwd(pathData)
load('fullData.rda')

slice = fullData[,c('ccode','year','gdpGr', 'minDist.min')]
slice$lnminDist.min = log(slice$minDist.min + 1)
slice$lnminDist.min[is.na(slice$lnminDist.min)] = 0

cntryData = slice[slice$ccode==100,]
library(gridExtra)
gdpTime = ggplot(cntryData, aes(x=year, y=gdpGr)) + geom_line()
distTime = ggplot(cntryData, aes(x=year, y=minDist.min)) + geom_bar(stat='identity')
grid.arrange(gdpTime, distTime)