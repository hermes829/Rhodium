if(Sys.info()["user"]=="janus829"){source('~/Desktop/Research/Rhodium/R/setup.R')}
if(Sys.info()["user"]=="s7m"){source('~/Research/Rhodium/R/setup.R')}
if(Sys.info()["user"]=="Ben"){source('/Users/Ben/Github/Rhodium/R/setup.R')}

##################################################################
# Bring in WB data
# [5,] "BX.KLT.DINV.CD.WD"        "Foreign direct investment, net inflows (BoP, current US$)"
# [12,] "AG.LND.TOTL.K2"        "Land area (sq. km)"
# [76,] "NY.GDP.DEFL.KD.ZG"             "Inflation, GDP deflator (annual %)"
# [94,] "NY.GDP.PCAP.KD"                 "GDP per capita (constant 2000 US$)"
# [95,] "NY.GDP.PCAP.KD.ZG"             "GDP per capita growth (annual %)"
# [86,] "NY.GDP.MKTP.KD"                 "GDP (constant 2000 US$)"
# [87,] "NY.GDP.MKTP.KD.ZG"             "GDP growth (annual %)"
# NY.GDP.TOTL.RT.ZS             "Natual Resource Rents % of GDP"
wbVars=c("BX.KLT.DINV.CD.WD","AG.LND.TOTL.K2", "NY.GDP.DEFL.KD.ZG",
  "NY.GDP.PCAP.KD", "NY.GDP.PCAP.KD.ZG",
  "NY.GDP.MKTP.KD", "NY.GDP.MKTP.KD.ZG", 
  'NY.GDP.TOTL.RT.ZS')
wbData=WDI(country='all', indicator=wbVars,
  start=1988, end=2010, extra=T)
names(wbData)[4:11]=c('fdi', 'landArea', 'inflation', 'gdpCap',
  'gdpCapGr', 'gdp', 'gdpGr', 'resourceGDP')
wbData$cname = toupper(countrycode(wbData$iso2c, 'iso2c', 'country.name'))
wbData = wbData[!is.na(wbData$cname),]
wbData$ccode = panel$ccode[match(wbData$cname, panel$cname)]
wbData = wbData[!is.na(wbData$ccode),]
setwd(pathData)
save(wbData, file='wbData.rda')
##################################################################