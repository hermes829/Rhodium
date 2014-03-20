if(Sys.info()["user"]=="janus829"){source('/Users/janus829/Desktop/Research/Rhodium/R/setup.R')}
if(Sys.info()["user"]=="Ben"){source('/Users/Ben/Github/Rhodium/R/setup.R')}

# Load conflict country year data
setwd(pathData)
load('countryYear_ConflictData.rda')

# Bring in WB data
# WDIsearch()
# [6,] "BN.KLT.DINV.CD.ZS"              "Foreign direct investment (% of GDP)"                                                                                                      
# [7,] "BN.KLT.PRVT.GD.ZS"              "Private capital flows, total (% of GDP)" 
# [76,] "NY.GDP.DEFL.KD.ZG"              "Inflation, GDP deflator (annual %)"
# [77,] "NY.GDP.DEFL.ZS"                 "GDP deflator (base year varies by country)" 
# 
# [5,] "BX.KLT.DINV.CD.WD"    "Foreign direct investment, net inflows (BoP, current US$)" 
# [94,] "NY.GDP.PCAP.KD"                 "GDP per capita (constant 2000 US$)"
# [95,] "NY.GDP.PCAP.KD.ZG"              "GDP per capita growth (annual %)"
# [82,] "NY.GDP.MKTP.CD"                 "GDP (current US$)"  
# [86,] "NY.GDP.MKTP.KD"                 "GDP (constant 2000 US$)"  
# [87,] "NY.GDP.MKTP.KD.ZG"              "GDP growth (annual %)"
wbData=WDI(country='all', indicator=c("BX.KLT.DINV.CD.WD", "NY.GDP.DEFL.KD.ZG", "NY.GDP.PCAP.KD", "NY.GDP.PCAP.KD.ZG", "NY.GDP.MKTP.CD", "NY.GDP.MKTP.KD", "NY.GDP.MKTP.KD.ZG"), start=1988, end=2010, extra=T)

# Add cnames to this shit
wbData$cname = countrycode(wbData$iso2c, 'iso2c', 'country.name')
wbData = wbData[!is.na(wbData$cname),]
wbData$ccode = panel$ccode[match(wbData$cname, panel$cname)]
wbData = wbData[!is.na(wbData$ccode),]

# Merge into country conflict data
# Lagging all data
wbData$cyear = paste0(wbData$ccode, wbData$year)
yData = merge(yData, wbData[,c(4:10,16,ncol(wbData))], by='cyear', all.x=T, all.y=F)
wbData$cyear = paste0(wbData$ccode, wbData$year-1)
yData = merge(yData, wbData[,c(4:10,16,ncol(wbData))], by='cyear', all.x=T, all.y=F, suffixes=c("_l0",""))
wbData$cyear = paste0(wbData$ccode, wbData$year+1)
yData = merge(yData, wbData[,c(4:10,16,ncol(wbData))], by='cyear', all.x=T, all.y=F, suffixes=c("_l1","_p1"))

# Save this things
setwd(pathData)
save(yData, file='combinedData.rda')
