##########################################
# Set workspace
source('/Users/janus829/Desktop/Research/Rhodium/R/setup.R')
##########################################

##########################################
# Load city data
setwd(paste(pathData,'/CityPopData/Combined',sep=''))
cityPop=read.csv('CityPopData.csv')
cityLL=read.csv('cityLatLong.csv')
##########################################

##########################################
# Create unique identifiers and combine
cityPop$cntCty=paste(cityPop$Country, cityPop$City, sep='_')
cityLL$cntCty=paste(cityLL$Country, cityLL$City, sep='_')

cityPop$Lat=cityLL$Lat[match(cityPop$cntCty, cityLL$cntCty)]
cityPop$Long=cityLL$Long[match(cityPop$cntCty, cityLL$cntCty)]
##########################################

##########################################
# Cleaning country names

##########################################

##########################################
# Add WB pop data
wbPop=WDIsearch(string="Population, total")[1]
popData=WDI(country='all', indicator=wbPop, start=1989, end=2008)


# [301,] "Population in largest city"
# [302,] "Population in the largest city (% of urban population)"
# [303,] "Population in urban agglomerations of more than 1 million"
# [304,] "Population in urban agglomerations of more than 1 million (% of total population)"
##########################################

##########################################
# Save combined file
##########################################