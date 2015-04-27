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
cityPop$Country=charSM(cityPop$Country)
cityPop$Country=gsub('&', 'and', cityPop$Country)
cityPop$Country[cityPop$Country=='Soviet Union']='USSR'
cityPop$Country[cityPop$Country=='Ivory Coast']="Cote d'Ivoire"
cityPop$Country[cityPop$Country=='Republic of the Congo']='Congo'
cityPop$Country[cityPop$Country=='Democratic Republic of the Congo']='Congo, Democratic Republic of'
cityPop$Country[cityPop$Country=='Quatar']='Qatar'
cityPop$Country[cityPop$Country=='Swenen']='Sweden'
cityPop$Country[cityPop$Country=='Ukaraine']='Ukraine'
cityPop$Country[cityPop$Country=='Untied Arab Emirates']='United Arab Emirates'
cityPop$Country[cityPop$Country=='Republic of Yemen']='Yemen'
cityPop$Country[cityPop$Country=='Djbouti']='Djibouti'
cityPop$Country[cityPop$Country=='Serbia & Montenegro']='Serbia and Montenegro'
cityPop$Country[cityPop$Country=='Solomon Islands']='Solomon Is.'

cityPop$cname=panel$cname[match(cityPop$Country, panel$CNTRY_NAME)]
unique(cityPop[is.na(cityPop$cname),c('Country', 'cname')]) # Check NAs
##########################################

##########################################
# Add WB pop data
wbPop=WDIsearch(string="Population, total")[1]
popData=WDI(country='all', indicator=wbPop, start=1989, end=2008)
popData$country[popData$country=='Bahamas, The']='Bahamas'
popData$country[popData$country=='Brunei Darussalam']='Brunei'
popData$country[popData$country=='Congo, Rep.']='Congo'
popData$country[popData$country=='Congo, Dem. Rep.']='Congo, Democratic Republic of'
popData$country[popData$country=='Egypt, Arab Rep.']='Egypt'
popData$country[popData$country=='Gambia, The']='The Gambia'
popData$country[popData$country=='Iran, Islamic Rep.']='Iran'
popData$country[popData$country=='Korea, Dem. Rep.']='North Korea'
popData$country[popData$country=='Korea, Rep.']='South Korea'
popData$country[popData$country=='Kyrgyz Republic']='Kyrgyzstan'
popData$country[popData$country=='Lao PDR']='Laos'
popData$country[popData$country=='Macedonia, FYR']='Macedonia'
popData$country[popData$country=='Marshall Islands']='Marshall Is.'
popData$country[popData$country=='Micronesia, Fed. Sts.']='Micronesia'
popData$country[popData$country=='Russian Federation']='Russia'
popData$country[popData$country=='Slovak Republic']='Slovakia'
popData$country[popData$country=='Solomon Islands']='Solomon Is.'
popData$country[popData$country=='Syrian Arab Republic']='Syria'
popData$country[popData$country=='Timor-Leste']='Timor Leste'
popData$country[popData$country=='Venezuela RB']='Venezuela'
popData$country[popData$country=='Yemen, Rep.']='Yemen'


popData$cname=panel$cname[match(popData$country, panel$CNTRY_NAME)]
unique(popData[is.na(popData$cname),c('country', 'cname')]) # Check NAs
# [301,] "Population in largest city"
# [302,] "Population in the largest city (% of urban population)"
# [303,] "Population in urban agglomerations of more than 1 million"
# [304,] "Population in urban agglomerations of more than 1 million (% of total population)"
##########################################

##########################################
# Add pop data to city data
cityPop$TotPop=popData$SP.POP.TOTL[match(cityPop$cname, popData$cname)]
##########################################

##########################################
# Save combined file
setwd(pathData)
save(cityPop, file='cityTotPopLatLong.rda')
##########################################