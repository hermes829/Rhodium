require("geonames")
require("RJSONIO")
#library("RJSONIO") #Load Library

locationLookup <- function(locations,country="AF")
{
  country <- rep(country,length.out=length(locations))
  lat <- rep(NA,length(locations))
  lon <- rep(NA,length(locations))
  adm1 <- rep(NA,length(locations))
  options(warn=-1)
  for(i in 1:length(locations))
  {
    loc <- locations[i]
    cty <- country[i]
    results <- GNsearch(name=loc,country=cty)
    try(lat[i] <- results[1,"lat"],silent=T)
    try(lon[i] <- results[1,"lng"],silent=T)
    try(adm1[i] <- results[1,"adminName1"],silent=T)
  } 
  options(warn=0)
  return(data.frame(lat=lat,lon=lon,adm1=adm1))
}

reverseLookup <- function(lat,lon,radius=10)
{
  country <- rep(NA,length(lat))
  adm1 <- country
  toponym <- country
  distance <- country
  options(warn=-1)
  for(i in 1:length(lat))
  {
    templat <- lat[i]
    templon <- lon[i]
    results <- GNfindNearbyPlaceName(templat,templon,radius,1)
    try(country[i] <- results[1,"countryName"],silent=T)
    try(adm1[i] <- results[1,"adminName1"],silent=T)
    try(toponym[i] <- results[1,"toponymName"],silent=T)
    try(distance[i] <- results[1,"distance"],silent=T)
  }
  options(warn=0)
  return(data.frame(country=country,admin1Name=adm1,toponymName=toponym,distance=distance))
}

googleLookup <- function(locname)
{
  locname <- toupper(gsub(' ','%20',locname)) #Encode URL Parameters
  lat <- rep(NA, length(locname))
  lon <- rep(NA, length(locname))
  for(loc in unique(locname))
  {
    results <- getGeoCode(loc)
    lat[locname==loc] <- results[1]
    lon[locname==loc] <- results[2]
  }
  return(data.frame(lat=lat,lon=lon))
}

getGeoCode <- function(gcStr)
{
  #Open Connection
  connectStr <- paste('http://maps.google.com/maps/api/geocode/json?sensor=false&address=',gcStr, sep="") 
  con <- url(connectStr)
  data.json <- fromJSON(paste(readLines(con), collapse=""))
  close(con)
  #Flatten the received JSON
  data.json <- unlist(data.json)
  lat <- data.json["results.geometry.location.lat"]
  lng <- data.json["results.geometry.location.lng"]
  gcodes <- c(lat, lng)
  names(gcodes) <- c("Lat", "Lng")
  return (gcodes)
}