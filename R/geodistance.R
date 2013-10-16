#####################################
# TITLE:    geodistance             #
# AUTHOR:   Ben Radford             #
# DATE:     June 22, 2013           #
# SUMMARY:  Calculates the distance #
# between two points given latitude #
# and longitude using the Haversine #
# formula (assumes spherical earth) #
#####################################

require("geosphere")

##############################################################################
# HAVERSINE FORMULA FOR DISTANCE ON A SPHERE #################################
##############################################################################
geodistance <- function(lat1,lon1,lat2,lon2,r=6371)
{
  dLat <- deg2rad(lat2-lat1)
  dLon <- deg2rad(lon2-lon1)
  lat1 <- deg2rad(lat1)
  lat2 <- deg2rad(lat2)
  
  a <- sin(dLat/2) * sin(dLat/2) + sin(dLon/2) * sin(dLon/2) * cos(lat1) * cos(lat2)
  c <- 2 * atan2(sqrt(a), sqrt(1-a)) 
  d <- r * c
  return(d)
}
deg2rad <- function(deg)
{
  return(deg*(pi/180))
}
##############################################################################


##############################################################################
# WRAPPER FOR VINCENTY FORMULA - DISTANCE ON ELLIPSOID #######################
##############################################################################
vincenty <- function(lat1,lon1,lat2,lon2,unit="km")
{
  coords1 <- cbind(lon=lon1,lat=lat1)
  coords2 <- cbind(lon=lon2,lat=lat2)
  result <- distVincentyEllipsoid(coords1,coords2)
  if(unit=="km")
    result <- result/1000
  print(result)
  return(result)
}
##############################################################################


##############################################################################
# FIND THE SUM OF LOCATIONS WITHIN A GIVEN RADIUS OF A POINT #################
##############################################################################
sumInRadius <- function(lat1,lon1,lat2,lon2,radius=100)
{
  print(cbind(lat1,lon1,lat2,lon2))
  return(sum(vincenty(lat1,lon1,lat2,lon2)<=radius))
}

inRadius <- function(lat1,lon1,country1,lat2,lon2,country2,radius=100)
{
  lat1 <- as.matrix(lat1)
  lat2 <- as.matrix(lat2)
  lon1 <- as.matrix(lon1)
  lon2 <- as.matrix(lon2)
  country1 <- as.character(country1)
  country2 <- as.character(country2)
  locations1 <- data.frame(lat1=as.numeric(lat1),lon1=as.numeric(lon1),country1=country1,stringsAsFactors=F)
  locations2 <- data.frame(lat2=as.numeric(lat2),lon2=as.numeric(lon2),country2=country2,stringsAsFactors=F)
  results <- rep(NA,nrow(locations1))
  if(length(setdiff(unique(country1),unique(country2)))!=0)
    cat(paste("Warning: ",setdiff(unique(country1),unique(country2))," is/are in set 1 but not set 2.\n",sep=""))
  if(length(setdiff(unique(country2),unique(country1)))!=0)
    cat(paste("Warning: ",setdiff(unique(country2),unique(country1))," is/are in set 2 but not set 1.\n",sep=""))
  for(i in 1:nrow(locations1))
  {
    temp2 <- locations2[locations2$country2==country1[i],]
    temp2$lat1 <- locations1$lat1[i]
    temp2$lon1 <- locations1$lon1[i]
    results[i] <- sumInRadius(temp2$lat1,temp2$lon1,temp2$lat2,temp2$lon2,radius)
  }
  return(results)
}
##############################################################################


##############################################################################
# FIND THE MIN DISTANCE FROM POINT A TO ALL OF POINTS ########################
##############################################################################
minRadius <- function(lat1,lon1,lat2,lon2)
{
  print(cbind(lat1,lon1,lat2,lon2))
  return(min(vincenty(lat1,lon1,lat2,lon2)))
}

minDist <- function(lat1,lon1,country1,lat2,lon2,country2)
{
  lat1 <- as.matrix(lat1)
  lat2 <- as.matrix(lat2)
  lon1 <- as.matrix(lon1)
  lon2 <- as.matrix(lon2)
  country1 <- as.character(country1)
  country2 <- as.character(country2)
  locations1 <- data.frame(lat1=as.numeric(lat1),lon1=as.numeric(lon1),country1=country1,stringsAsFactors=F)
  locations2 <- data.frame(lat2=as.numeric(lat2),lon2=as.numeric(lon2),country2=country2,stringsAsFactors=F)
  results <- rep(NA,nrow(locations1))
  if(length(setdiff(unique(country1),unique(country2)))!=0)
    cat(paste("Warning: ",setdiff(unique(country1),unique(country2))," is/are in set 1 but not set 2.\n",sep=""))
  if(length(setdiff(unique(country2),unique(country1)))!=0)
    cat(paste("Warning: ",setdiff(unique(country2),unique(country1))," is/are in set 2 but not set 1.\n",sep=""))
  for(i in 1:nrow(locations1))
  {
    temp2 <- locations2[locations2$country2==country1[i],]
    temp2$lat1 <- locations1$lat1[i]
    temp2$lon1 <- locations1$lon1[i]
    results[i] <- minRadius(temp2$lat1,temp2$lon1,temp2$lat2,temp2$lon2)
  }
  return(results)
}
##############################################################################

locs1 <- googleLookup(c("Durham, NC","London, England"))
locs1$country <- c("USA","UK")
locs2 <- googleLookup(c("Raleigh,NC","Durham,NC","Hendersonville,NC","Seattle, WA","Cary, NC","Sheffield, England","Kingson, England","Winston Salem, NC","Paris, France"))
locs2$country <- c("USA","USA","USA","USA","USA","UK","UK","USA","FRA")
minDist(locs1$lat,locs1$lon,locs1$country,locs2$lat,locs2$lon,locs2$country)