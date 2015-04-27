#####################################
# TITLE:    geodistance             #
# AUTHOR:   Ben Radford             #
# DATE:     June 22, 2013           #
# SUMMARY:  Calculates the distance #
# between two points given latitude #
# and longitude using the Haversine #
# formula (assumes spherical earth) #
#####################################

if(!"geosphere"%in%rownames(installed.packages()))
  install.packages("geosphere")
library("geosphere")

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
  return(result)
}
##############################################################################


##############################################################################
# FIND THE SUM OF LOCATIONS WITHIN A GIVEN RADIUS OF A POINT #################
##############################################################################
sumInRadius <- function(lat1,lon1,lat2,lon2,radius)
{
  return(sum(vincenty(lat1,lon1,lat2,lon2)<=radius))
}

inRadius <- function(lat1,lon1,country1,year1,lat2,lon2,country2,year2,radius)
{
  lat1 <- as.matrix(lat1)
  lat2 <- as.matrix(lat2)
  lon1 <- as.matrix(lon1)
  lon2 <- as.matrix(lon2)
  country1 <- as.character(country1)
  country2 <- as.character(country2)
  locations1 <- data.frame(lat1=as.numeric(lat1),lon1=as.numeric(lon1),country1=country1,year1=year1,stringsAsFactors=F)
  locations2 <- data.frame(lat2=as.numeric(lat2),lon2=as.numeric(lon2),country2=country2,year2=year2,stringsAsFactors=F)
  results <- data.frame(value=rep(NA,nrow(locations1)),country1=rep(NA,nrow(locations1)),country2=rep(NA,nrow(locations1)),year=rep(NA,nrow(locations1)),whichone=rep(NA,nrow(locations1)))
  if(length(setdiff(unique(country1),unique(country2)))!=0)
    cat(paste("Warning: ",setdiff(unique(country1),unique(country2))," is/are in set 1 but not set 2.\n",sep=""))
  if(length(setdiff(unique(country2),unique(country1)))!=0)
    cat(paste("Warning: ",setdiff(unique(country2),unique(country1))," is/are in set 2 but not set 1.\n",sep=""))
  for(i in 1:nrow(locations1))
  {
    c1 <- locations1$country1[i]
    y <- locations1$year1[i]
    temp2 <- locations2[locations2$country2==locations1$country1[i] & locations2$year2==locations1$year1[i],]
    if(nrow(temp2)>0)
    {
      temp2$lat1 <- locations1$lat1[i]
      temp2$lon1 <- locations1$lon1[i]
      results$value[i] <- sumInRadius(temp2$lat1,temp2$lon1,temp2$lat2,temp2$lon2,radius[i])
      c2 <- unique(temp2$country2)
      whichone <- 1
      if(is.na(results$value[i]))
        whichone <- 3
    }
    else
    {
      results$value[i] <- NA
      c2 <- NA
      whichone <- 2
    }
    results$country1[i] <- c1
    results$country2[i] <- c2
    results$year[i] <- y
    results$whichone[i] <- whichone
  }
  return(results)
}
##############################################################################


##############################################################################
# FIND THE MIN DISTANCE FROM POINT A TO ALL OF POINTS ########################
##############################################################################
minRadius <- function(lat1,lon1,lat2,lon2)
{
#   print(cbind(lat1,lon1,lat2,lon2))
  dists <- vincenty(lat1,lon1,lat2,lon2)
  return(min(dists))
}

minDist <- function(lat1,lon1,country1,year1,lat2,lon2,country2,year2)
{
#   lat1 <- as.matrix(lat1)
#   lat2 <- as.matrix(lat2)
#   lon1 <- as.matrix(lon1)
#   lon2 <- as.matrix(lon2)
  country1 <- as.character(country1)
  country2 <- as.character(country2)
  locations1 <- data.frame(lat1=as.numeric(lat1),lon1=as.numeric(lon1),country1=country1,year1=year1,stringsAsFactors=F)
  locations2 <- data.frame(lat2=as.numeric(lat2),lon2=as.numeric(lon2),country2=country2,year2=year2,stringsAsFactors=F)
  results <- rep(NA,nrow(locations1))
  if(length(setdiff(unique(country1),unique(country2)))!=0)
    cat(paste("Warning: ",setdiff(unique(country1),unique(country2))," is/are in set 1 but not set 2.\n",sep=""))
  if(length(setdiff(unique(country2),unique(country1)))!=0)
    cat(paste("Warning: ",setdiff(unique(country2),unique(country1))," is/are in set 2 but not set 1.\n",sep=""))
  for(i in 1:nrow(locations1))
  {
    temp2 <- locations2[locations2$country2==locations1$country1[i] & locations2$year2==locations1$year1[i],]
    temp2$lat1 <- locations1$lat1[i]
    temp2$lon1 <- locations1$lon1[i]
    results[i] <- minRadius(temp2$lat1,temp2$lon1,temp2$lat2,temp2$lon2)
  }
  return(results)
}
##############################################################################

cat("geodistance functions loaded...\n")
cat("Functions:\n")
cat("\u2022 minDist(lat1,lon1,country1,lat2,lon2,country2)\n")
cat("\t \u21d2 Returns the minimum distance between point lat1, lon1 and points lat2, lon2 within the same country.\n")
cat("\u2022 inRadius(lat1,lon1,country1,lat2,lon2,country2,radius=100)\n")
cat("\t \u21d2 Returns the number of points in lat2, lon2, within radius (km) of lat1, lon1 for each country.\n")
cat("Notes:\n")
cat("Functions default to Vincenty distance given WGS84 projection.\n")
