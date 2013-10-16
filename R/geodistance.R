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

vincenty <- function(lat1,lon1,lat2,lon2,unit="km")
{
  coords1 <- data.frame(lon=lon1,lat=lat1)
  coords2 <- data.frame(lon=lon2,lat=lat2)
  result <- distVincentyEllipsoid(coords1,coords2)
  if(unit=="km")
    result <- result/1000
  return(result)
}

vincenty(c(-16.499,35.9886),c(-68.146,78.9072),c(35.318,35.7789),c(-82.461,78.8003))