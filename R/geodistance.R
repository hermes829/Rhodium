geodistance <- function(lat1,lon1,lat2,lon2,r=6371)
{
  dLat <- deg2rad(lat2-lat1)
  dLon <- deg2rad(lon2-lon1)
  lat1 <- deg2rad(lat1)
  lat2 <- deg2rad(lat2)
  
  a = sin(dLat/2) * sin(dLat/2) + sin(dLon/2) * sin(dLon/2) * cos(lat1) * cos(lat2)
  c = 2 * atan2(sqrt(a), sqrt(1-a)) 
  d = r * c
  return(d)
}
deg2rad <- function(deg)
{
  return(deg*(pi/180))
}