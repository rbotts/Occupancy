#R Version 3.4.4
#Converting X/Y geographic points to Latitude/Longitude
require("proj4") #Requires installing libproj-dev on your system first

xy <- function(x, y) {
  proj4string <- "+proj=utm +zone=17 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "
  pj <- project(data.frame(x,y), proj4string, inverse=TRUE)
  return(data.frame(Latitude=pj$y, Longitude=pj$x))
}

#Adding Longitude and Latitude columns to occ.dat
xy.dat <- xy(x = occ.dat$X, y = occ.dat$Y)
occ.dat <- cbind(occ.dat, xy.dat)
