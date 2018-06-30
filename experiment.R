#R version 3.4.4
#Occupancy analysis experimentation

#Setup ----
require("raster")
require("lubridate")
require("unmarked")
occ.dat <- read.csv("~/Documents/MooringData-June2018-Clean.csv", stringsAsFactors = FALSE)

occ.dat["YearMonth"] <- sapply(X = occ.dat$Date,
                               FUN = function(x) {paste(year(x), month(x))})

height <- raster("GIS/Elevation_CostaRica.tif")
hii <- raster("GIS/HII_NorthAmerica.tif")
woodbiomass <- raster("GIS/WHRC_AbovegroundWodyBiomass.tif")

coords <- data.frame("long" = occ.dat$Longitude, "lat" = occ.dat$Latitude)
occ.dat["Height"] <- extract(x = height, y = coords)
occ.dat["HumanInfluence"] <- extract(x = hii, y = coords)
occ.dat["ForestBiomass"] <-- extract(x = woodbiomass, y = coords)

cat("Height\n")
print(summary(occ.dat$Height))
cat("\nHuman\n")
print(summary(occ.dat$HumanInfluence))
cat("\nForest\n")
print(summary(occ.dat$ForestBiomass))
