#R version 3.4.4
#Occupancy analysis experimentation

#Setup ----
require("raster")
require("lubridate")
require("unmarked")
occ.dat <- read.csv("~/Documents/MooringData-June2018-Clean.csv", stringsAsFactors = FALSE)

#Add a more granular/sensible "season" column to occ.dat
occ.dat["YearMonth"] <- sapply(X = occ.dat$Date,
                               FUN = function(x) {paste(year(x), month(x))})

#Add raster data ----
#Load some raster datasets
height <- raster("GIS/Elevation_CostaRica.tif")
hii <- raster("GIS/HII_NorthAmerica.tif")
woodbiomass <- raster("GIS/WHRC_AbovegroundWoodBiomass.tif")

#hii has some NA values, so we crop down to Costa Rica (otherwise it takes up a *lot* of memory) and average the surrounding cells to guess something reasonable for those values
costaRicaExtent <- extent(x = -86, xmax = -82, ymin = 8, ymax = 11)
hii <- crop(x = hii, y = costaRicaExtent)
hii <- focal(x = hii,
             w = matrix(data = 1, nrow = 3, ncol = 3),
             fun = mean,
             na.rm = TRUE,
             NAonly = TRUE)

#Add columns to occ.dat containing appropriate values from raster data
coords <- data.frame("long" = occ.dat$Longitude, "lat" = occ.dat$Latitude)
occ.dat["Height"] <- extract(x = height, y = coords)
occ.dat["HumanInfluence"] <- extract(x = hii, y = coords)
occ.dat["ForestBiomass"] <- extract(x = woodbiomass, y = coords)

#Looking at raster data associated with our camera sites
cat("Height\n")
print(summary(occ.dat$Height))
cat("\nHuman\n")
print(summary(occ.dat$HumanInfluence))
cat("\nForest\n")
print(summary(occ.dat$ForestBiomass))
