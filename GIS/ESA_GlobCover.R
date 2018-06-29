#R Version 3.4.4
#Playing with land cover data from the ESA's GlobCover project (http://due.esrin.esa.int/page_globcover.php)
#To interpret the values, see the validation document, section 4.1 (http://due.esrin.esa.int/files/GLOBCOVER2009_Validation_Report_2.2.pdf)
require("raster")
require("rworldmap")
require("rworldxtra")

#Import GlobCover data
globcover <- raster("~/Downloads/GLOBCOVER_L4_200901_200912_V2.3.tif")

#Zoom in to only Costa Rica
costaRicaExtent <- extent(x = -86, xmax = -82, ymin = 8, ymax = 11)
costaRicaCover <- crop(x = globcover, y = costaRicaExtent)

#Ignore "open ocean" values (210)
costaRicaCover[costaRicaCover == 210] <- NA

#Plot and overlay map outlines
par(mar = c(5,5,5,5)+0.1)
plot(
  x = c(-84, 9),
  xlim = c(-86, -82),
  ylim = c(8, 11),
  type = "n",
  main = "Land Cover in Costa Rica",
  xlab = "Longitude",
  ylab = "Latitude"
)
plot(costaRicaCover, add = TRUE)
newmap <- getMap(resolution = "high")
plot(newmap, add = TRUE)
par(mar = c(5,4,4,2)+0.1)
