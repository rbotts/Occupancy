#R Version 3.4.4
#Convert data on Aboveground Woody Biomass (https://databasin.org/datasets/679d83420e464ff9aff714184c2bbe09) from ArcGIS raster format to regular TIFF, warp to a new projection, then plot that with rworldmap outlines overlaid
require("raster")
require("rworldmap")
require("rworldxtra")

#Function to take the biomass data path and output filename, and convert that to a new raster file (format default: tif)
#Note: this function is copied from HII-tiff.R, make sure to copy any changes between there
pyGDAL_translate <- function(path, outFile, fileFormat = "GTiff") {

  #Get path to python executable
  pypath <- Sys.which('gdal_translate')
  if (!file.exists(pypath)) stop("Can't find gdal_translate on your system. On Debian-based systems, try `apt-get install python-gdal`.")

  #Change working directory as necessary, but change back at the end
  owd <- getwd()
  on.exit(setwd(owd))
  setwd(path)

  #Append .tif extension if not there (when converting to GTiff)
  if (fileFormat == "GTiff" & extension(outFile) != ".tif" & extension(outFile) != ".tiff") {
    warning("The MIME extension of outFile doesn't look right. Appended \".tif\" to it.")
    outFile <- paste0(outFile, ".tif")
  }

  transCommand <- paste0(pypath, " -of ", fileFormat, " ", path, " ", path, "/", outFile)
  cat(transCommand, "\n\n")
  system(command = transCommand)
  cat(paste0("\n", "Converted file written to ", path, "/", outFile, "\n\n"))
}

#Function to warp biomass from utm projection to longlat "projection"
pyGDAL_warp <- function(inFile, outFile, targetProj = "+proj=longlat") {

  #Get path to python executable
  pypath <- Sys.which('gdalwarp')
  if (!file.exists(pypath)) stop("Can't find gdalwarp on your system. On Debian-based systems, try `apt-get install python-gdal`.")

  #Build and execute command
  warpCommand <- paste0(pypath, " -t_srs ", "\'", targetProj, "\'", " -overwrite ", inFile, " ", outFile)
  cat("\n", warpCommand, "\n\n")
  system(command = warpCommand)
  cat("\n", "Warped file written to", outFile, "\n\n")
}

#Generate converted raster file
path <- "~/Downloads/WoodyBiomass/data/commondata/data0/whrc_agb_saea"
output <- "WoodyBiomass.tif"
pyGDAL_translate(path = path, outFile = output)
pyGDAL_warp(inFile = paste0(path, "/", output), outFile = paste0(path, "/", "warped-", output))

#Load data
woodybiomass <- raster(x = paste0(path, "/", "warped-", output))
newmap <- getMap(resolution = "high")

#Zoom in to only Costa Rica
costaRicaExtent <- extent(x = -86, xmax = -82, ymin = 8, ymax = 11)
woodybiomass <- crop(x = woodybiomass, y = costaRicaExtent)

#Plot Setup
png(filename = "Plots/WHRC-WoodyBiomass.png", width = 1024, height = 768)
par(mar = c(5,5,5,5)+0.1)
plot(x = c(-84,9), xlim = c(-86,-82), ylim = c(8,11), type = "n", main = "Aboveground Woody Biomass in Costa Rica (Mg/Hectare)", xlab = "Longitude", ylab = "Latitude")

#Plot data and map
plot(woodybiomass, add = TRUE)
plot(newmap, add = TRUE)

#Clean up
par(mar = c(5,4,4,2)+0.1)
dev.off()
