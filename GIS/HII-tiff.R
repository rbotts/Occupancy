#R Version 3.4.4
#Convert data on the Human Influence Index (http://dx.doi.org/10.7927/H4BP00QC) from ArcGIS raster format to regular TIFF, then plot that with rworldmap outlines overlaid
require("raster")
require("rworldmap")
require("rworldxtra")

#Function to take the HII data path and output filename, and convert that to a new raster file (format default: tif)
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

#Generate converted raster file
path <- "~/Downloads/hii_n_america_grid/hii_n_amer"
output <- "HII_NorthAmerica.tif"
pyGDAL_translate(path = path, outFile = output)

#Plot Setup
png(filename = paste0(path, "/", output, ".png"), width = 1024, height = 768)
par(mar = c(5,5,5,5)+0.1)
plot(x = c(-84,9), xlim = c(-86,-82), ylim = c(8,11), type = "n", main = "Human Influence Index in Costa Rica", xlab = "Longitude", ylab = "Latitude")

#Load and plot data
hii <- raster(x = paste0(path, "/", output))
newmap <- getMap(resolution = "high")
plot(hii, add = TRUE, maxpixels = 3e7)
plot(newmap, add = TRUE)

#Clean up
par(mar = c(5,4,4,2)+0.1)
dev.off()
