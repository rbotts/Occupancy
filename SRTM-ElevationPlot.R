require("raster")
require("rworldmap")
require("rworldxtra")

#Function to take a directory's path, output filename, and
pyGDAL_merge_STRM <- function(path, outFile, xlim, ylim) {

  #Get path to python executable
  pypath <- Sys.which('gdal_merge.py')
  if (!file.exists(pypath)) stop("Can't find gdal_merge.py on your system. On Debian-based systems, try `apt-get install python-gdal`.")

  #Change working directory as necessary, but change back at the end
  owd <- getwd()
  on.exit(setwd(owd))
  setwd(path)

  #Make vector of SRTM filenames
  fileList <- sapply(X = xlim[1]:xlim[2],
                     FUN = function(x) {
                       if (max(abs(x)) > 180) stop("Longitude > +-180 impossible. Abort!")
                       return(
                         sapply(ylim[1]:ylim[2], FUN = function(y) {
                           if (max(abs(y)) > 60) stop("No SRTM latitude > +-60. Abort!")
                           EW <- ifelse(test = x >= 0, yes = "E", no = "W")
                           NS <- ifelse(test = y >= 0, yes = "N", no = "S")
                           x <- abs(x)
                           y <- abs(y)
                           lon <- ifelse(test = nchar(x) <= 2, no = as.character(x),
                                         yes = ifelse(test = nchar(x) == 1,
                                                      yes = paste0("0", "0", x),
                                                      no = paste0("0", x)))
                           lat <- ifelse(test = nchar(y) == 1,
                                         yes = paste0("0", y),
                                         no = y)
                           return(paste0(NS, lat, EW, lon, ".hgt"))
                         })
                       )
                     })
  fileList <- as.vector(fileList)

  #Only keep files that actually exist
  fileList <- sapply(X = fileList,
                     FUN = function(x) {
                       filePath <- paste0(path, "/", x)
                       if (file.exists(filePath)) return(x)
                     })
  fileList <- unlist(fileList)
  names(fileList) <- NULL
  if (is.null(fileList)) stop("No SRTM HGT files found! Are you sure `path` is accurate?")
  fileList <- paste(fileList, collapse = " ")

  mergeCommand <- paste0(pypath, " -o ", outFile, " ", fileList)
  cat(mergeCommand, "\n")
  system(command = mergeCommand)
  cat(paste0("Merged file written to ", path, "/", outFile, "\n"))
}

#Set path to folder containing unzipped .hgt files
path <- "~/Downloads/HGT"
outFile <- "rpytest.tif"

#Merge selected STRM files into one raster file
pyGDAL_merge_STRM(path = path,
                  outFile = outFile,
                  xlim = c(-86,-82), ylim = c(8,11))

#Plot setup
png(filename = paste0(path, "/", "CR-SRTM.png"), width = 1024, height = 768)
par(mar = c(5,5,5,5)+0.1)
plot(x = 0, y = 0, xlim = c(-86,-82), ylim = c(8,11), xlab = "Longitude", ylab = "Latitude", main = "Elevation in Costa Rica", type = "n")

#Plot raster data
elevation <- raster(paste0(path, "/", outFile))
plot(elevation, add = TRUE)

#Plot map outlines
newmap <- getMap(resolution = "high")
plot(newmap, add = TRUE, xlim = c(-86,-82), ylim = c(8,11))

par(bg = "white", mar = c(5,4,4,2)+0.1)
dev.off()
