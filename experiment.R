#R version 3.4.4
#Occupancy analysis experimentation

#Setup ----
require("raster")
require("lubridate")
require("unmarked")
require("tictoc")
occ.dat <- read.csv("~/Documents/MooringData-June2018-Clean.csv", stringsAsFactors = FALSE)

#Add more granular/sensible "season" columns to occ.dat
occ.dat["YearMonth"] <- sapply(X = occ.dat$Date,
                               FUN = function(x) {
                                 if (nchar(as.character(month(x))) == 1) {
                                   paste0(year(x), " 0", month(x))
                                 } else paste(year(x), month(x))
                               })

#Add raster data ----
tic("Load and crop raster data")
#Load some raster datasets
height <- raster("GIS/Elevation_CostaRica.tif")
hii <- raster("GIS/HII_NorthAmerica.tif")
woodbiomass <- raster("GIS/WHRC_AbovegroundWoodBiomass.tif")

#Crop rasters down to Costa Rica
costaRicaExtent <- extent(x = -86, xmax = -82, ymin = 8, ymax = 11)
height <- crop(x = height, y = costaRicaExtent)
height[height == 0] <- NA #Nothing should be at 0 elevation, at least not without some higher/lower land right nearby
hii <- crop(x = hii, y = costaRicaExtent)
woodbiomass <- crop(x = woodbiomass, y = costaRicaExtent)
toc()

#Average surrounding cells to get smoothed distributions without NA holes
tic("Focal raster data")
height <- focal(x = height,
                w = matrix(data = 1, nrow = 5, ncol = 5), #more aggressive smoothing because some large holes in mountain data, need to be smoothed over
                fun = mean,
                na.rm = TRUE)
hii <- focal(x = hii,
             w = matrix(data = 1, nrow = 3, ncol = 3),
             fun = mean,
             na.rm = TRUE)
woodbiomass <- focal(x = woodbiomass,
                     w = matrix(data = 1, nrow = 3, ncol = 3),
                     fun = mean,
                     na.rm = TRUE)
toc()

#Add columns to occ.dat containing appropriate values from raster data
coords <- data.frame("long" = occ.dat$Longitude, "lat" = occ.dat$Latitude)
occ.dat["Height"] <- extract(x = height, y = coords)
occ.dat["HumanInfluence"] <- extract(x = hii, y = coords)
occ.dat["ForestBiomass"] <- extract(x = woodbiomass, y = coords)

#Looking at raster data associated with our camera sites
# cat("Height\n")
# print(summary(occ.dat$Height))
# cat("\nHuman\n")
# print(summary(occ.dat$HumanInfluence))
# cat("\nForest\n")
# print(summary(occ.dat$ForestBiomass))
# cat("\n")

#Preparing UMF ----
studySpecies <- "Puma concolor"
ind.dat <- occ.dat[occ.dat$Independent == "Yes" &
                     occ.dat$Species == studySpecies
                   ,]

#see how many times a camera is active in a month *and* captures studySpecies at some point in that month
# rec <- c()
# i <- 1
# for (ym in sort(unique(occ.dat$YearMonth))) {
#   for (cam in sort(unique(occ.dat$CamNumber1))) {
#     if (length(occ.dat$Species[occ.dat$CamNumber1 == cam & occ.dat$YearMonth == ym]) > 0) {
#       rec[i] <- studySpecies %in% occ.dat$Species[occ.dat$CamNumber1 == cam & occ.dat$YearMonth == ym]
#       i <- i+1
#     } else {
#       rec [i] <- NA
#       i <- i+1
#     }
#   }
# }
#
# print(sum(rec, na.rm = TRUE) / sum(!is.na(rec)))
# 0.217691
#Puma are detected in 21.8% of surveys

#Building UMF ----
#Define: season = season, survey = yearmonth, site = CamNumber1

#Get list of sites that have ever detected studySpecies
siteList <- sort(unique(ind.dat$Site))

#Get list of cameras at those sites
camList <- sort(unique(occ.dat$CamNumber1[occ.dat$Site %in% siteList]))

#Get list of surveys in which those cameras were active
surveyList <- sort(unique(subset(x = occ.dat$YearMonth,
                                 subset = length(occ.dat$YearMonth[occ.dat$CamNumber1 %in% camList])
                                          > 0
                                 )))

#Create list of seasons -- ***meta knowledge***: want Winter 2010 thru Spring 2018
seasonList <- c("2010 Winter", paste(rep(2011:2017, each = 4), rep(c("Spring", "Summer", "Fall", "Winter"), times = 7)), "2018 Spring")
#Note that Winter 20xx = Dec 20xx, Jan 20xx+1, Feb 20xx+1

#Generate the detection matrix y
y <- matrix(data = NA,
            nrow = length(camList),
            ncol = 3*length(seasonList),
            dimnames = list("Site" = camList,
                            "Survey" = paste(rep(seasonList, each = 3),
                                             rep(c(12,1:11), times = length(seasonList)/4))
                            ))
tic("Generate detection matrix y") #Note: this took over an hour when I ran it on my machine; be prepared to wait!
for (ro in 1:length(camList)) {
  cat("Working on camera", camList[ro], "...\n")
  for (season in seasonList) {
    for (i in 1:3) {
      co <- i + which(seasonList == season) - 1
      #print(season)
      seas <- strsplit(x = season, split = " ", fixed = TRUE)[[1]]
      #print(seas)
      yr <- as.numeric(seas[1])
      sea <- seas[2]
      if (sea == "Winter") { #Get months for winter seasons
        m <- switch(EXPR = as.character(i),
                    "1" = 12,
                    "2" = 1,
                    "3" = 2)
        if (m != 12) yr <- yr + 1 #include NEXT year's data for Jan/Feb
      } else if (sea == "Spring") { #Get months for spring seasons
        m <- switch(EXPR = as.character(i),
                    "1" = 3,
                    "2" = 4,
                    "3" = 5)
      } else if (sea == "Summer") { #Get months for summer seasons
        m <- switch(EXPR = as.character(i),
                    "1" = 6,
                    "2" = 7,
                    "3" = 8)
      } else if (sea == "Fall") { #Get months for fall/autumn seasons
        m <- switch(EXPR = as.character(i),
                    "1" = 9,
                    "2" = 10,
                    "3" = 11)
      } else {
        m <- NA
        warning("Error! Season not recognized!")
      }
      #See if site was active that month...
      check <- length(occ.dat$Time[occ.dat$CamNumber1 == camList[ro]
                                   & month(occ.dat$Date) == m
                                   & year(occ.dat$Date) == yr])
      #print(check)
      #Get number of observations of studySpecies at that site that month...
      obs <- length(ind.dat$Time[ind.dat$CamNumber1 == camList[ro]
                                 & month(ind.dat$Date) == m
                                 & year(ind.dat$Date) == yr])
      #print(obs)
      #Record NA if site was not active that month, otherwise record obs (may be zero, that's intentional)
      y[ro,co] <- ifelse(test = check > 1,
                         yes = obs,
                         no = NA)
    }
  }
}
toc()

#Generate the site-covariates data.frame siteFrame
hgt <- sapply(X = camList, FUN = function(x) {mean(occ.dat$Height[occ.dat$CamNumber1 == x])})
human <- sapply(X = camList, FUN = function(x) {mean(occ.dat$HumanInfluence[occ.dat$CamNumber1 == x])})
wood <- sapply(X = camList, FUN = function(x) {mean(occ.dat$ForestBiomass[occ.dat$CamNumber1 == x])})
siteFrame <- data.frame("Site" = 1:length(camList), "Height" = hgt, "HumanInfluence" = human, "WoodBiomass" = wood, row.names = camList)

#Generate the observation-covariates list of data.frames obsList
surveyMat <- matrix(data = rep(1:(3*length(seasonList)), each = length(camList)),
                    nrow = length(camList),
                    ncol = 3*length(seasonList),
                    dimnames = dimnames(y))
obsList <- list("survey" = as.data.frame(surveyMat))

#Generate the season-covariates list of matrices seaList
nuMat <- matrix(data = rep(1:length(seasonList), each = length(camList)),
                nrow = length(camList),
                ncol = length(seasonList))
seaList <- list("season" = nuMat)

#FINALLY make the frame
umf <- unmarkedMultFrame(numPrimary = length(seasonList),
                         y = y,
                         siteCovs = siteFrame,
                         obsCovs = obsList,
                         yearlySiteCovs = seaList)
print(summary(umf))
