#R version 3.4.4
#Covariate formatting functions
require("lubridate")

# Example fileInput, studySpecies, and covName
# inFile <- "~/Documents/MooringData2018.csv"
# study  <- "Puma concolor"
# covAlt <- "Elevation"

#Utility function to find the most common item in a vector
common <- function(x) return(names(tail(sort(table(x)), n = 1)))

#Function to take an occInput data.frame (occData) and species of interest (studySpecies) and output a covariate matrix of the year of each survey
occCov.Year <- function(occData, studySpecies) {
  #Data setup ----
  ind.dat <- subset(occData, occData$Species == studySpecies & occData$Independent == "Yes")
  seasonlist <- unique(occData$Season)
  n <- length(unique(occData$Site)) #Number of sites, just as in occMatrix
  primary <- length(seasonlist) #Number of seasons, just as in occMatrix
  secondary <- 1 #Number of surveys per seasons, just as in occMatrix
  
  #Year/Season match ----
  yearSea <- c()
  for (i in 1:primary) {
    tmp.dat <- subset(occData$Date, occData$Season == seasonlist[i] & occData$Independent == "Yes")
    yearSea[i] <- common(year(tmp.dat)) #Save most common year to yearSea
  }
  
  #Year matrix ----
  yearMat <- matrix(rep(yearSea, each = n), n, primary)
  
  return(data.frame(yearMat))
}

#Function to take an occInput data.frame (occData), a species of interest (studySpecies), and the name of a column in occData containing a covariate variable (covName) and output a covariate matrix of that variable
occCov.Season <- function(occData, studySpecies, covName) {
  #Data setup ----
  ind.dat <- subset(occData, occData$Species %in% studySpecies & occData$Independent == "Yes")
  seasonlist <- unique(occData$Season)
  n <- length(unique(occData$Site)) #Number of sites, just as in occMatrix
  primary <- length(seasonlist) #Number of seasons, just as in occMatrix
  secondary <- 1 #Number of surveys per seasons, just as in occMatrix
  
  #Output matrix ----
  seasonMat <- matrix(NA, n, primary)
  for (i in 1:primary) {
    tmp.dat <- subset(occData[[covName]], occData$Season == seasonlist[i] & occData$Independent == "Yes")
    tmp.vec <- mean(tmp.dat, na.rm = TRUE)
    if (!is.nan(tmp.vec)) seasonMat[,i] <- tmp.vec
  }
  return(seasonMat)
}

#Function to take an occInput data.frame (occData), a species of interst (studySpecies), and the name of a column in occData containing a covariate variable (covName), outputting a covariate vector of that variable for each site
occCov.Site <- function(occData, studySpecies, covName) {
  #Data setup ----
  ind.dat <- subset(occData, occData$Species == studySpecies & occData$Independent == "Yes")
  sitelist <- unique(ind.dat$Site)
  n <- length(sitelist) #Number of sites, just as in occMatrix
  
  #Output vector ----
  altVec <- c()
  for (i in 1:n) {
    tmp.dat <- subset(occData[[covName]], occData$Site == sitelist[i] & occData$Independent == "Yes")
    alt <- mean(tmp.dat, na.rm = TRUE)
    if (!is.nan(alt)) altVec[i] <- alt
    else altVec[i] <- NA
  }
  return(altVec)
}
