#R version 3.4.4
#Covariate formatting functions
require("lubridate")

inFile <- "~/Documents/MooringData2018.csv"
study <- "Puma concolor"

#Utility function to find the most common item in a vector
common <- function(x) return(names(tail(sort(table(x)), n = 1)))

#Function to take an occInput data.frame (occData) and species of interest (studySpecies) and output a covariate matrix of the year of each survey
occCov.Year <- function(occData, studySpecies) {
  #Data setup ----
  ind.dat <- subset(occData, occData$Species == studySpecies & occData$Independent == "Yes")
  seasonlist <- unique(ind.dat$Season)
  n <- length(unique(ind.dat$Site)) #Number of sites, just as in occMatrix
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

#Function to take an occInput data.frame (occData) and species of interst (studySpecies) and output a covariate vector of the altitude/elevation of each site
occCov.Alt <- function(occData, studySpecies) {
  #Data setup ----
  ind.dat <- subset(occData, occData$Species == studySpecies & occData$Independent == "Yes")
  sitelist <- unique(ind.dat$Site)
  n <- length(sitelist) #Number of sites, just as in occMatrix
  
  #Altitude vector ----
  altVec <- c()
  for (i in 1:n) {
    tmp.dat <- subset(occData$Elevation, occData$Site == sitelist[i] & occData$Independent == "Yes")
    alt <- mean(tmp.dat, na.rm = TRUE)
    if (!is.nan(alt)) altVec[i] <- alt
    else altVec[i] <- NA
  }
  return(altVec)
}
