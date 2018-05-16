#R version 3.4.4
#Data entry & processing functions
require("lubridate")

inFile <- "~/Documents/MooringData2018.csv"
study <- "Puma concolor"

#Function to take a file path to a csv (fileInput) containing Survey.Name, Date, Species, and Elevation columns, and output a more nicely formatted data.frame containing all that *and* Site, and Season columns
occInput <- function(fileInput, dateFormat = "%m/%d/%Y", Mooring = FALSE) {
  
  occ.dat <- read.csv(file = fileInput)
  occ.dat$Date <- base::as.Date(occ.dat$Date, format = dateFormat) #Format date objects nicely
  
  #Mooring edits ----
  #Data processing specific to our data sets
  if (Mooring == TRUE) {
    occ.dat <- occ.dat[, c(1,3,4,8,11,12,13,25)] #Remove extraneous columns
    occ.dat$Survey.Name <- gsub("\\s*El\\s*|\\s*La\\s*|\\s*National Park\\s*", "", occ.dat$Survey.Name) #Remove "El", "La", and "National Park" from survey names
    
    #Correct database errors
    occ.dat$Survey.Name <- gsub("Villa Mills", "Via Mills", occ.dat$Survey.Name)
    occ.dat$Survey.Name <- gsub("2011\\s*", "2011", occ.dat$Survey.Name)
  }
  
  #Site ----
  #Add site column to occ.dat, taking Survey.Name and ignoring any numbers (e.g: years) or seasons (e.g: "Spring")
  occ.dat["Site"] <-
    gsub(
      "\\s*\\d\\s*|\\s*Spring\\s*|\\s*Summer\\s*|\\s*Fall\\s*|\\s*Autumn\\s*|\\s*Winter\\s*",
      "",
      occ.dat$Survey.Name
    )
  
  #Season ----
  #Add season column to occ.dat, taking Survey.Name and ignoring the site
  sitelist <- unique(occ.dat$Site)
  seasonEx <- paste0("\\s*", sitelist[1], "\\s*")
  for (i in 2:length(sitelist)) { #Generate a regex to search for sitenames
    seasonEx <- paste0(seasonEx, "|\\s*", sitelist[i], "\\s*")
  }
  
  occ.dat["Season"] <- gsub(seasonEx, "", occ.dat$Survey.Name) #Exclude sitenames from Survey.Name to get seasons
  
  return(occ.dat)
}

#Function to take an occInput data.frame (occData) and output a detection matrix for the specified species of interest (studySpecies)
occMatrix <- function(occData, studySpecies) {
  #Data setup ----
  ind.dat <- subset(occData, occData$Independent == "Yes" & occData$Species == studySpecies)
  sitelist <- unique(ind.dat$Site)
  seasonlist <- unique(ind.dat$Season)
  n <- length(sitelist) #Number of sites (e.g: Savegre Valley + Bosque de Agua = 2 sites)
  primary <- length(seasonlist) #Number of seasons (e.g: Spring 2014 + Summer 2015 + Fall 2017 = 3 seasons)
  secondary <- 1 #Number of surveys in each season (How many times did we go out into the field each season? We'll call it one for now and just look at detection vs nondetection across that whole season, may go more granular later...)
  
  #Site/Season match ----
  #Make a matrix showing which sites/seasons actually belong together
  ssmat <- matrix(0, n, primary)
  for (j in 1:primary) {
    for (i in 1:n) {
      if (length(occData$Species[occData$Site == sitelist[i] & occData$Season == seasonlist[j]]) >= 1) {
        #If there are any detections at a particular site *and* season, record a 1 in the matrix
        ssmat[i,j] <- 1
      }
      #Otherwise record a 0
      else ssmat[i,j] <- 0
    }
  }
  
  #Detection matrix ----
  #Make the detection matrix
  y <- matrix(0, n, primary)
  for (j in 1:primary) {
    for (i in 1:n) {
      if (length(ind.dat$Species[ind.dat$Site == sitelist[i] & ind.dat$Season == seasonlist[j]]) >= 1) {
        #If there are any detections of studySpecies at a particular site and season, record a 1 in the matrix
        y[i,j] <- 1
      }
      #If those two sites/seasons don't actually go together, record an NA in the matrix
      else if (ssmat[i,j] == 0) y[i,j] <- NA
      #If site/season match and yet there were no detections, record a 0 in the matrix
      else y[i,j] <- 0
    }
  }
  
  return(y)
}
