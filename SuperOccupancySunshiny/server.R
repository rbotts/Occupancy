#SOS Server Script
require("shiny")
require("unmarked")
require("lubridate")
size.fileupload <- 64 #Max file size that can be uploaded is this many MB
options(shiny.maxRequestSize = size.fileupload*1024^2)

#Utility functions ----
occInput <- function(dfInput, dateFormat = "%m/%d/%Y", Mooring = FALSE, fileInput) {
  #Function to take either a file path to a csv (fileInput) or a data.frame object (dfInput) containing Survey.Name, Date, Species, and Elevation columns, and output a more nicely formatted data.frame containing all that *and* Site, and Season columns
  
  if (missing(dfInput)) dfInput <- read.csv(file = fileInput)
  dfInput$Date <- base::as.Date(dfInput$Date, format = dateFormat) #Format date objects nicely
  
  #Mooring edits ----
  #Data processing specific to our data sets
  if (Mooring == TRUE) {
    dfInput <- dfInput[, c(1,3,4,8,11,12,13,25)] #Remove extraneous columns
    dfInput$Survey.Name <- gsub("\\s*El\\s*|\\s*La\\s*|\\s*National Park\\s*", "", dfInput$Survey.Name) #Remove "El", "La", and "National Park" from survey names
    
    #Correct database errors
    dfInput$Survey.Name <- gsub("Villa Mills", "Via Mills", dfInput$Survey.Name)
    dfInput$Survey.Name <- gsub("2011\\s*", "2011", dfInput$Survey.Name)
  }
  
  #Site ----
  #Add site column to dfInput, taking Survey.Name and ignoring any numbers (e.g: years) or seasons (e.g: "Spring")
  dfInput["Site"] <-
    gsub(
      "\\s*\\d\\s*|\\s*Spring\\s*|\\s*Summer\\s*|\\s*Fall\\s*|\\s*Autumn\\s*|\\s*Winter\\s*",
      "",
      dfInput$Survey.Name
    )
  
  #Season ----
  #Add season column to dfInput, taking Survey.Name and ignoring the site
  sitelist <- unique(dfInput$Site)
  seasonEx <- paste0("\\s*", sitelist[1], "\\s*")
  for (i in 2:length(sitelist)) { #Generate a regex to search for sitenames
    seasonEx <- paste0(seasonEx, "|\\s*", sitelist[i], "\\s*")
  }
  
  dfInput["Season"] <- gsub(seasonEx, "", dfInput$Survey.Name) #Exclude sitenames from Survey.Name to get seasons
  
  return(dfInput)
}
occMatrix <- function(occData, studySpecies) {
  #Function to take an occInput data.frame (occData) and output a detection matrix for the specified species of interest (studySpecies)
  
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
common <- function(x) return(names(tail(sort(table(x)), n = 1))) #Utility function to find the most common item in a vector
occCov.Year <- function(occData, studySpecies) {
  #Function to take an occInput data.frame (occData) and species of interest (studySpecies) and output a covariate matrix of the year of each survey
  
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
occCov.Site <- function(occData, studySpecies, covName) {
  #Function to take an occInput data.frame (occData), a species of interst (studySpecies), and the name of a column in occData containing a covariate variable (covName), outputting a covariate vector of that variable for each site
  
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

#Server function ----
function(input, output) {
  observeEvent(eventExpr = input$dataButton, handlerExpr = {
    #Save uploaded csv as a data.frame
    raw.dat <- read.csv(input$updata$datapath, stringsAsFactors = FALSE)
    occ.dat <- occInput(dfInput = raw.dat, Mooring = input$mooringCheck)
    
    #2) Var/Cov UI ----
    occNames <- names(occ.dat)
    
    output$speciesSelect <<- renderUI(
      checkboxGroupInput(
        inputId = "speciesChosen",
        label = "Choose one species to analyze:",
        choices = sort(unique(occ.dat$Species))
      )
    )
    
    output$siteCovSelect <<- renderUI(
      checkboxGroupInput(inputId = "siteCovChosen",
                         label = "Choose covariates that vary by site:",
                         choices = occNames[occNames != "Survey.Name" &
                                            occNames != "Species" &
                                            occNames != "Date" &
                                            occNames != "Independent" &
                                            occNames != "Site"])
    )
    
    output$obsCovSelect <<- renderUI(
      checkboxGroupInput(
        inputId = "obsCovChosen",
        label = "Choose covariates that vary by observation:",
        choices = occNames[occNames != "Survey.Name" &
                           occNames != "Species" &
                           occNames != "Independent"]
        )
    )
    
    output$yearlyCovSelect <<- renderUI(
      checkboxGroupInput(
        inputId = "yearlyCovChosen",
        label = "Choose covariates that vary by year:",
        choices = c(occNames[occNames != "Survey.Name" &
                             occNames != "Species" &
                             occNames != "Date" &
                             occNames != "Independent"],
                    "Year"),
        selected = "Year"
      )
    )
    
    #
  })
}
