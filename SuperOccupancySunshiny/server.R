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
  ind.dat <- subset(occData, occData$Independent == "Yes" & occData$Species %in% studySpecies)
  sitelist <- unique(occData$Site)
  seasonlist <- unique(occData$Season)
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
occCov.Season <- function(occData, studySpecies, covName) {
  #Function to take an occInput data.frame (occData), a species of interest (studySpecies), and the name of a column in occData containing a covariate variable (covName) and output a covariate matrix of that variable
  
  #Data setup ----
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
occCov.Year <- function(occData, studySpecies) {
  #Function to take an occInput data.frame (occData) and species of interest (studySpecies) and output a covariate matrix of the year of each survey
  
  #Data setup ----
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
occCov.Site <- function(occData, studySpecies, covName) {
  #Function to take an occInput data.frame (occData), a species of interst (studySpecies), and the name of a column in occData containing a covariate variable (covName), outputting a covariate vector of that variable for each site
  
  #Data setup ----
  sitelist <- unique(occData$Site)
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
plotBar <- function(percentage, xlab, barColor = "darkred") {
  #Plots a loading bar that is percentage% complete and the color of barColor, with a message specified by xlab
  if (length(percentage) > 1 || !is.numeric(percentage)) warning("percentage must be a numeric vector of length 1!")
  else {
    marOrig <- par("mar")
    finOrig <- par("fin")
    par(mar=c(1,0,0,0), fin=c(4, 0.5))
    barplot(percentage, col = barColor, xlim = c(0,100), horiz = TRUE, axes = FALSE)
    mtext(xlab, side = 1)
    par(mar=marOrig, fin = finOrig)
  }
}

#Server function ----
function(input, output) {
  observeEvent(eventExpr = input$dataButton, handlerExpr = {
    #Save uploaded csv as a data.frame
    raw.dat <- read.csv(input$updata$datapath, stringsAsFactors = FALSE)
    occ.dat <- occInput(dfInput = raw.dat, Mooring = input$mooringCheck)
    
    #2) Var/Cov UI ----
    occNames <- names(occ.dat)
    
    output$speciesSelect <- renderUI(
      checkboxGroupInput(
        inputId = "speciesChosen",
        label = "Choose one species to analyze:",
        choices = sort(unique(occ.dat$Species))
      )
    )
    
    output$siteCovSelect <- renderUI(
      checkboxGroupInput(inputId = "siteCovChosen",
                         label = "Choose covariates that vary by site:",
                         choices = occNames[occNames != "Survey.Name" &
                                              occNames != "Species" &
                                              occNames != "Date" &
                                              occNames != "Independent" &
                                              occNames != "Site"])
    )
    
    output$obsCovSelect <- renderUI(
      checkboxGroupInput(
        inputId = "obsCovChosen",
        label = "Choose covariates that vary by observation:",
        choices = occNames[occNames != "Survey.Name" &
                             occNames != "Species" &
                             occNames != "Independent"]
      )
    )
    
    output$seasonCovSelect <- renderUI(
      checkboxGroupInput(
        inputId = "seasonCovChosen",
        label = "Choose covariates that vary by season:",
        choices = c("Year",
                    occNames[occNames != "Survey.Name" &
                               occNames != "Species" &
                               occNames != "Date" &
                               occNames != "Independent"]),
        selected = "Year"
      )
    )
    
    output$runModel <- renderUI(
      wellPanel(
        fluidRow(column(12, align = "center",
                        div("Once you've selected all the variables and covariates you like, press the following button to run the Occupancy analysis. Note that this may take a few minutes to complete, especially if using a large dataset.", style = "text-align:left"),
                        actionButton(
                          inputId = "goButton",
                          label = "Go!",
                          width = "50%"
                        )
                 )
        )
      )
    )
    
    #3) Model Select ----
        observeEvent(eventExpr = input$goButton, handlerExpr = {
      
      #SiteCov ----
      siteCovList <- data.frame(site = 1:length(unique(occ.dat$Site)))
      if (!is.null(length(input$siteCovChosen))) {
        for (i in 1:length(input$siteCovChosen)) {
          siteCovList <- cbind( #Add new column to siteCovList for siteCovChosen i
            siteCovList,
            occCov.Site(
              occData = occ.dat,
              studySpecies = input$speciesChosen,
              covName = input$siteCovChosen[i]
            )
          )
          names(siteCovList) <- c("site", input$siteCovChosen[1:i]) #Name columns appropriately
        }
      }
      
      #ObsCov ----
      #To add later...
      
      #SeasonCov ----
      seasonCovList <- list()
      
      if(!is.null(length(input$seasonCovChosen))) {
        for (i in 1:length(input$seasonCovChosen)) {
          if (input$seasonCovChosen[i] == "Year") {
            seasonCovList$Year <- occCov.Year(occData = occ.dat, studySpecies = input$speciesChosen)
          }
          else {
            seasonCovList[[input$seasonCovChosen[i]]] <- occCov.Season(occData = occ.dat, studySpecies = input$speciesChosen, covName = input$seasonCovChosen[i])
          }
        }
      }
      
      #Modeling ----
      umf <- unmarkedMultFrame( #Model the data with the covariates selected in tab 2
        y = occMatrix(occData = occ.dat, studySpecies = input$speciesChosen),
        siteCovs = siteCovList,
        numPrimary = length(unique(occ.dat$Season)),
        yearlySiteCovs = seasonCovList
        #, obsCovs = #a list of covariates that vary by both survey and season
      )
      
      
      m0 <<- colext(
        psiformula = ~ 1,
        gammaformula = ~Year + Elevation,
        epsilonformula = ~Year + Elevation,
        pformula = ~Year,
        data = umf
      )
      
      out <- capture.output({
        print(summary(umf))
        print(summary(m0))
      }) #Capture the model summary as a character vector
      
      #Model text out ----
      output$modelParameters <- renderUI( #Output the model summary as a nice HTML box
        HTML(
          paste(
            c("<pre>", #Wrap everything with <pre> tags to preserve whitespace formatting
              "<h4>", input$speciesChosen, "</h4>", #Header for the box: species being studied
              gsub("<NA>", " NA ", out[3:length(out)], fixed = TRUE), #items between <> confuse HTML, so get rid of them
              "</pre>"),
            collapse = "<br>" #throw a <br> between every line to preserve newline formatting
          )
        )
      )
      
      #Model plot out ----
      siteC <- names(siteCovList)
      
      #Psi prediction
      nd.psi <- data.frame(Elevation = 1:3500)
      E.psi <<- predict(object = m0, type = "psi", newdata = nd.psi, appendData = TRUE)
      
      #E.col <- predict(object = m0, type = "col", newdata = nd, appendData= TRUE)
      #E.ext <- predict(object = m0, type = "ext", newdata = nd, appendData= TRUE)
      
      #Det prediction
      yearUnique <- unique(year(occ.dat$Date))
      nd.det <- data.frame(
        Year = factor(as.character(yearUnique[1]), levels = yearUnique)
      )
      E.det <<- predict(object = m0, type = "det", newdata = nd.det, appendData = TRUE)
      
      output$occPlots <- renderPlot({
        
        par(mfcol = c(2,1))
        
        #Psi plot
        plot(
          x = E.psi$Elevation,
          y = E.psi$Predicted,
          ylim = c(-3,3),
          type = "l",
          ylab = expression(hat(psi)),
          main = paste(input$speciesChosen, sep = ", "),
          cex.lab = 0.8,
          cex.axis = 0.8
        )
        lines(E.psi$Elevation, E.psi$Predicted+1.96*E.psi$SE, col=gray(0.7))
        lines(E.psi$Elevation, E.psi$Predicted-1.96*E.psi$SE, col=gray(0.7))
        
        #Col plot
        # plot(
        #   x = E.col$Elevation,
        #   y = E.col$Predicted,
        #   ylim = c(-3,3),
        #   type = "l",
        #   ylab = expression(gamma),
        #   cex.lab = 0.8,
        #   cex.axis = 0.8
        # )
        # lines(E.col$Elevation, E.col$Predicted+1.96*E.psi$SE, col=gray(0.7))
        # lines(E.col$Elevation, E.col$Predicted-1.96*E.psi$SE, col=gray(0.7))
        # 
        # #Ext plot
        # plot(
        #   x = E.ext$Elevation,
        #   y = E.ext$Predicted,
        #   ylim = c(-3,3),
        #   type = "l",
        #   ylab = expression(epsilon),
        #   cex.lab = 0.8,
        #   cex.axis = 0.8
        # )
        # lines(E.ext$Elevation, E.ext$Predicted+1.96*E.psi$SE, col=gray(0.7))
        # lines(E.ext$Elevation, E.ext$Predicted-1.96*E.psi$SE, col=gray(0.7))
        # 
        #Det plot
        plot(
          x = E.det$Year,
          y = E.det$Predicted,
          ylim = c(-3,3),
          type = "l",
          ylab = expression(italic(p)),
          cex.lab = 0.8,
          cex.axis = 0.8
        )
        lines(E.det$Year, E.det$Predicted+1.96*E.psi$SE, col=gray(0.7))
        lines(E.det$Year, E.det$Predicted-1.96*E.psi$SE, col=gray(0.7))
        
        par(mfcol = c(1,1))
      })
    })
  })
}
