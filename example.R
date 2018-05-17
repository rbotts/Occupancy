#Basic Dynamic Occupancy
#R version 3.4.4
require("unmarked")

studySpecies <- "Puma concolor" #Which species will be studied?

#Data input, cleaning, organization ----
occ.dat <- read.csv(file = "~/Documents/OverlapAnalysis/Examples/sampledata.csv")

#Correcting database errors
occ.dat$Survey.Name <- gsub("Villa Mills", "Via Mills", occ.dat$Survey.Name)
if (exists("occ.dat$X") && exists("occ.dat$Y")) {
  occ.dat$X[occ.dat$X == 0] <- NA
  occ.dat$Y[occ.dat$Y == 0] <- NA
}

#Add a column to occ.dat that gives the site without the season, year, etc.
occ.dat["Site"] <-
  gsub(
    "\\s*\\d\\s*|\\s*Spring\\s*|\\s*Summer\\s*|\\s*Fall\\s*|\\s*National Park\\s*",
    "",
    occ.dat$Survey.Name
  )

#Generate a regex expression to search for any site names, disregarding whitespace on either side
sitelist <- unique(occ.dat$Site)
seasonEx <- paste0("\\s*", sitelist[1], "\\s*")
for (i in 2:length(sitelist)) {
  seasonEx <- paste0(seasonEx, "|\\s*", sitelist[i], "\\s*")
}

#Add a column to occ.dat that gives the season and year, but not the location (e.g: Fall 2014)
occ.dat["Season"] <-
  gsub(
    paste0(seasonEx, "|\\s*National Park\\s*"),
    "",
    occ.dat$Survey.Name
  )


#Preparing unmarkedMultFrame ----
ind.dat <- subset(occ.dat, occ.dat$Independent == "Yes" & occ.dat$Species == studySpecies)
seasonlist <- unique(ind.dat$Season)
sitelist <- unique(ind.dat$Site)

n <- length(sitelist) #Number of sites
primary <- length(seasonlist) #Number of seasons
secondary <- 1 #Number of surveys in each season?

#Make a matrix showing which sites/seasons actually belong together
ssmat <- matrix(0, n, primary)
for (j in 1:primary) {
  for (i in 1:n) {
    if (length(occ.dat$Species[occ.dat$Site == sitelist[i] & occ.dat$Season == seasonlist[j]]) >= 1) {
      #If there are any detections at a particular site *and* season, record a 1 in the matrix
      ssmat[i,j] <- 1
    }
    #Otherwise record a 0
    else ssmat[i,j] <- 0
  }
}

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

#Make a matrix giving the year of each observation
ymat <- matrix(rep(gsub("\\s*", "", seasonlist), each = n), n, primary)
ymat <- substr(ymat, nchar(ymat)-3, nchar(ymat))
ymat <- data.frame(ymat)

#Put it all together...
umf <- unmarkedMultFrame(
  y = y,
  siteCovs = data.frame(site=1:n),
  obsCovs = list(occasion=data.frame(matrix(rep(1:primary, each = n), n, primary))),
  yearlySiteCovs = list(year=ymat),
  numPrimary = primary
)


#Fitting basic occupancy models ----
#Constant parameters
m0 <- colext(
  psiformula = ~ 1,     #First-year Occupancy
  gammaformula = ~ 1,   #Colonization
  epsilonformula = ~ 1, #Extinction
  pformula = ~ 1,       #Detection probability
  data = umf
)

#Time-dependent parameters
m1 <- colext(
  psiformula = ~ 1,     #First-year Occupancy
  gammaformula = ~ year-1,   #Colonization
  epsilonformula = ~ year-1, #Extinction
  pformula = ~ year-1,       #Detection probability
  data = umf
)
