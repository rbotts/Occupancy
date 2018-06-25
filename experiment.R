#R version 3.4.4
#Occupancy analysis experimentation

#Setup ----
require("lubridate")
require("unmarked")
occ.dat <- read.csv("~/Documents/MooringData-June2018-Clean.csv")

#Site and month/year ----
occ.dat["Site"] <- gsub("\\s*20\\d\\d\\s*|\\s*Spring\\s*|\\s*Summer\\s*|\\s*Fall\\s*|\\s*El\\s*|\\s*La\\s*|\\s*National Park\\s*|\\s*\\-\\s*",
                           "",
                           occ.dat$Survey.Name)
# dateFormat = "%m/%d/%Y"
# occ.dat$Date <- base::as.Date(occ.dat$Date, format = dateFormat)
occ.dat["MonthYear"] <- sapply(X = occ.dat$Date,
                               FUN = function(x) {paste(year(x), month(x))})

#Site/MonthYear Match ----
siteList <- sort(unique(occ.dat$Site))
myList <- sort(unique(occ.dat$MonthYear))
matchMat <- matrix(data = NA, nrow = length(siteList), ncol = length(myList))
for (i in 1:length(siteList)) {
  site <- siteList[i]
  for (j in 1:length(myList)) {
    my <- myList[j]

    if (length(subset(occ.dat$Time,
                      occ.dat$Site == site & occ.dat$MonthYear == my))) {
      matchMat[i,j] <- 1
    } else matchMat[i,j] <- 0
  }
}
rownames(matchMat) <- siteList
colnames(matchMat) <- myList
print(matchMat)
