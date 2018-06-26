#R version 3.4.4
#Occupancy analysis experimentation

#Setup ----
require("lubridate")
require("unmarked")
occ.dat <- read.csv("~/Documents/MooringData-June2018-Clean.csv")

occ.dat["YearMonth"] <- sapply(X = occ.dat$Date,
                               FUN = function(x) {paste(year(x), month(x))})

occ.dat["Region"] <- apply(X = occ.dat,
                           MARGIN = 1,
                           FUN = function(x) {
                              lon <- as.numeric(x["Longitude"])
                              lat <- as.numeric(x["Latitude"])
                              return(paste0(round(lon, digits = 2), ";", round(lat, digits = 2)))
                            })

print(unique(occ.dat$Region))
