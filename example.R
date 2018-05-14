#Basic Dynamic Occupancy
require("unmarked")

occ.dat <- read.csv(file = "~/Documents/MooringData2018.csv")[,c(1,3,4,8,11,12,25)]
occ.dat <- subset(occ.dat, occ.dat$Independent == "Yes")
