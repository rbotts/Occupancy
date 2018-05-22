#R version 3.4.4
#Plotting Occupancy Models

require("unmarked")
source("example.R")

nd <- data.frame(year = 2010:2014)
E.psi <- predict(m1, type = "psi", newdata = nd, appendData = TRUE)

with(E.psi, {
  plot(year, Predicted, ylim=c(-3,3), type="l",
       xlab="Year",
       ylab=expression(hat(psi)), cex.lab=0.8, cex.axis=0.8)
  lines(year, Predicted+1.96*SE, col=gray(0.7))
  lines(year, Predicted-1.96*SE, col=gray(0.7))
})