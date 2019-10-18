## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----install package, eval = F-------------------------------------------
#  install.packages('remotes')
#  remotes::install_github('Greensway/BIRDS')

## ----basic example, eval = TRUE------------------------------------------
library(BIRDS)
# Create a grid for your sample area that will be used to summarise the data:
grid <- makeGrid(gotaland, gridSize = 10)
# The grid can be easily created in different ways. 

# Import the species observation data:
PBD<-bombusObs
# alternatively, you could load a previously downloaded .CSV file 
# PBD <- read.csv(file="path/to/your/file.csv)

# Convert the data from an observation-based to a visit-based format, adding a unique identifier for each visit:
OB <- organizeBirds(PBD, sppCol = "scientificName", simplifySppName = TRUE)

# Summarise the data:
SB <- summariseBirds(OB, grid=grid)

# Look at some summarised variables:
# Number of observations
EBnObs <- exportBirds(SB, dimension = "temporal", timeRes = "yearly", 
                      variable = "nObs", method = "sum")
# Number of visits
EBnVis <- exportBirds(SB, dimension = "temporal", timeRes = "yearly", 
                      variable = "nVis", method = "sum")
# Average species list length (SLL) per year (a double-average, i.e. the mean over cell values
# for the median SLL from all visits per year and cell) 
EBavgSll <- colMeans(SB$spatioTemporal[,,"Yearly","avgSll"], na.rm = TRUE)
# The ratio of number of observations over number of visits
relObs<-EBnObs/EBnVis

## ----figure 1, fig.show='hold', fig.width= 7, fig.height= 5, fig.cap = "Time series for *Bombus* spp. dataset."----
par(mar=c(4,4,1,6), las=1)
plot(time(EBnObs), EBnObs, type = "l", lwd = 3, xlab = "Year", ylab = "Number", 
     ylim=c(0, max(EBnObs)), xaxp=c(2000, 2018, 18))
lines(time(EBnObs), EBnVis, lwd=3, lty=2)
lines(time(EBnObs), relObs * max(EBnObs) / max(relObs), lwd=3, lty=1, col="#78D2EB")
lines(time(EBnObs), EBavgSll * max(EBnObs) / max(EBavgSll), lwd=3, lty=1, col="#FFB3B5")
axis(4, at = seq(0, max(EBnObs), length.out = 5), 
     labels = round(seq(0,max(relObs), length.out = 5), 1), 
     lwd = 2, col = "#78D2EB", col.ticks = "#78D2EB")
axis(4, at = seq(0, max(EBnObs), length.out = 5) , 
     labels = round(seq(0,max(EBavgSll), length.out = 5), 1), 
     lwd = 2, col = "#FFB3B5", col.ticks = "#FFB3B5", line = 3)
legend("topleft", legend=c("n.observations","n.visits"), 
       lty = c(1,2), lwd = 3, bty = "n")
legend("bottomright", legend=c("n.observations / n.visits", "avg. SLL per cell"),
       lty = 1, lwd = 3, col = c("#78D2EB", "#FFB3B5"), bty = "n")

## ----figure 2, fig.show='hold', fig.width= 7, fig.height= 3--------------
library(sp)
wNonEmpty<-unname( which( unlist(lapply(SB$overlaid, nrow)) != 0) )
EB <- exportBirds(SB, "Spatial", "Month", "nYears", "sum")
# because the dimension is "spatial", the result is a 'SpatialPolygonDataFrame'

palBW <- leaflet::colorNumeric(c("white", "navyblue"), 
                               c(0, max(EB@data, na.rm = TRUE)), 
                               na.color = "transparent")
par(mfrow=c(1,3), mar=c(1,1,1,1))
plot(SB$spatial[wNonEmpty,], col="grey", border = NA)
plot(gotaland, col=NA, border = "grey", lwd=1, add=TRUE)
mtext("Visited cells", 3, line=-1)

plot(EB, col=palBW(EB@data$Jul), border = NA)
plot(gotaland, col=NA, border = "grey", lwd=1, add=TRUE)
mtext("Number of years for which \nJuly was sampled", 3, line=-2)

plot(EB, col=palBW(EB@data$Dec), border = NA)
plot(gotaland, col=NA, border = "grey", lwd=1, add=TRUE)
mtext("Number of years for which \nDecember was sampled", 3, line=-2)
legend("bottomleft", legend=seq(0, max(EB@data, na.rm = TRUE), length.out = 5),
      col = palBW(seq(0, max(EB@data, na.rm = TRUE), length.out = 5)),
      title = "Number of years", pch = 15, bty="n")

## ----figure 3, fig.show='hold', fig.width= 7, fig.height= 4--------------
par(mfrow=c(1,2), mar=c(1,1,1,1))
palBW <- leaflet::colorNumeric(c("white", "navyblue"), 
                               c(0, max(SB$spatial@data$nVis, na.rm = TRUE)), 
                               na.color = "transparent")
seqNVis<-round(seq(0, max(SB$spatial@data$nVis, na.rm = TRUE), length.out = 5))
plot(SB$spatial, col=palBW(SB$spatial@data$nVis), border = NA)
plot(gotaland, col=NA, border = "grey", lwd=1, add=TRUE)
legend("bottomleft", legend=seqNVis, col = palBW(seqNVis),
      title = "Number of \nobservations", pch = 15, bty="n")

ign<-exposeIgnorance(SB$spatial@data$nVis, h = 5)
palBWR <- leaflet::colorNumeric(c("navyblue", "white","red"), c(0, 1), 
                                na.color = "transparent")
plot(gotaland, col="grey90", border = "grey90", lwd=1)
plot(SB$spatial, col=palBWR(ign), border = NA, add=TRUE)
plot(gotaland, col=NA, border = "grey", lwd=1, add=TRUE)
legend("bottomleft", legend=c(seq(0, 1, length.out = 5), "NA"),
      col = c(palBWR(seq(0, 1, length.out = 5)), "grey90"),
      title = "Ignorance \nnVis, \nO0.5=5", pch = 15, bty="n")

