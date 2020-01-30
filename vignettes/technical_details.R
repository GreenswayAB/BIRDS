## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=FALSE--------------------------------------------------------------
#  ?organiseBirds()
#  OB <- organizeBirds(bombusObs, simplifySppName = TRUE)
#  OB <- organizeBirds(bombusObs, sppCol = "species", simplifySppName = FALSE,
#                       taxonRankCol = "taxonRank", taxonRank = c("SPECIES", "SUBSPECIES","VARIETY"))

## ---- eval=FALSE--------------------------------------------------------------
#  visitStats <- exploreVisits(OB)
#  # open an interactive data explorer
#  esquisse::esquisser(visitStats)
#  
#  # alternativelly, plot the variable you want, e.g.:
#  # to see the distribution of distances covered on each visit
#  # hist(visitStat$effortDiam)

## ---- eval=FALSE--------------------------------------------------------------
#  visits(OB)<-createVisits(x, columns = c("locality", "day", "month", "year", "recordedBy"))

## ---- eval=FALSE--------------------------------------------------------------
#  grid <- makeGrid(searchPolygon, gridSize = 10) # grid size in kilometers!
#  SB <- summariseBirds(OB, grid=grid)

## ---- eval=FALSE--------------------------------------------------------------
#  exportBirds(SB, dimension = "temporal", timeRes = "yearly", variable = "nObs", method = "sum")
#  # this is equivalent to
#  
#  colSums(SB$spatioTemporal[,,"Yearly","nObs"], na.rm = TRUE)
#  
#  
#  exportBirds(SB, dimension = "temporal", timeRes = "month", variable = "nVis", method = "sum")
#  # that is wquivalent to
#  apply(SB$spatioTemporal[,,1:12,"nVis"], 3,  sum, na.rm = TRUE)
#  
#  exportBirds(SB, dimension = "temporal", timeRes = "monthly", variable = "nVis", method = "sum")
#  # that is somehow equivalent to the xts method except the later excludes months without data
#  xts::apply.monthly(SB$temporal[,"nVis"], sum)
#  
#  exportBirds(SB, dimension = "spatial", timeRes = "NULL", variable = "nYears", method = "sum")@data

## ---- eval=FALSE--------------------------------------------------------------
#  focalSpSummary(SB, "Bombus humilis")

## ---- eval=FALSE--------------------------------------------------------------
#  focalSpReport(SB, "Bombus humilis")

## ---- eval=FALSE--------------------------------------------------------------
#  par(mfrow=c(1,2), mar=c(1,1,1,1))
#  palBW <- leaflet::colorNumeric(c("white", "navyblue"),
#                                 c(0, max(SB$spatial@data$nVis, na.rm = TRUE)),
#                                 na.color = "transparent")
#  seqNVis<-round(seq(0, max(SB$spatial@data$nVis, na.rm = TRUE), length.out = 5))
#  plot(SB$spatial, col=palBW(SB$spatial@data$nVis), border = NA)
#  plot(gotaland, col=NA, border = "grey", lwd=1, add=TRUE)
#  legend("bottomleft", legend=seqNVis, col = palBW(seqNVis),
#        title = "Number of \nobservations", pch = 15, bty="n")
#  
#  ign<-exposeIgnorance(SB$spatial@data$nVis, h = 5)
#  palBWR <- leaflet::colorNumeric(c("navyblue", "white","red"), c(0, 1),
#                                  na.color = "transparent")
#  plot(gotaland, col="grey90", border = "grey90", lwd=1)
#  plot(SB$spatial, col=palBWR(ign), border = NA, add=TRUE)
#  plot(gotaland, col=NA, border = "grey", lwd=1, add=TRUE)
#  legend("bottomleft", legend=c(seq(0, 1, length.out = 5), "NA"),
#        col = c(palBWR(seq(0, 1, length.out = 5)), "grey90"),
#        title = "Ignorance \nnVis, \nO0.5=5", pch = 15, bty="n")

## ---- eval=FALSE--------------------------------------------------------------
#  ## Community analysis -->
#  CM <- communityMatrix(SB, sampleUnit="visit")
#  sp1 <- vegan::specaccum(CM, method = "exact")

