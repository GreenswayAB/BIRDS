## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----checkProj2, echo=FALSE---------------------------------------------------
projVer <- sf::sf_extSoftVersion()["PROJ"]
doNextChunk <- as.logical(compareVersion("5.1.0", projVer) == -1)

## ----SB, eval = doNextChunk, echo=TRUE, message=FALSE, warning=FALSE----------
#  library(BIRDS)
#  library(xts)
#  # Create a grid for your sample area that will be used to summarise the data:
#  grid <- makeGrid(gotaland, gridSize = 10)
#  # The grid can be easily created in different ways.
#  # Import the species observation data:
#  PBD <- bombusObs
#  # alternatively, you could load a previously downloaded .CSV file
#  # PBD <- read.csv(file="path/to/your/file.csv)
#  
#  # Convert the data from an observation-based to a visit-based format,
#  # adding a unique identifier for each visit:
#  OB <- organizeBirds(PBD, sppCol = "scientificName")
#  
#  # Summarise the data:
#  SB <- summariseBirds(OB, grid=grid)

## ----head, eval = doNextChunk-------------------------------------------------
#  sb.xts <- SB$temporal
#  head(sb.xts)
#  dim(sb.xts)

## ----subsetting, eval = doNextChunk, echo=TRUE, message=FALSE, warning=FALSE----
#  sb.xts["2017-09"] #a specific month
#  sb.xts["2017-09-07"] #a specific day
#  sb.xts["2017-01-01/2017-05-01"] #for a period

## ----monthly and plot, eval = doNextChunk, echo=TRUE, fig.height=5, fig.width=9, message=FALSE, warning=FALSE----
#  obs.m <- to.monthly(sb.xts$nObs)
#  obs.m["2017-04"]
#  sb.xts["2017-04"]
#  
#  plot(obs.m["2010/2017",2], col = "darkblue", grid.ticks.on = "month",
#       major.ticks = "month", grid.col = "lightgrey",
#       main = "Maximum number of daily observations per month")

## ----completting, eval = doNextChunk, echo=TRUE, message=FALSE, warning=FALSE----
#  # sb.xts.na is same as sb.xts but with all missing days added (with NAs)
#  rng <- range(time(sb.xts))
#  sb.xts.na <- merge(sb.xts, xts(, seq(rng[1], rng[2], by = "day")))

## ----completting2, eval = doNextChunk, echo=TRUE, fig.height=5, fig.width=9, message=FALSE, warning=FALSE----
#  z.seq <- seq(rng[1], rng[2], by = "month")
#  YM.sb.xts<-unique(as.Date(as.yearmon(index(sb.xts))))
#  YM.z.seq<-as.Date(as.yearmon(z.seq))
#  wTrm <- which(YM.z.seq %in% YM.sb.xts)
#  z.seq <- z.seq[-wTrm] ## remove the days for which
#  
#  z <- xts(, z.seq)
#  
#  sb.xts.0 <- merge(sb.xts, z, fill = 0)
#  
#  obs.m.0 <- to.monthly(sb.xts.0$nObs)
#  plot(obs.m.0["2010/2017",2], col = "darkblue", grid.ticks.on = "month",
#       major.ticks = "month", grid.col = "lightgrey",
#       main = "Maximum number of daily observations per month")

