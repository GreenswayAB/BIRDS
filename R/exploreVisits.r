#' A function to explore the definition of field visits
#'
#' A function to explore the definition of field visits. Visits are a central concept
#' in the approach to species observation data used by the BIRDS package. In order to assess if your
#' definition of visit aligns with your grid size, you must explore the spatial extent of visits.
#' @param x an object of class \sQuote{OrganizedBirds} (organised BIRDS Spatial Dataframe).
#' See \code{\link{organizeBirds}}.
#' @param visitCol name of the column for the visits UID.
#' @param sppCol name of the column for species names.
#' @return a \code{data.frame} with summarized data per visit:
#' \itemize{
#'   \item \dQuote{day}
#'   \item \dQuote{month}
#'   \item \dQuote{year}
#'   \item \dQuote{nObs}: number of species observations
#'   \item \dQuote{SLL}: species list length (i.e. the number of observed species)
#'   \item \dQuote{effortDiam}: the diameter of the minumum circle that covers all points (i.e. locations for
#'   species observations), in meters. This is calculated as two times the maximum distance between
#'   the centroid of all observation points and any individual observation.
#'   \item \dQuote{medianDist}: the median (Q2) of the distances between the centroid and the observations, in meters.
#'   \item \dQuote{iqrDist}: the interquartile range of the distances between the centroid the observations, in meters.
#'   \item \dQuote{nOutliers}: the number of observations whose distance to the centroid is considered an outlier.
#'   Ouliers are defined as distances grater than the Q3 * 1.5 (i.e. \code{length(boxplot.stats(distances)$out)} as
#'   all distances are positive).
#'}
#' @examples
#'\donttest{
#' # create a visit-based data object from the original observation-based data
#' OB<-organizeBirds(bombusObs)
#' visitStats<-exploreVisits(OB)
#' # open an interactive data explorer
#' esquisse::esquisser(visitStats)
#' }
#'
#' # alternativelly, plot the variable you want, e.g.:
#' # to see the distribution of distances covered on each visit
#' # hist(visitStat$effortDiam)
#' # to see the distribution of species list lengths of each visit
#' # hist(visitStat$SLL)
#' # to identify suspicious visits reported the first day of each month or year
#' # hist(visitStat$day)
#' # to see correlations
#' # plot(visitStat$nObs, visitStat$effortDiam)
#' # plot(visitStat$SLL, visitStat$effortDiam)
#' # to see the ditributions of observations along the days of the month
#' # plot(visitStat$day, visitStat$nObs)
#' @export
#' @seealso \code{\link{createVisits}}, \code{\link{organiseBirds}}
exploreVisits<-function(x, visitCol=attr(x, "visitCol"), sppCol="scientificName"){
  if (class(x) == "OrganizedBirds") {
    spdf<- x$spdf
    dat <- spdf@data
  } else {
    stop("The object 'df' must be of class OrganizedBirds. See the function 'organizeBirds()'.")
  }
  uniqueUID <- unique(dat[, visitCol])
  uniqueUID <- sort(uniqueUID)
  nUID <- length(uniqueUID)

  tbl <- table(dat[, visitCol])
  # identical(names(tbl), as.character(uniqueUID))

  visitStat <- data.frame("visitUID" = uniqueUID,
                          "day" = NA,
                          "month" = NA,
                          "Month" = NA,
                          "year" = NA,
                          "date" = NA,
                          "centroidX" = NA,
                          "centroidY" = NA,
                          "nObs"= as.numeric(tbl), # number of records per visit, NOT species list length
                          "SLL" = NA, # species list length, i.e. number of observed species
                          "effortDiam" = NA, # the diameter of the minumum circle that covers all points, in meters
                          "medianDist" = NA, # the median (Q2) of the distances between the centroid and all points
                          "iqrDist" = NA, # the interquartile range of the distances between the centroid and all points
                          "nOutliers" = NA) # number of observations estimated to be outliers

  cat(paste("Analysing", nUID, "visits..."))

  for(i in 1:nUID){
    wVis <- which(dat[, visitCol] == uniqueUID[i])
    visitStat$SLL[i]  <- length(unique(as.character(dat[wVis, sppCol])))
    visitStat$day[i]  <- as.numeric(unique(dat[wVis, "day"]))
    visitStat$month[i] <- as.numeric(unique(dat[wVis, "month"]))
    visitStat$year[i] <- as.numeric(unique(dat[wVis, "year"]))

    coord <- sp::coordinates(spdf[wVis, ])
    coordPaste <- apply(coord, 1, paste0, collapse = ",")
    coordUnique <- matrix(coord[!duplicated(coordPaste)], ncol = 2)

    ctr <- rgeos::gCentroid(spdf[wVis,])
    visitStat$centroidX[i] <- ctr@coords[1]
    visitStat$centroidY[i] <- ctr@coords[2]

    if (nrow(coordUnique) > 1) {
      distances<-geosphere::distGeo(ctr, coordUnique)

      # That could be the diameter of the minumum circle that covers all points
      visitStat$effortDiam[i] <- round(max(distances) * 2, 0)
      visitStat$medianDist[i] <- round(median(distances), 2)
      visitStat$iqrDist[i]    <- round(IQR(distances), 2)
      visitStat$nOutliers[i]  <- length(boxplot.stats(distances)$out)
    } else {
      visitStat$effortDiam[i] <- 0
      visitStat$medianDist[i] <- 0
      visitStat$iqrDist[i]    <- 0
      visitStat$nOutliers[i]  <- 0
    }
  }
  visitStat$date <- as.Date(paste(visitStat$year, visitStat$month, visitStat$day, sep="-"), format = "%Y-%m-%d")
  visitStat$monthAb <- as.factor(months(visitStat$date))
  levels(visitStat$monthAb) <- month.name

  cat(paste("Finished analysing", nUID, "visits."))
  return(visitStat)
}
