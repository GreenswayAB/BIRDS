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
#' # esquisse::esquisser(visitStats)
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
    stop("The object 'x' must be of class OrganizedBirds. See the function 'organizedBirds()'.")
  }
  uniqueUID <- unique(dat[, visitCol])
  uniqueUID <- sort(uniqueUID)
  nUID <- length(uniqueUID)

  # tbl <- table(dat[, visitCol])
  # identical(names(tbl), as.character(uniqueUID))

  visitStat <- data.frame("visitUID" = uniqueUID,
                          "day" = NA,
                          "month" = NA,
                          "Month" = NA,
                          "year" = NA,
                          "date" = NA,
                          "centroidX" = NA,
                          "centroidY" = NA,
                          "nObs"= NA, # number of records per visit, NOT species list length  #as.numeric(tbl),
                          "SLL" = NA, # species list length, i.e. number of observed species
                          "effortDiam" = NA, # the diameter of the minumum circle that covers all points, in meters
                          "medianDist" = NA, # the median (Q2) of the distances between the centroid and all points
                          "iqrDist" = NA, # the interquartile range of the distances between the centroid and all points
                          "nOutliers" = NA) # number of observations estimated to be outliers

  cat(paste("Analysing", nUID, "visits..."))
  datGBY <- group_by(dat, !!! rlang::syms(visitCol))

  visitStat$nObs <- summarise(datGBY, nObs= n())$nObs
  visitStat$SLL  <- summarise(datGBY, SLL = n_distinct(scientificName)  )$SLL
  visitStat$day  <- summarise(datGBY, day = as.character(unique(day)))$day
  visitStat$month<- summarise(datGBY, mon = as.character(unique(month)))$mon
  visitStat$year <- summarise(datGBY, yea = as.character(unique(year)))$yea
  rm(datGBY)

  ### TODO? can this lapply be done with dplyr?
  ctr <- lapply(uniqueUID, FUN = function(x){
    wVis <- which(dat[, visitCol] == x)

    coord <- sp::coordinates(spdf[wVis, ])
    coordPaste <- apply(coord, 1, paste0, collapse = ",")
    coordUnique <- matrix(coord[!duplicated(coordPaste)], ncol = 2)

    ctr <- rgeos::gCentroid(spdf[wVis,])
    centroidX <- ctr@coords[1]
    centroidY <- ctr@coords[2]

    if (nrow(coordUnique) > 1) {
      distances<-geosphere::distGeo(ctr, coord)

      # That could be the diameter of the minumum circle that covers all points
      effortDiam <- round(max(distances) * 2, 0)
      medianDist <- round(median(distances), 0)
      iqrDist    <- round(IQR(distances), 0)
      nOutliers  <- length(boxplot.stats(distances)$out)
    } else {
      effortDiam <- 1
      medianDist <- 1
      iqrDist    <- 1
      nOutliers  <- 0
    }
    return(list(centroidX, centroidY, effortDiam, medianDist, iqrDist,nOutliers))
  } )

  tmp<-matrix(unlist(ctr), ncol = 6, byrow = TRUE)

  visitStat$centroidX <- tmp[,1]
  visitStat$centroidY <- tmp[,2]
  visitStat$effortDiam <- tmp[,3]
  visitStat$medianDist <- tmp[,4]
  visitStat$iqrDist    <- tmp[,5]
  visitStat$nOutliers  <- tmp[,6]

  visitStat$date <- as.Date(paste(visitStat$year, visitStat$month, visitStat$day, sep="-"), format = "%Y-%m-%d")
  visitStat$Month <- as.factor(months(visitStat$date))
  levels(visitStat$Month) <- month.name

  cat(paste("Finished analysing", nUID, "visits.\n"))
  return(visitStat)
}


#' A function to make the exploreVisits Spatial
#'
#' A function to
#' @param x an object of class \sQuote{data.frame} from exploreVistis.
#' @param xyCols a character vector with the column names for the coordinates.
#' Default to \code{c("centroidX","centroidY")}
#' @param dataCRS a character string with the proj4 description of the original
#' coordinate projeciton system (CRS). Default to \code{"+init=epsg:4326"}
#' @param radius either a character string with the name of the column
#' containing the radius of the visit circle, or a numeric vector with its value
#' in meters. Default to \code{"medianDist"}
#'
#' @return a list with a \code{SpatialPointsDataFrame} (the centroids) and a
#' \code{ "SpatialPolygonsDataFrame"} (the effort circles). Note that when plotted
#' directly effort circles may not look like circles in the returned
#' (Pseudo-Mercator) projection.
#'
#' @examples
#' # create a visit-based data object from the original observation-based data
#' OB<-organizeBirds(bombusObs)
#' visitStats<-exploreVisits(OB)
#' spV<-spatialVisits(visitStats)
#' plot(spV$effort)
#' @export
#' @seealso \code{\link{exploreVisits}}, \code{\link{organiseBirds}}
spatialVisits <- function(x,
                          xyCols=c("centroidX","centroidY"),
                          dataCRS="+init=epsg:4326",
                          radius="medianDist"){
  if (class(x) == "data.frame") {
    sp::coordinates(x) <- xyCols
    sp::proj4string(x) <- dataCRS ## because I know where it comes from
  } else {
    stop("The object 'x' must be of class data.frame (after exploreVisits). See the function 'exploreVisits()'.")
  }

  if(radius=="" | is.na(radius) | is.null(radius)){
    radiusVal<- rep(1, nrow(x@data))
  } else if(radius %in% colnames(x@data)){
    radiusVal<-x@data[,radius]
    ## convert meters to degrees?
  } else if(is.numeric(radius) & length(radius)==nrow(x@data)){
    radiusVal<-radius
  } else {
    stop("The parameter 'radius' needs to be one od the column names or a numeric
         vector of length equal to the number of visits")
  }

  # xTrans <- sp::spTransform(x, CRSobj = "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") ##https://epsg.io/54012
  xTrans <- sp::spTransform(x, CRSobj = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") ##https://epsg.io/54009  #
  # xTrans <- sp::spTransform(x, CRSobj = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs") ##https://epsg.io/3857
  buff <- rgeos::gBuffer(xTrans,  byid=TRUE, id=x@data$visitUID, width=radiusVal)
  buff <- sp::spTransform(buff, CRSobj = dataCRS)

  return(list("points"=x, "effort"=buff))

}

getUTMzone <- function(points){

  ##Find which UTM-zones that have the most points
  utmZone <- sp::over(points, utmZones)
  # Error in .local(x, y, returnList, fn, ...) :
  # identicalCRS(x, y) is not TRUE
  # organiseBirds() returns "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  # and it is interpreted as not identical of "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

  # sp::proj4string(utmZones) <- proj4string(points) ## because I know where it comes from

  freqZones <- table(utmZone$ZONE)

  # maxZones <- names(freqZones)[which(freqZones == max(freqZones))]
  maxZones <- names(which.max(freqZones))

  alternatives <- rep(FALSE, length(maxZones))

  names(alternatives) <- maxZones

  ##Goes though the zones with most points and checks if other points are to far away.

  for(a in names(alternatives)){

    nZonesOver <- length(which(as.integer(names(freqZones)) > as.integer(a)))
    nZonesUnder <- length(which(as.integer(names(freqZones)) < as.integer(a)))

    if(nZonesOver <= 1 && nZonesUnder <= 1){
      alternatives[a]<-TRUE
    }

  }

  ##Results
  res <- list("zone" = NULL, "msg" = NULL)

  if(sum(alternatives) == 0){
    ##No acceptable zones found

    res$msg <- "There is no UTM zone where all points overlap with or with its adjacent zones"

    return(res)

  }else if (sum(alternatives) == 1){
    ## One zone found with points only in adjecent zone found.

    res$zone <- as.integer(names(alternatives)[alternatives])
    res$msg <- "Success!"

    return(res)

  }else{
    ## Two adjacent zones with same number of points were found.
## OR MORE than two?

    meanPoint <- sp::SpatialPoints(geosphere::geomean(points), proj4string = points@proj4string)

    utmMeanZone <- sp::over(meanPoint, utmZones)

    res$zone <- as.integer(utmMeanZone$ZONE)
    res$msg <- "The points are split over two UTM-zones. The zone with the centroid for all the points was chosen."

    return(res)

  }

}
