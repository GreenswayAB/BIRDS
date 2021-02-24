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
#'   \item \dQuote{effortDiam}: the 2 times the maximum of the distances between
#'   the centroid of all observation points and any individual observation.
#'   \item \dQuote{medianDist}: the median (Q2) of the distances between the
#'   centroid and the observations, in meters.
#'   \item \dQuote{iqrDist}: the interquartile range of the distances between the
#'    centroid the observations, in meters.
#'   \item \dQuote{nUniqueLoc}: the number of unique combination of coordinates (locations).
#'   \item \dQuote{nClusters}: the number of clusters defined by the DBSCAN algorithm,
#'   a minimum of 3 observations per cluster within the median distance between
#'   all observations. If the number of clusters is = 0 means that there are at
#'   least 3 unique locations but observations are too spread and no cluster was
#'   found. If the number of unique locations is less than 3, observations are
#'   considered as a single cluster without outliers.
#'   \item \dQuote{nOutliers}: the number of observations whose distance to any
#'   cluster is beyond the median distance between all observations.
#'}
#' @examples
#' if(interactive()){
#' # create a visit-based data object from the original observation-based data
#' OB <- organizeBirds(bombusObs)
#' visitStats <- exploreVisits(OB)
#' # esquisse::esquisser(visitStats)
#' # alternatively, plot the variable you want, e.g.:
#' # to see the distribution of distances covered on each visit
#' hist(visitStats$effortDiam)
#' # to see the distribution of species list lengths of each visit
#' hist(visitStats$SLL)
#' # to identify suspicious visits reported the first day of each month or year
#' hist(visitStats$day)
#' # to see correlations
#' plot(visitStats$nObs, visitStats$effortDiam)
#' plot(visitStats$SLL, visitStats$effortDiam)
#' # to see the ditributions of observations along the days of the month
#' plot(visitStats$day, visitStats$nObs)
#' }
#' @export
#' @importFrom rlang .data
#' @importFrom dplyr group_by summarise n n_distinct sym
#' @importFrom rgeos gCentroid
#' @importFrom dbscan dbscan
#' @importFrom lubridate date day month year ymd
#' @importFrom geosphere distGeo distm
#' @seealso \code{\link{createVisits}}, \code{\link{organiseBirds}}
exploreVisits <- function(x,
                        visitCol=NULL, #visitCol=attr(x, "visitCol"),
                        sppCol="scientificName"){
  minPts <- 3 ## Minumin number of points required for clustering

  if (class(x) == "OrganizedBirds") {
    spdf<- x$spdf
    dat <- slot(spdf, "data")
  } else {
    stop("The object 'x' must be of class OrganizedBirds. See the function 'organizedBirds()'.")
  }
  if (is.null(visitCol)){
    visitCol<-attr(x, "visitCol")
  }
  if (!(visitCol %in% colnames(dat))) stop(paste("There is no column called",
                                                 visitCol, "in your organised dataset."))

  uniqueUID <- unique(dat[, visitCol])
  uniqueUID <- sort(uniqueUID)
  nUID <- length(uniqueUID)

  # dat$date <- date(paste(dat$year, dat$month, dat$day, sep = "-"))
  dat$date <- ymd(paste(dat$year, dat$month, dat$day, sep = "-"))

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
                          "nUniqueLoc" = NA, # number of unique locations
                          "nClusters" = NA, # Number of clusters
                          "nOutliers" = NA) # number of observations estimated to be outliers from the clusters

  message(paste("Analysing", nUID, "visits..."))
  # datGBY <- group_by(dat, !!! rlang::syms(visitCol))
  datGBY <- group_by(dat, !! sym(visitCol))

  visitStat$nObs <- summarise(datGBY, nObs= n())$nObs
  visitStat$SLL  <- summarise(datGBY, SLL = n_distinct(.data$scientificName)  )$SLL
  dates <- summarise(datGBY, date = min(date)) ##If the visits are over multiple days, we take the first.
  visitStat$day  <- day(dates$date)
  visitStat$month<- month(dates$date)
  visitStat$year <- year(dates$date)
  rm(datGBY)

  ### TODO? can this lapply be done with dplyr?
  ctrList <- lapply(uniqueUID, FUN = function(x){
    wVis <- which(dat[, visitCol] == x)
    spdfTmp <- spdf[wVis, ]

    coord <- sp::coordinates(spdfTmp)
    coordPaste <- apply(coord, 1, paste0, collapse = ",")
    coordUnique <- matrix(coord[!duplicated(coordPaste)], ncol = 2)
    nUniqueLoc <- nrow(coordUnique)

    ctr <- gCentroid(spdfTmp) ## still valid if two points
    centroidX <- ctr@coords[1]
    centroidY <- ctr@coords[2]

    if (nUniqueLoc > 1) {
      distances <- distGeo(ctr, sp::coordinates(spdfTmp)) ## the unstransformed spdf
      distM <- distm(spdfTmp, spdfTmp) ## the unstransformed spdf
      distMLT <- distM[lower.tri(distM)]
      distancesOut <- distMLT[which(distMLT>0)]

      spdfTmpTr <- sp::spTransform(spdfTmp,
                                   CRSobj = suppressWarnings(
                                     CRS("+init=epsg:3857")))
      # coord <- sp::coordinates(spdfTmpTr)
      # coordPaste <- apply(coord, 1, paste0, collapse = ",")
      # coordUnique <- matrix(coord[!duplicated(coordPaste)], ncol = 2)
      # shotGroups::getMinCircle(coordUnique) # The minimum circle that covers all points
      # this function is very much dependent on the projection
      # issue #4 the function shotgun::minCircle() is not reliable for extreme
      # cases with few points or with outliers. We stick to max distance from centroid.

      effortDiam <- round(max(distances) * 2, 0)
      medianDist <- round(median(distances), 0)
      iqrDist    <- round(IQR(distances), 0)
      # nOutliers  <- length(boxplot.stats(distancesOut)$out) ### TODO think another way to compute this
      if(nUniqueLoc >= minPts ){
        clusters  <- dbscan(sp::coordinates(spdfTmpTr),
                            eps = median(distancesOut),
                            minPts = minPts)
        nOutliers <- sum(clusters$cluster==0)
        nClusters  <- sum(unique(clusters$cluster)!=0)
      } else {
        nClusters  <- 1
        nOutliers  <- 0
      }

    } else {
      effortDiam <- 1 # 1m
      medianDist <- 1
      iqrDist    <- 1
      nClusters  <- 1
      nOutliers  <- 0
    }

    return(c(centroidX, centroidY, effortDiam, medianDist,
           iqrDist, nUniqueLoc, nClusters, nOutliers))
  } )
  varsCtr <- c("centroidX", "centroidY","effortDiam", "medianDist","iqrDist",
             "nUniqueLoc", "nClusters", "nOutliers")
  tmp <- matrix(unlist(ctrList), ncol = length(varsCtr), byrow = TRUE,
                dimnames = list(uniqueUID, varsCtr))

  visitStat[, match(varsCtr, colnames(visitStat))] <- tmp

  # visitStat$date <- as.Date(paste(visitStat$year,
  #                                 visitStat$month,
  #                                 visitStat$day,
  #                                 sep="-"),
  #                           format = "%Y-%m-%d")
  visitStat$date <- lubridate::ymd(paste(visitStat$year,
                                         visitStat$month,
                                         visitStat$day,
                                         sep="-"))
  visitStat$Month <- as.factor(months(visitStat$date, abbreviate = FALSE))
  levels(visitStat$Month) <- month.name

  message(paste("Finished analysing", nUID, "visits.\n"))
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
#' library(sp)
#' OB<-organizeBirds(bombusObsShort)
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

  utmCRS <- suppressWarnings(CRS(getUTMproj(x)))
  xTrans <- spTransform(x, CRSobj = utmCRS)

  buff <- rgeos::gBuffer(xTrans,  byid = TRUE, id=x@data$visitUID, width=radiusVal)
  buff <- spTransform(buff, CRSobj = dataCRS)

  return(list("points"=x, "effort"=buff))

}

## internal function. Depends on the polygon object utmZones
#' @keywords internal
getUTMzone <- function(points){
  ##Find which UTM-zones that have the most points
  utmZonesTr <- suppressWarnings(spTransform(utmZones,
                                             slot(points, "proj4string"))
                              ) #To accept all reference systems for points.
  utmZone <- over(points, utmZonesTr)
  freqZones <- table(utmZone$ZONE[utmZone$ZONE !=0 ]) ## Zone 0 is both norht and south so we check for it later
  maxZones <- names(freqZones)[which(freqZones == max(freqZones))]

  alternatives <- rep(FALSE, length(maxZones))
  names(alternatives) <- maxZones

  ## To check on zone 0 points we need to check row A, B, Y and Z
  freqRows <- table(utmZone$ROW_)

  if(is.null(maxZones)){
    ##First we need to check if there is points in any other zone.
    ##If not we accept anything more than zero
    if(any(sum(freqRows[c("A", "B")]) > 0, sum(freqRows[c("Y", "Z")]) > 0)){
      alternatives<-c("0" = FALSE)
    }
  }else{
    ##Else we check if there is more or as many points in zone 0 as in any other zone.
    if(any(sum(freqRows[c("A", "B")]) > freqZones[maxZones],
           sum(freqRows[c("Y", "Z")]) > freqZones[maxZones])){
      alternatives<-c("0" = FALSE)
    }else if(any(sum(freqRows[c("A", "B")]) == freqZones[maxZones],
                 sum(freqRows[c("Y", "Z")]) == freqZones[maxZones])){
      alternatives["0"]<-FALSE
    }
  }

  ##Goes through the zones with most points and checks if other points are to far away.
  for(a in names(alternatives)){
    if(as.integer(a)>0){
      ##If not in polar regions
      if(a != "1" && a != "60"){
          ##If not at the "edge" of the world
          aint <- as.integer(a)
          alternatives[a] <- all(as.integer(names(freqZones)) %in%  c(aint-1, aint, aint+1, 0))
      }else{
          ##We're at the edge of the world, or at least the edge of the UTM world map
          if(a == "1"){
            alternatives[a] <- all(names(freqZones) %in%  c("60", "1", "2", "0")) ##These are the zones we accept
          }else{
            alternatives[a] <- all(names(freqZones) %in%  c("59", "60", "1", "0")) ##These are the zones we accept
          }
        }
    }else{
      ## We are in the polar regions!
      alternatives[a] <- any(
        all(utmZone$ROW_ %in%  c("X", "Y", "Z")), ## We're in the nothern hemisphere!
        all(utmZone$ROW_ %in%  c("A", "B", "C")) ## Now we're down south!
      )
    }
  }

  ##Results
  res <- list("zone" = NULL) #, "msg" = NULL)
  if(sum(alternatives) == 0){
    ##No acceptable zones found
    message("There is no UTM zone where all points overlap with or with its adjacent zones.\n")
    # return(res)
  }else if (sum(alternatives) == 1){
    ## One zone found with points only in adjecent zone found.
    if(all(names(alternatives) == "0")){
      if(all(utmZone$ROW_ %in%  c("X", "Y", "Z"))){
        res$zone <- "0N"
      }else{
        res$zone <- "0S"
      }
    }else{
      res$zone <- as.integer(names(alternatives)[alternatives])
    }
  }else{
    ## Two adjacent zones with same number of points were found.
    ## If one of them is zone 0 the other must be in row X or Z, hence we choose zone 0
    if("0" %in% names(alternatives)){
      if(all(utmZone$ROW_ %in%  c("X", "Y", "Z"))){
        res$zone <- "0N"
      }else{
        res$zone <- "0S"
      }
      message("The points are split over two UTM-zones but close to the polar regions. Therefor zone 0 is chosen.\n")
    }

    meanPoint <- sp::SpatialPoints(geosphere::geomean(points),
                                   proj4string = slot(points, "proj4string"))
    utmMeanZone <- sp::over(meanPoint, utmZones)
    res$zone <- as.integer(utmMeanZone$ZONE)
    message("The points are split over two UTM-zones. The zone with the centroid for all the points was chosen.\n")
  }
  return(res$zone)
}

#' A wrapper around getUTMzone and produce a proj4 string
#'
#' @param x an object of class \sQuote{OrganizedBirds} or \sQuote{SpatialPointsDataFrame}
#' @return a proj4 character string for an apropiate UTM zone
#' @export
#' @examples
#' OB <- organizeBirds(bombusObs)
#' getUTMproj(OB)
getUTMproj<-function(x){
  if (class(x) == "OrganizedBirds") {
    spdf <- x$spdf
  } else {
    if(class(x) == "SpatialPointsDataFrame"){
      spdf <- x
      spdf <- spTransform(spdf,
                          CRSobj = suppressWarnings(
                            CRS("+init=epsg:4326"))
                          )
    } else {
      stop("Input data is neither an object of class 'OrganizedBirds' or 'SpatialPointsDataFrame'")
    }
  }

  ## error no CRS
  # if (is.na(proj4string(spdf))) { #slot(points, "proj4string") or soon wkt()
  #   stop("The polygon has no coordinate projection system (CRS) associated")
  # }
  if (is.na(slot(spdf, "proj4string"))) {
    stop("The polygon has no coordinate projection system (CRS) associated")
  }

  utmZone <- getUTMzone(spdf)
  if(!is.null(utmZone)){
    if(utmZone == "0N"){
      #"+init=epsg:5041"
      proj4 <- "+proj=stere +lat_0=90 +lat_ts=90 +lon_0=0 +k=0.994 +x_0=2000000 +y_0=2000000 +datum=WGS84 +units=m +no_defs" #"+init=epsg:5041"
    }
    if(utmZone == "0S"){
      #"+init=epsg:5042"
      proj4 <- "+proj=stere +lat_0=-90 +lat_ts=-90 +lon_0=0 +k=0.994 +x_0=2000000 +y_0=2000000 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
    }
    if(is.integer(utmZone)){
      proj4 <- paste0("+proj=utm +zone=", utmZone," +datum=WGS84")
    }
  }
  return(proj4)
}

