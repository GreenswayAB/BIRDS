#' An internal function to explore the definition of field visits
#'
#' @param x an subset object to pass to apply
#' @param dat an object of class \sQuote{OrganizedBirds} (organised BIRDS Spatial data frame).
#' @param visitCol name of the column for the visits UID.
#' @param spdf spatial object of dat.
#' @param minPts minimum number of points, default to 3.
#' @return a \code{data.frame} with summarized data per visit:
#' \itemize{
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
#' @importFrom rlang .data
#' @importFrom dplyr group_by summarise n n_distinct sym
#' @importFrom dbscan dbscan
#' @importFrom lubridate date day month year ymd
#' @importFrom geosphere distGeo distm
#' @seealso \code{\link{createVisits}}, \code{\link{organiseBirds}}
#' @keywords internal
lapplyVisits <- function(x, dat, visitCol, spdf, minPts){
  wVis <- which(dat[, visitCol] == x)
  spdfTmp <- spdf[wVis, ]
  spdfTmpTr <- st_transform(spdfTmp,
                            crs = st_crs(3857) )

  coord <- st_coordinates(spdfTmp) # the untransformed spdf
  coordPaste <- apply(coord, 1, paste0, collapse = ",")
  coordUnique <- matrix(coord[!duplicated(coordPaste)], ncol = 2)
  nUniqueLoc <- nrow(coordUnique)

  ctr <- spdfTmpTr %>%
    st_union() %>%
    st_convex_hull() %>%
    st_centroid() %>%
    st_transform( 4326 ) %>%
    st_coordinates()

  centroidX <- ctr[,"X"]
  centroidY <- ctr[,"Y"]

  if (nUniqueLoc > 1) {
    distances <- distGeo(ctr[, c("X", "Y")], coord)
    distM <- distm(coord, coord) ## the untransformed spdf
    distMLT <- distM[lower.tri(distM)]
    distancesOut <- distMLT[which(distMLT > 0)]

    # shotGroups::getMinCircle(coordUnique) # The minimum circle that covers all points
    # this function is very much dependent on the projection
    # issue #4 the function shotgun::minCircle() is not reliable for extreme
    # cases with few points or with outliers. We stick to max distance from centroid.

    effortDiam <- round(max(distances) * 2, 0)
    medianDist <- round(median(distances), 0)
    iqrDist    <- round(IQR(distances), 0)

    if(nUniqueLoc >= minPts ){
      clusters  <- dbscan(st_coordinates(spdfTmpTr),
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
}

#' A function to explore the definition of field visits
#'
#' A function to explore the definition of field visits. Visits are a central concept
#' in the approach to species observation data used by the BIRDS package. In order to assess if your
#' definition of visit aligns with your grid size, you must explore the spatial extent of visits.
#' @param x an object of class \sQuote{OrganizedBirds} (organised BIRDS Spatial data frame).
#' See \code{\link{organizeBirds}}.
#' @param visitCol name of the column for the visits UID.
#' @param sppCol name of the column for species names.
#' @param parallel logic. Whether to run in parallel (then the package 'parallel'
#' is required). Default is FALSE#'
#' @param nc integer or NULL
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
#' # to see the distributions of observations along the days of the month
#' plot(visitStats$day, visitStats$nObs)
#' }
#' @export
#' @importFrom rlang .data
#' @importFrom dplyr group_by summarise n n_distinct sym
#' @importFrom dbscan dbscan
#' @importFrom lubridate date day month year ymd
#' @importFrom geosphere distGeo distm
#' @seealso \code{\link{createVisits}}, \code{\link{organiseBirds}}
exploreVisits <- function(x,
                        visitCol=NULL, #visitCol=attr(x, "visitCol"),
                        sppCol="scientificName",
                        parallel = FALSE,
                        nc = NULL){
  if(!is.logical(parallel)) stop("Argument 'parallel' is required and needs to be 'logical'")

  if(parallel){
    # if(!"parallel" %in% installed.packages()) stop("The package 'parallel' is required if the argument 'parallel' is set to TRUE")
    if(length(find.package(package = "parallel", 
                           quiet = TRUE, 
                           verbose = FALSE)) == 0) stop("The package 'parallel' is required if the argument 'parallel' is set to TRUE")

    ### handling cores
    ncDet <- parallel::detectCores()
    if(is.null(nc)){
      nc <- ncDet-1
    }else{
      if(is.integer(nc) | is.numeric(nc)){
        nc <- max(1, round(nc))
      }else{
        stop("'nc' need to be integer or numeric")
      }
    }

    if(ncDet < nc){
      nc <- ncDet-1
      warning("The number of detected cores is smaller than the required by 'nc'. Falling back to nc = 'number of cores - 1'")
    }

    if(parallel & nc == 1){
      parallel <- FALSE
      message("Use 'nc' > 1 to take advantage of parallel")
    }
  }

  ## Other requirements
  minPts <- 3 ## Minimum number of points required for clustering

  if (inherits(x, "OrganizedBirds")) {
    spdf <- x$spdf
    dat <- st_drop_geometry(spdf)
  } else {
    stop("The object 'x' must be of class OrganizedBirds. See the function 'organizedBirds()'.")
  }
  if (is.null(visitCol)) {
    visitCol <- attr(x, "visitCol")
  }

  if (!(visitCol %in% colnames(dat))) stop(paste("There is no column called",
                                                 visitCol, "in your organised dataset."))

  uniqueUID <- unique(dat[, visitCol])
  uniqueUID <- sort(uniqueUID)
  nUID <- length(uniqueUID)

  dat$date <- ymd(paste(dat$year, dat$month, dat$day, sep = "-"))

  visitStat <- data.frame("visitUID" = uniqueUID,
                          "day" = NA,
                          "month" = NA,
                          "Month" = NA,
                          "year" = NA,
                          "date" = NA,
                          "centroidX" = NA,
                          "centroidY" = NA,
                          "nObs" = NA, # number of records per visit, NOT species list length  #as.numeric(tbl),
                          "SLL" = NA, # species list length, i.e. number of observed species
                          "effortDiam" = NA, # the diameter of the minumum circle that covers all points, in meters
                          "medianDist" = NA, # the median (Q2) of the distances between the centroid and all points
                          "iqrDist" = NA, # the interquartile range of the distances between the centroid and all points
                          "nUniqueLoc" = NA, # number of unique locations
                          "nClusters" = NA, # Number of clusters
                          "nOutliers" = NA) # number of observations estimated to be outliers from the clusters

  message(paste("Analysing", nUID, "visits..."))
  datGBY <- group_by(dat, !! sym(visitCol))

  visitStat$nObs <- summarise(datGBY, nObs = n())$nObs
  visitStat$SLL  <- summarise(datGBY, SLL = n_distinct(.data$scientificName))$SLL
  dates <- summarise(datGBY, date = min(date)) ##If the visits are over multiple days, we take the first.
  visitStat$day   <- day(dates$date)
  visitStat$month <- month(dates$date)
  visitStat$year  <- year(dates$date)
  rm(datGBY)

  envirFunc <- environment()
  ### TODO? can this lapply be done with dplyr?
  if (!parallel) {
    ctrList <- lapply(uniqueUID,
                      FUN = lapplyVisits,
                      dat = dat, visitCol = visitCol, 
                      spdf = spdf, minPts = minPts ) #end lapply
  }else{
    cl <- parallel::makeCluster(nc)
    parallel::clusterExport(cl, varlist = list("uniqueUID", "dat", "visitCol", "spdf", "minPts"), envir = envirFunc)
    parallel::clusterEvalQ(cl, {
                        library("sf")
                        library("dbscan")
                      }
      )
    message("Running in parallel with ", nc, " cores")
    ctrList <- parallel::parLapply(cl,
                                   uniqueUID,
                                   fun = lapplyVisits,
                                   dat = dat, visitCol = visitCol, spdf = spdf, minPts = minPts)
    parallel::stopCluster(cl)
  }

  varsCtr <- c("centroidX", "centroidY","effortDiam", "medianDist","iqrDist",
             "nUniqueLoc", "nClusters", "nOutliers")
  tmp <- matrix(unlist(ctrList),
                ncol = length(varsCtr),
                byrow = TRUE,
                dimnames = list(uniqueUID, varsCtr))

  visitStat[, match(varsCtr, colnames(visitStat))] <- tmp

  visitStat$date <- lubridate::ymd(paste(visitStat$year,
                                         visitStat$month,
                                         visitStat$day,
                                         sep = "-"))
  visitStat$Month <- as.factor(months(visitStat$date, abbreviate = FALSE))
  levels(visitStat$Month) <- month.name

  message(paste("Finished analysing", nUID, "visits.\n"))
  return(visitStat)
}


#' A function to convert visits into a spatial object
#' @param x an object of class \sQuote{data.frame} from exploreVisits.
#' @param xyCols a character vector with the column names for the coordinates.
#' Default to \code{c("centroidX","centroidY")}
#' @param dataCRS a character string or numeric with the original
#' coordinate reference system (CRS). Default to \code{4326}
#' @param radius either a character string with the name of the column
#' containing the radius of the visit circle, or a numeric vector with its value
#' in meters. Default to \code{"medianDist"}
#'
#' @return a list with the centroids and a the effort circles. Note that when plotted
#' directly effort circles may not look like circles in the returned
#' (Pseudo-Mercator) projection.
#'
#' @examples
#' \donttest{
#' # create a visit-based data object from the original observation-based data
#' library(sf)
#' OB <- organizeBirds(bombusObsShort)
#' visitStats<-exploreVisits(OB)
#' spV<-spatialVisits(visitStats)
#' plot(spV$effort$geometry)
#' }
#' @export
#' @seealso \code{\link{exploreVisits}}, \code{\link{organiseBirds}}
spatialVisits <- function(x,
                          xyCols=c("centroidX","centroidY"),
                          dataCRS="4326",
                          radius="medianDist"){
  crs <- st_crs(as.numeric(dataCRS))

  if (inherits(x, "data.frame")) {
    x <- st_as_sf(x, coords = xyCols)
    st_crs(x) <- st_crs(crs) ## because I know where it comes from
  } else {
    stop("The object 'x' must be of class data.frame (after exploreVisits). See the function 'exploreVisits()'.")
  }

  if (radius == "" | is.na(radius) | is.null(radius)) {
    radiusVal <- rep(1, nrow(x))
  } else if (radius %in% colnames(x)) {
    radiusVal <- st_drop_geometry(x[,radius])
    ## convert meters to degrees?
  } else if (is.numeric(radius) & length(radius) == nrow(x)) {
    radiusVal <- radius
  } else {
    stop("The parameter 'radius' needs to be one od the column names or a numeric
         vector of length equal to the number of visits")
  }

  utmCRS <- st_crs(getUTMproj(x))
  xTrans <- st_transform(x,
                         crs = utmCRS)

  buff <- st_buffer(xTrans,
                    dist = as.integer(unlist(radiusVal)))

  buff <- st_transform(buff,
                       crs = crs)

  return(list("points" = x, "effort" = buff))

}
