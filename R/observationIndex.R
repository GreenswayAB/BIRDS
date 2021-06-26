#' Normalize
#'
#' This function nomalizes a vector to a 0-1 range
#' @param x a numerical vector
#' @return a positive numeric vector
#'
#' @keywords internal
normalize <- function(x) {
  num <- (x - min(x, na.rm = TRUE))
  den <- (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  norm <- num / den
  return (norm)
}

#' The actual observation index function
#'
#' This function implements the algorithms to calculate the observation index.
#' OI = log ( (At / (At + Rt) ) / ( A / (A + R) ) )
#' where At is the sum of observations for focal species during time t (or grid),
#' Rt is sum of observations of all species in reference group during t,
#' A and R are the total sums for observations.
#' @param focal observations/visits for the focal species
#' @param group observations/visits for the reference species group
#' @param fs.rm if TRUE, assumes that the observations for the focal species are
#' included in 'group' and will remove them
#' @param norm if TRUE, the result is normalized to a 0-1 range
#' @return a positive observation index
#' @references Telfer, Preston & Rothery (2002) <doi:10.1016/S0006-3207(02)00050-2>
#' @keywords internal
logObsInd<-function(focal,
                    group,
                    fs.rm=TRUE,
                    norm=TRUE){
  if (fs.rm){
    group <- group - focal
    ## check: if group is negative after removing, then the focal specis
    if(any(group < 0)) stop("Some of the observations for the group are negative
                            after removing the focal species. Maybe they were
                            already removed? Try with 'fs.rm=FALSE'")
  }
  focalS <- sum(focal, na.rm=TRUE)
  if(focalS==0) return(rep(0,length(focal)))

  groupS <- sum(group, na.rm=TRUE)
  res2log <- (focal / (focal + group)) / (focalS / (focalS + groupS))

  res <- log( res2log + ifelse(res2log == 0, 0.1, 0) )
  if (norm){
    res <- normalize(res)
  }
  return(res)
}

#' Extract presence from SB
#'
#' This function removes absence observations of the focal species
#' @param x an object of class \sQuote{SummarizeBirds}.
#' @return returns x without observations where the value presence is < 1
#'
#' @keywords internal
extractPresence<-function(x){
  if("presence" %in% colnames(x) ) {
    wNotPres <- which(x$presence != 1 | is.na(x$presence))
    if(length(wNotPres) >= 1){
      x <- x[-wNotPres,]
    }
  }

  return(x)
}

#' Relative observation index (Temporal)
#'
#' This function extracts the proportion of visits (or observations) detecting
#' a focal species to all visits (or observations) over time.
#' @param x an object of class \sQuote{SummarizeBirds}.
#' @param timeRes the time resolution:  \code{"Yearly", "Monthly"} or \code{"Daily"}
#' @param focalSp the focal sp to look for.
#' @param visits if TRUE (default) the observation index is calculated over number
#' of visits, else uses the number of observations.
#' @param fs.rm if TRUE, assumes that the observations for the focal species are
#' included in 'group' and will remove them
#' @param norm if TRUE, the result is normalized to a 0-1 range
#'
#' @return An xts timeseries
#'
#' @keywords internal
#' @importFrom rlang .data
obsIndexTemporal<-function(x,
                           timeRes,
                           focalSp = NULL,
                           visits = TRUE,
                           fs.rm = TRUE,
                           norm = TRUE){
  if (class(x) != "SummarizedBirds") {
    stop("The object 'x' must be of class SummarizedBirds.")
  }
  if (is.null(focalSp)) {
    stop("Please, define the focal species to search for.")
  }
  visitCol <- attr(x, "visitCol")
  timeRes <- tolower(timeRes)
  yearsAll <- as.numeric(dimnames(x$spatioTemporal)[[2]])

  spData <- deconstructOverlay(x$overlaid, attr(x, "visitCol"))

  spData$dates <- as.Date(switch(timeRes,
                     "yearly" = paste0(spData$year, "-01-01"),
                     "monthly"= paste0(spData$year, "-", sprintf("%02d", spData$month), "-01"),
                     "daily"  = paste0(spData$year, "-", sprintf("%02d", spData$month), "-", sprintf("%02d", spData$day))))

  res <- if(timeRes=="yearly"){
    xts::xts(rep(NA, length(seq(min(spData$dates), max(spData$dates), by="year"))),
             seq(min(spData$dates), max(spData$dates), by="year"))
  }else if (timeRes=="monthly"){
    xts::xts(rep(NA, length(seq(min(spData$dates), max(spData$dates), by="month"))),
             seq(min(spData$dates), max(spData$dates), by="month"))
  }else if(timeRes=="daily"){
    xts::xts(rep(NA, length(seq(min(spData$dates), max(spData$dates), by="day"))),
             seq(min(spData$dates), max(spData$dates), by="day"))
  }else{
    stop("Unknown time resolution")
  }

  spNgby<-group_by(spData[spData$scientificName==focalSp,], .data$dates)
  ## if there is a column for presence then remove absences
  spNgby<-extractPresence(spNgby)

  if (visits){
    allN <- summarise(group_by(spData, .data$dates),
                      all= n_distinct(!!dplyr::sym(visitCol)))
    spN <- summarise(spNgby,
                     sp=n_distinct(!!dplyr::sym(visitCol)))
  } else {
    allN <- summarise(group_by(spData, .data$dates),
                      all=n())
    spN <- summarise(spNgby,
                     sp=n())
  }

  allN<-xts::xts(allN$all, allN$dates)
  spN<-xts::xts(spN$sp, spN$dates)

  res<-merge(res, allN, join='left')
  res<-merge(res, spN, join='left', fill=0)

  res<-res[,-1]

  if(! "allN" %in% colnames(res)){
    res$spN<-NA
  }

  if(! "spN" %in% colnames(res)){
    res$spN<-0
  }

  # res$relObs<-res$spN/res$allN
  res$relObs<-logObsInd(res$spN,
                        res$allN,
                        fs.rm=fs.rm,
                        norm=norm)
  return(res)
}

#' Relative observation index (Spatial)
#'
#' This function extracts the proportion of visits (or observations) detecting a
#' focal species to all visits (or observations) over space.
#' @param x an object of class \sQuote{SummarizeBirds}.
#' @param focalSp the focal sp to look for
#' @param visits if TRUE (default) the observation index is calculated over number
#' of visits, else uses the number of observations.
#' @param fs.rm if TRUE, assumes that the observations for the focal species are
#' included in 'group' and will remove them
#' @param norm if TRUE, the result is normalized to a 0-1 range
#'
#' @return A spatial object
#'
#' @keywords internal
obsIndexSpatial<-function(x,
                          focalSp = NULL,
                          visits = TRUE,
                          fs.rm = TRUE,
                          norm = TRUE){
  if (class(x) != "SummarizedBirds") {
    stop("The object 'x' must be of class SummarizedBirds.")
  }
  if (is.null(focalSp)) {
    stop("Please, define the focal species to search for.")
  }
  visitCol <- attr(x, "visitCol")
  overlaid<-x$overlaid

  r <- lapply(overlaid,
            function(x){
              if(!all(is.na(x))){
                # spNgby<-group_by(x[x$scientificName==focalSp,])
                spNgby<-x[x$scientificName==focalSp,]
                ## if there is a column for presence then remove absences
                spNgby<-extractPresence(spNgby)
                if (visits){
                  allN <- summarise(x, n_distinct(!!dplyr::sym(visitCol)))
                  spN <- summarise(spNgby, n_distinct(!!dplyr::sym(visitCol)))
                } else {
                  allN <- summarise(x, n())
                  spN <- summarise(spNgby, n())
                }

                return( c(pull(allN), pull(spN) ) )
              } else {
                return(c(0,0))
              }
            }
  )

  r <- data.frame(
          matrix(
            unlist(r),
            ncol = 2,
            byrow = TRUE)
          )
  colnames(r)<-c("allN", "spN")

  r$relObs <- logObsInd(r$spN,
                        r$allN,
                        fs.rm=fs.rm,
                        norm=norm)

  r[r$allN==0, ] <- NA
  res <- st_sf(cbind(r, st_geometry(x$spatial)))

  return(res)
}


#' Observation Index
#'
#' This function extracts the proportion of visits (or observations) detecting
#' a focal species to all visits (or observations) over time or space.
#'
#' @note  It implements the following algorithm to calculate the observation index:
#' OI = log ( (At / (At + Rt) ) / ( A / (A + R) ) )
#' where At is the sum of observations of a focal species during time t (or gridcell),
#' Rt is sum of observations of all species in reference group during t (or gridcell),
#' A and R are the total sums for observations. If the ratio to log = 0 it adds
#' 0.1 to avoid -Inf results.
#'
#' @param x an object of class \sQuote{SummarizeBirds}.
#' @param dimension a character string indicating if the export should be
#'   \code{"spatial"} or \code{"temporal"}
#' @param timeRes the time resolution as a character string if
#' \code{dimension = "temporal"}:  \code{"Yearly", "Monthly"} or \code{"Daily"}
#' @param focalSp the focal species to look for
#' @param visits if TRUE (default) the observation index is calculated over number
#' of visits, else uses the number of observations
#' @param fs.rm if TRUE, assumes that the observations for the focal species are
#' included in 'group' and will remove them
#' @param norm if TRUE, the result is nomalized to a 0-1 range

#' @references Telfer, Preston 6 Rothery (2002) <doi:10.1016/S0006-3207(02)00050-2>
#' @return If \code{dimension = "spatial"} a \sQuote{SpatialPolygonsDataFrame}
#' or a \sQuote{xts} timeseries if \code{dimension = "temporal"}.
#' @export
#'
#' @examples
#' \donttest{
#' grid <- makeGrid(gotaland, gridSize = 10)
#' PBD <- bombusObsShort
#' OB <- organizeBirds(PBD, sppCol = "scientificName", simplifySppName = TRUE)
#' SB <- summariseBirds(OB, grid=grid)
#' spp <- listSpecies(SB)
#' tempOI <- obsIndex(SB, "temporal", "yearly", focalSp=spp[3], fs.rm = FALSE)
#' plot(tempOI$relObs, main=spp[3])

#' spatOI <- obsIndex(SB, "spatial", focalSp=spp[3])
#' minOI <- min(spatOI$relObs, na.rm=TRUE)
#' maxOI <- max(spatOI$relObs, na.rm=TRUE)
#' palRW <- leaflet::colorNumeric(c("white", "red"), c(minOI, maxOI), na.color = "transparent")
#' plot(spatOI, col=palRW(spatOI$relObs), border="grey", main=spp[3])
#' legend("bottomleft", legend=seq(minOI, maxOI, length.out = 5),
#'        col = palRW(seq(minOI, maxOI, length.out = 5)), pch = 15, bty="n")
#'}
obsIndex<-function(x,
                   dimension,
                   timeRes = NULL,
                   focalSp = NULL,
                   visits = TRUE,
                   fs.rm = TRUE,
                   norm = TRUE){

  dimension<-tolower(dimension)
  if (dimension=="spatial"){
    if(! is.null(timeRes)){
      warning("'timeRes' is not NULL. It will not be used in a spatial export.")
    }
    return(obsIndexSpatial(x,focalSp, visits, fs.rm, norm))

  }else if (dimension=="temporal"){
    return(obsIndexTemporal(x, timeRes, focalSp, visits, fs.rm, norm))

  }else{
    stop("Unknown definition of \"dimension\"")
  }
}
