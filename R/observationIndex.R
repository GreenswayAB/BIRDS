#' Observation Index
#'
#' This function extracts the proportion of observations for a focal species to
#' all observations (including the focal species) over time or area.
#'
#' @param x an object of class \sQuote{SummarizeBirds}.
#' @param dimension a character string indicating if the export should be
#'   \code{"spatial"} or \code{"temporal"}
#' @param timeRes the time resolution as a character string if \code{dimension = "temporal"}:  \code{"Yearly", "Monthly"} or \code{"Daily"}
#' @param focalSp the focal species to look for.
#'
#' @return If \code{dimension = "spatial"} a \sQuote{SpatialPolygonsDataFrame} or a \sQuote{xts} timeseries if \code{dimension = "temporal"}.
#' @export
#'
#' @examples
#' grid <- makeGrid(gotaland, gridSize = 10)
#' PBD<-bombusObs
#' OB <- organizeBirds(PBD, sppCol = "scientificName", simplifySppName = TRUE)
#' SB <- summariseBirds(OB, grid=grid)
#' spp <- listSpecies(SB)
#' obsInd<-obsIndex(SB, "temporal", "yearly", focalSp=spp[1])

obsIndex<-function(x, dimension, timeRes = NULL, focalSp = NULL){
  
  dimension<-tolower(dimension)
  
  if(dimension=="spatial"){
    if(! is.null(timeRes)){
      warning("'timeRes' is not NULL. It will not be used in a spatial export.")
    }
    return(obsIndexSpatial(x, focalSp))
  }else if(dimension=="temporal"){
    return(obsIndexTemporal(x, timeRes, focalSp))
  }else{
    stop("Unknown defenition of \"dimension\"")
  }
}

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
#' This function extracts the proportion of observations for a focal species to
#' the all observations (including the focal species) over time.
#' @param x an object of class \sQuote{SummarizeBirds}.
#' @param timeRes the time resolution:  \code{"Yearly", "Monthly"} or \code{"Daily"}
#' @param focalSp the focal sp to look for.
#'
#' @return An xts timeseries
#'
#' @examples
#' grid <- makeGrid(gotaland, gridSize = 10)
#' PBD<-bombusObs
#' OB <- organizeBirds(PBD, sppCol = "scientificName", simplifySppName = TRUE)
#' SB <- summariseBirds(OB, grid=grid)
#' spp <- listSpecies(SB)
#' obsInd<-obsIndexTemporal(SB, "yearly", focalSp=spp[1])
#' 
#' @keywords internal
obsIndexTemporal<-function(x, timeRes, focalSp=NULL){
  if (class(x) != "SummarizedBirds") {
    stop("The object 'x' must be of class SummarizedBirds.")
  }
  if (is.null(focalSp)) {
    stop("Please, define the focal species to search for.")
  }
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


  allN<-summarise(group_by(spData, dates), all=n())
  spNgby<-group_by(spData[spData$scientificName==focalSp,], dates)

  ## if there is a column for presence then remove absences
  spNgby<-extractPresence(spNgby)

  spN<-summarise(spNgby, sp=n())

  allN<-xts::xts(allN$all, allN$dates)
  spN<-xts::xts(spN$sp, spN$dates)

  res<-merge(res,allN,join='left')
  res<-merge(res,spN,join='left', fill=0)
  
  res<-res[,-1]

  if(! "allN" %in% colnames(res)){
    res$spN<-NA
  }
  
  
  if(! "spN" %in% colnames(res)){
    res$spN<-0
  }
  
  res$relObs<-res$spN/res$allN

  return(res)

}


obsIndexSpatial<-function(x, focalSp=NULL){
  if (class(x) != "SummarizedBirds") {
    stop("The object 'x' must be of class SummarizedBirds.")
  }
  if (is.null(focalSp)) {
    stop("Please, define the focal species to search for.")
  }
  
  r<-lapply(x$overlaid, function(x){
    c(nrow(x),nrow(extractPresence(x[x$scientificName==focalSp,])))
  })
  
  r<-data.frame(matrix(unlist(r), ncol = 2, byrow = TRUE))
  
  colnames(r)<-c("allN", "spN")
  
  r[r$allN==0,"allN"]<-NA
  
  r$relObs<-r$spN/r$allN
  
  res<-x$spatial
  
  res@data<-r
  
  return(res)  
  
}

