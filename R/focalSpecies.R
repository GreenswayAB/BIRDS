#' Lists all species names from the data set
#'
#' Lists all species names from the data set.
#' @param x an object of class \sQuote{OrganizedBirds} or \sQuote{SummarizeBirds}.
#' @return a \code{vector} with all species names in the data set
#' @examples
#' OB <- organizeBirds(bombusObsShort, sppCol = "scientificName", simplifySppName = TRUE)
#' allSpp <- listSpecies(OB)
#' @export
#' @seealso \code{\link{summarizeBirds}}, \code{\link{exportBirds}}
listSpecies<-function(x){
  if (class(x) == "OrganizedBirds") {
    allSpecies<- sort(unique(as.character(x$spdf$scientificName)))
  } else if (class(x) == "SummarizedBirds") {
    overList<-x$overlaid
    allSpecies<-sort( unique( unlist(
      lapply(overList, FUN=function(x)return(x$scientificName)))))
  } else {
    stop("The object 'x' must be of class OrganizedBirds or SummarizedBirds.")
  }
  return(allSpecies)
}

#' Summarize records of a species among all visits
#'
#' A function to summarise records of a species among all visits. Returns number of grid cells with occurrences,
#' number of observations, number of visits, number of years and number of months with occurrences.
#' @param x an object of class \sQuote{SummarizeBirds}.
#' @param focalSp the focal sp to look for.
#' @return a \code{data.frame} with summary data for the focal species
#' @examples
#' \donttest{
#' OB <- organizeBirds(bombusObsShort, sppCol = "scientificName", simplifySppName = TRUE)
#' grid <- makeGrid(searchPolygon, gridSize = 10)
#' SB <- summariseBirds(OB, grid=grid)
#' allSpp <- listSpecies(SB)
#' focal<-"Bombus campestris"
#' focalSpSummary(SB, focalSp=focal)
#' }
#' @export
#' @seealso \code{\link{summarizeBirds}}, \code{\link{exportBirds}}
focalSpSummary <- function(x, focalSp=NULL){
  if (class(x) != "SummarizedBirds") {
    stop("The object 'x' must be of class SummarizedBirds.")
  }
  if (is.null(focalSp)) {
    stop("Please, define the focal species to search for.")
  }
  visitCol<-attr(x, "visitCol")

  allSpecies <- listSpecies(x)

  wFocal <- match(focalSp, allSpecies)

  wOverFocal <- unname(unlist(lapply(x$overlaid,
                                     FUN=function(x) {
                                       return(focalSp %in% x$scientificName)
                                     }
                                     )
                              )
                       )

  if(sum(wOverFocal) == 0) stop("The focal species was not found in the data set.")
  # overFocal <- lapply(x$overlaid, FUN=function(x) return(x[x$scientificName==focalSp,]))
  overFocal <- x$overlaid[wOverFocal]


  ## if there is a column for presence then remove absences
  if("presence" %in% colnames(overFocal[[1]]) ) {
    overFocal <- lapply(overFocal, FUN=function(x){
      wNotPres <- which(x$presence != 1 | is.na(x$presence))
      if(length(wNotPres)>1){
        return(x[-wNotPres,])
      } else { return(x) }
    })
  }

  yearsAll <- sort(unique(lubridate::year(x$temporal)))
  yearRng <- range(yearsAll) # range(unname(unlist(lapply(x$overlaid, FUN=function(x) x$year) )))
  yearMax <- max(yearsAll)
  yearMin <- min(yearsAll)

  # wNonEmptyFocal <- unname(which(unlist(lapply(overFocal, nrow))>0))
  # nCells <- length(wNonEmptyFocal)
  # nObs <- sum(unlist(lapply(overFocal[wNonEmptyFocal], nrow)))
  # visitsFocal <- unname(unlist(lapply(overFocal[wNonEmptyFocal], FUN=function(x) x[,visitCol]) ))
  # nVis <- length(visitsFocal)
  # yearsFocal <- sort(unique(unlist(lapply(overFocal[wNonEmptyFocal], FUN=function(x) x[,"year"]) )))
  # monthsFocal <- sort(unique(unlist(lapply(overFocal[wNonEmptyFocal], FUN=function(x) x[,"month"]) )))
  nCells <- sum(wOverFocal)
  nObs <- sum(unlist(lapply(overFocal,
                            function(x) return(sum(x$scientificName == focalSp))) ))
  visitsFocal <- unique(unname(unlist(lapply(overFocal,
                                             FUN=function(x) x[x$scientificName == focalSp,visitCol]) )))
  nVis <- length(visitsFocal)
  yearsFocal <- sort(unique(unlist(lapply(overFocal,
                                          FUN=function(x) x[x$scientificName == focalSp,"year"]) )))
  monthsFocal <- sort(unique(unlist(lapply(overFocal,
                                           FUN=function(x) x[x$scientificName == focalSp,"month"]) )))

  return(data.frame("species"=focalSp,
                    "nCells"=nCells,
                    "nObs"=nObs,
                    "nVis"=nVis,
                    "visitsUID"=paste(visitsFocal, collapse = ","),
                    "nYears"=length(yearsFocal),
                    "nMonths"=length(monthsFocal),
                    stringsAsFactors = FALSE))
}


#' Summarise all records for a species
#'
#' This function will produce a simple visual report for the obsrevation pattern
#' of the focal species. It shows grid cells with records on a map,
#' and bar charts with number of records per year and month.
#' @param x an object of class \sQuote{SummarizeBirds}.
#' @param focalSp the focal spp to look for.
#' @param long whether the map should be long or wide.
#' @param polygon (optional) an object of class \sQuote{SpatialPolygon} or
#' \sQuote{SpatialPolygonDataFrame}. (Default is \code{NULL})
#' @param colVis color to plot visited gird cells
#' @param colPres color to plot grid cells where species is present
#' @param ... further plot parameters
#' @return a plot with a brief species summary
#' @examples
#' \donttest{
#' library(sp)
#' OB <- organizeBirds(bombusObsShort, sppCol = "scientificName", simplifySppName = TRUE)
#' grid <- makeGrid(searchPolygon, gridSize = 10)
#' SB <- summariseBirds(OB, grid=grid)
#' allSpp <- listSpecies(SB)
#' focal <- allSpp[2]
#' focalSpReport(SB, focalSp=focal)
#' }
#' @export
#' @seealso \code{\link{summarizeBirds}}, \code{\link{exportBirds}}
focalSpReport <- function(x, focalSp=NULL, long=TRUE, polygon = NULL,
                          colVis = "grey", colPres = "red", ...){
  if (class(x) != "SummarizedBirds") {
    stop("The object 'x' must be of class SummarizedBirds.")
  }
  if (is.null(focalSp)) {
    stop("Please, define the focal species to search for.")
  }
  visitCol<-attr(x, "visitCol")
  # wNonEmpty<-unname(which(unlist(lapply(x$overlaid, nrow))>0))
  wNonEmpty<-attr(x, "nonEmptyGridCells")

  allSpecies <- listSpecies(x)

  wFocal <- match(focalSp, allSpecies)
  if(!(focalSp %in% allSpecies)) stop(paste0("The focal species ", focalSp,
                                             " was not found among the species names in the data set."))
  # overFocal <- lapply(x$overlaid, FUN=function(x)return(x[x$scientificName==focalSp,]))
  wOverFocal <- unname(unlist(lapply(x$overlaid,
                                     FUN=function(x) return(focalSp %in% x$scientificName))))
  overFocal <- x$overlaid[wOverFocal]

  yearsAll <- sort(unique(lubridate::year(x$temporal)))
  yearRng <- range(yearsAll)
  yearMax <- max(yearsAll)
  yearMin <- min(yearsAll)

  wNonEmptyFocal <- wOverFocal #unname(which(unlist(lapply(overFocal, nrow))>0))
  nObs <- sum(unlist(lapply(overFocal,
                            function(x) return(sum(x$scientificName == focalSp))) ))
  visitsFocal <- unique(unname(unlist(lapply(overFocal,
                                      FUN=function(x) x[x$scientificName == focalSp, visitCol])
                                      )))
  nVis <- length(visitsFocal)
  yearsFocal <- unname(unlist(lapply(overFocal,
                              FUN=function(x) x[x$scientificName == focalSp,"year"]) ))
  yearsFocalTbl <- table( factor(yearsFocal,
                             levels = yearsAll)
                        )

  monthsFocal <- unname(unlist(lapply(overFocal,
                                      FUN=function(x) x[x$scientificName == focalSp,"month"]) ))
  monthsFocalTbl <- table( factor(monthsFocal,
                                  levels=1:12,
                                  labels = month.abb[1:12])
                           )

  reportStrg <- paste0("Number of observations: ", nObs)

  ### check the polygon
  if(!is.null(polygon)){
    if (!(class(polygon) %in% c("SpatialPolygons", "SpatialPolygonsDataFrame"))) {
      warning("Entered polygon is not a SpatialPolygon nor SpatialPolygonsDataFrame.")
      polygon<-NULL
    }else{
      ## error no CRS
      if (is.na(proj4string(polygon))) {
        warning("The polygon has no coordinate projection system (CRS) associated")
        polygon<-NULL
      } else {
        # Transform to SB$spatial projection
        # polygon <- spTransform(polygon, CRSobj = slot(x$spatial, "proj4string"))
        polygon <- sf::as_Spatial(
                    sf::st_transform(
                      sf::st_as_sf(polygon),
                      crs = sf::st_crs(slot(x$spatial, "proj4string"))$wkt) )
      }
    }
  }

  oldpar <- par(no.readonly =TRUE)
  on.exit(par(oldpar))
  layout(matrix(c(1,2,1,3), nrow = 2, byrow = long))
  par(mar=c(1,1,1,1))
  plot(x$spatial[wNonEmpty,], col = colVis, border = NA, ...)
  plot(x$spatial[wNonEmptyFocal,], col = colPres, border = NA, add=TRUE)
  if(!is.null(polygon)) plot(polygon, col=NA, border=1, lwd=2, add=TRUE)
  mtext(focalSp, side=3, font = 3, line = -.5)
  mtext(reportStrg, side=3, font = 1, line = -1.5, cex = .8)
  legend("bottomleft", legend=c("visited", "present"), col = c(colVis, colPres), pch = 15, bty="n")

  par(mar=c(3,4,1,1))
  barplot(yearsFocalTbl, ylab = "n. visits", las=2)
  par(mar=c(4,4,1,1))
  barplot(monthsFocalTbl, ylab = "n. visits", las=2)
}


#' Summarize all records for a species
#'
#' A function that counts the number of observations, number of visits and number of grid cells with occurrences for all species.
#' @param x an object of class \sQuote{SummarizeBirds}.
#' @return a \code{data.frame} with summary data for each species
#' @examples
#'\donttest{
#' grid <- makeGrid(searchPolygon, gridSize = 10)
#' SB <- summarizeBirds(organizeBirds(bombusObsShort), grid=grid)
#' summSB <- speciesSummary(SB)
#' }
#' @export
#' @seealso \code{\link{summarizeBirds}}, \code{\link{exportBirds}}
speciesSummary <- function(x){
  if (class(x) != "SummarizedBirds") {
    stop("The object 'x' must be of class SummarizedBirds.")
  }
  allSpecies <- listSpecies(x)
  res <- data.frame("species"=character(0),
                  "nCells"=numeric(0),
                  "nObs"=numeric(0),
                  "nVis"=numeric(0),
                  "visitsUID"=character(0),
                  "nYears"=numeric(0),
                  "nMonths"=numeric(0),
                  stringsAsFactors = FALSE)

  tmp <- sapply(allSpecies,
                function(y){
                  fsp <- focalSpSummary(x, focalSp = y)
                  message(y)
                  return(fsp)
                }, simplify = FALSE)
  res <- do.call(rbind, tmp)
  # res <- rbind(res, as.data.frame(tmp))
  rownames(res) <- NULL

  # for(s in allSpecies){
  #   res<-rbind(res, focalSpSummary(x, focalSp = s))
  #   message(s,"\n")
  # }

  return(res)
}
