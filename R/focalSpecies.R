#' Lists all species names from the data set
#'
#' Lists all species names from the data set.
#' @param x an object of class \sQuote{OrganizedBirds} or \sQuote{SummarizeBirds}.
#' @return a \code{vector} with all species names in the data set
#' @examples
#' OB <- organizeBirds(bryophytaObs, sppCol = "scientificName", simplifySppName = TRUE)
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
#' OB <- organizeBirds(bryophytaObs, sppCol = "scientificName", simplifySppName = TRUE)
#' grid <- makeGrid(searchPolygon, gridSize = 10)
#' SB <- summariseBirds(OB, grid=grid)
#' allSpp <- listSpecies(SB)
#' focal<-"Zygodon viridissimus Bridel, 1826"
#' focalSpSummary(SB, focalSp=focal)
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

  overFocal <- lapply(x$overlaid, FUN=function(x)return(x[x$scientificName==focalSp,]))

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

  wNonEmptyFocal <- unname(which(unlist(lapply(overFocal, nrow))>0))
  nCells <- length(wNonEmptyFocal)
  nObs <- sum(unlist(lapply(overFocal[wNonEmptyFocal], nrow)))
  visitsFocal <- unname(unlist(lapply(overFocal[wNonEmptyFocal], FUN=function(x) x[,visitCol]) ))
  nVis <- length(visitsFocal)
  yearsFocal <- sort(unique(unlist(lapply(overFocal[wNonEmptyFocal], FUN=function(x) x[,"year"]) )))
  monthsFocal <- sort(unique(unlist(lapply(overFocal[wNonEmptyFocal], FUN=function(x) x[,"month"]) )))

  return(data.frame("species"=focalSp,
                    "nCells"=nCells,
                    "nObs"=nObs,
                    "nVis"=nVis,
                    "visitsUID"=paste(visitsFocal, collapse = ","),
                    # "years"=paste(yearsFocal, collapse = ","),
                    # "months"=paste(monthsFocal, collapse = ","),
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
#' @param colVis color to plot visited gird cells
#' @param colPres color to plot grid cells where species is present
#' @param ... further plot parameters
#' @return a plot with a brief species summary
#' @examples
#' library(sp)
#' OB <- organizeBirds(bryophytaObs, sppCol = "scientificName", simplifySppName = TRUE)
#' grid <- makeGrid(searchPolygon, gridSize = 10)
#' SB <- summariseBirds(OB, grid=grid)
#' allSpp <- listSpecies(SB)
#' focal<-"Zygodon viridissimus"
#' focalSpReport(SB, focalSp=focal)
#' @export
#' @seealso \code{\link{summarizeBirds}}, \code{\link{exportBirds}}
focalSpReport <- function(x, focalSp=NULL, long=TRUE, colVis = "grey", colPres = "red", ...){
  if (class(x) != "SummarizedBirds") {
    stop("The object 'x' must be of class SummarizedBirds.")
  }
  if (is.null(focalSp)) {
    stop("Please, define the focal species to search for.")
  }
  visitCol<-attr(x, "visitCol")
  wNonEmpty<-unname(which(unlist(lapply(x$overlaid, nrow))>0))
  allSpecies <- listSpecies(x)

  wFocal <- match(focalSp, allSpecies)
  if(!(focalSp %in% allSpecies)) stop(paste0("The focal species ", focalSp,
                                             " was not faound among the species names in the data set."))
  overFocal <- lapply(x$overlaid, FUN=function(x)return(x[x$scientificName==focalSp,]))

  yearsAll <- sort(unique(lubridate::year(x$temporal)))
  yearRng <- range(yearsAll)
  yearMax <- max(yearsAll)
  yearMin <- min(yearsAll)

  wNonEmptyFocal <- unname(which(unlist(lapply(overFocal, nrow))>0))
  nObs <- sum(unlist(lapply(overFocal[wNonEmptyFocal], nrow)))
  visitsFocal <- unname(unlist(lapply(overFocal[wNonEmptyFocal],
                                      FUN=function(x) x[, visitCol]) ))
  nVis <- length(visitsFocal)
  yearsFocal <- unname(unlist(lapply(overFocal[wNonEmptyFocal],
                              FUN=function(x) x[,"year"]) ))
  yearsFocalTbl <- table( factor(yearsFocal,
                             levels = yearsAll)
                        )

  monthsFocal <- unname(unlist(lapply(overFocal[wNonEmptyFocal],
                                      FUN=function(x) x[,"month"]) ))
  monthsFocalTbl <- table( factor(monthsFocal,
                                  levels=1:12,
                                  labels = month.abb[1:12])
                           )

  reportStrg <- paste0("Number of observations: ", nObs)

  layout(matrix(c(1,2,1,3), nrow = 2, byrow = long))
  par(mar=c(1,1,1,1))
  plot(x$spatial[wNonEmpty,], col = colVis, border = NA, ...)
  plot(x$spatial[wNonEmptyFocal,], col = colPres, border = NA, add=TRUE)
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
#' SB <- summarizeBirds(organizeBirds(bombusObs), grid=grid)
#' speciesSummary(SB)
#' }
#' @export
#' @seealso \code{\link{summarizeBirds}}, \code{\link{exportBirds}}
speciesSummary <- function(x){
  if (class(x) != "SummarizedBirds") {
    stop("The object 'x' must be of class SummarizedBirds.")
  }
  allSpecies <- listSpecies(x)
  res<-data.frame("species"=character(0),
                  "nCells"=numeric(0),
                  "nObs"=numeric(0),
                  "nVis"=numeric(0),
                  "visitsUID"=character(0),
                  # "years"=character(0),
                  # "months"=character(0),
                  "nYears"=numeric(0),
                  "nMonths"=numeric(0),
                  stringsAsFactors = FALSE)

  for(s in allSpecies){
    res<-rbind(res, focalSpSummary(x, focalSp = s))
    cat(s,"\n")
  }

  return(res)
}

#' Create a community matrix
#'
#' A function that counts the number of observations or visits per grid cell for all species.
#' @param x an object of class \sQuote{SummarizeBirds}.
#' @param sampleUnit an string specifying the sample unit within a grid cell.
#' Options are \dQuote{observation} (default) or \dQuote{visit}.
#' If spillOver=TRUE and visits are defined by locality, it may happen that some
#' species observations are counted in more than one grid cell.
#' @return a \code{matrix} with counts of observations or visits for each species on each non-empty grid cell.
#' @examples
#' grid <- makeGrid(searchPolygon, gridSize = 10)
#' SB <- summarizeBirds(organizeBirds(bombusObs), grid=grid)
#' CM <- communityMatrix(SB, sampleUnit="visit")
#' @export
#' @seealso \code{\link{summarizeBirds}}, \code{\link{exportBirds}}
communityMatrix<-function(x, sampleUnit="observation"){
  if (class(x) != "SummarizedBirds") {
    stop("The object 'x' must be of class SummarizedBirds.")
  }
  visitCol<-attr(x, "visitCol")

  allSpecies <- listSpecies(x)
  nCells<-length(x$spatial)
  cellID<-sapply(methods::slot(x$spatial, "polygons"), FUN=function(x) methods::slot(x, "ID"))
  wNonEmpty<-unname(which(unlist(lapply(x$overlaid, nrow))>0))
  res<-matrix(NA, nrow=nCells, ncol = length(allSpecies), dimnames = list(cellID, allSpecies))

  if (sampleUnit == "visit"){
    for(i in wNonEmpty){
      tmp.vis <- summarise(group_by(x$overlaid[[i]], scientificName, !!! rlang::syms(visitCol)))
      tmp <- rowSums(table(tmp.vis))
      wSp<- match( names(tmp), allSpecies)
      res[i, wSp] <- tmp
    }
  }
  if (sampleUnit == "observation"){
    for(i in wNonEmpty){
      tmp <- summarise(group_by(x$overlaid[[i]], scientificName), n=n())
      wSp<- match( tmp$scientificName, allSpecies)
      res[i, wSp] <- tmp$n
    }
  }
  return(res[wNonEmpty,])
}
