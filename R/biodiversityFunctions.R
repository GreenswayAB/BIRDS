#' Create a community matrix
#'
#' A function that counts the number of observations or visits per grid cell for
#' all species.
#' @param x an object of class \sQuote{SummarizeBirds}.
#' @param sampleUnit an string specifying the sample unit within a grid cell.
#' Options are \dQuote{observation} (default) or \dQuote{visit}.
#' If spillOver=TRUE and visits are defined by locality, it may happen that some
#' species observations are counted in more than one grid cell.
#' @return a \code{matrix} with counts of observations or visits for each species
#' on each non-empty grid cell.
#' @examples
#' \donttest{
#' grid <- makeGrid(searchPolygon, gridSize = 10)
#' SB <- summarizeBirds(organizeBirds(bombusObsShort), grid=grid)
#' CM <- communityMatrix(SB, sampleUnit="visit")
#' }
#' @export
#' @importFrom rlang .data
#' @seealso \code{\link{summarizeBirds}}, \code{\link{exportBirds}}
communityMatrix<-function(x,
                          sampleUnit="observation"){
  if (class(x) != "SummarizedBirds") {
    stop("The object 'x' must be of class SummarizedBirds.")
  }
  visitCol<-attr(x, "visitCol")

  allSpecies <- listSpecies(x)
  nCells <- length(x$spatial)
  cellID <- sapply(slot(x$spatial, "polygons"),
                   FUN = function(x) slot(x, "ID"))
  wNonEmpty <- unname(which(unlist(lapply(x$overlaid, nrow)) > 0))

  res <- matrix(NA,
                nrow=nCells,
                ncol = length(allSpecies),
                dimnames = list(cellID,
                                allSpecies))

  if (sampleUnit == "visit"){
    for(i in wNonEmpty){
      tmp.vis <- group_by(x$overlaid[[i]],
                          .data$scientificName,
                          !!sym(visitCol)) %>%
        summarise()
      tmp <- rowSums(table(tmp.vis))
      wSp <- match( names(tmp), allSpecies)
      res[i, wSp] <- tmp
    }
  }
  if (sampleUnit == "observation"){
    for(i in wNonEmpty){
      tmp <- group_by(x$overlaid[[i]],
                      .data$scientificName) %>%
        summarise(n=n())
      wSp <- match( tmp$scientificName, allSpecies)
      res[i, wSp] <- tmp$n
    }
  }
  return(res[wNonEmpty,])
}


#' Observations by Species or records by species matrix
#'
#' This function prepares the organised data into a "records-by-species" matrix
#' summarizing the number of observations (records)  per species in the shape
#' required by the functions from the KnowBR package for calculating 'completeness'.
#'
#' @references Lobo et al. (2018) <https://doi.org/10.1016/j.ecolind.2018.03.077>
#' @param x an object of class \sQuote{OrganizedBirds} (organised BIRDS Spatial Dataframe).
#' @param format the type of matrix to return: type 'A' where each row is a
#' combination of species, location (unique coordinates or visits' ID) and count,
#' or 'B' where each row is a location, each column a species, and the elements
#' of the matrix the counts
#' @param location the uniqueness of the position given by the coordinates or the visits' ID
#' @return a \code{data.frame} with a record-by-species matrix
#' @export
#' @importFrom tidyr pivot_wider
#' @seealso \code{\link{organizeBirds}}
recBySpp <- function(x, format="A", location="coordinates"){
  if (class(x) != "OrganizedBirds") {
    stop("The object 'x' must be of class OrganizedBirds. See the function 'organizedBirds()'.")
  }

  if (is.null(location) | !(location %in% c("coordinates", "visit"))){
    stop("The argument 'location' needs to be one of 'coordinates' or 'visit'")
  }
  if(location == "visit") location <- "visitUID"

  format <- tolower(format)
  if(is.null(format) | !(format %in% c("a","b"))){
    stop("The argument 'format' needs to be one of 'a' or 'b'")
  }

  ## paste coordinates
  coord <- coordinates(x$spdf)
  obCoor <- cbind(coord,
                  apply(coord , 1 , paste , collapse = "-" ),
                  slot(x$spdf, "data"))
  colnames(obCoor)[3] <- "coordinates"

  ### typeA
  resA <- obCoor %>%
    group_by(.data$scientificName, !!sym(location)) %>%
    summarise("count"=n())
  if(format == "a"){
    return(as.data.frame(resA))
  }else{
    ### typeB
    resB <- resA %>%
      pivot_wider(names_from = .data$scientificName,
                  values_from = .data$count)
    return(as.data.frame(resB))
  }

}



##### COMMUNITY MATRIX PER GRID CELL
OB <- organizeBirds(bombusObs, sppCol = "scientificName",
                    taxonRankCol = "taxonRank", taxonRank = "SPECIES",
                    simplifySppName = TRUE)
grid <- makeGrid(searchPolygon, gridSize = 10)

RxS <- recBySpp(OB)
KnowBPolygon(RxS, format="A", grid)

SB <- summariseBirds(OB, grid=grid)

x <- SB$overlaid[["ID997"]]
specMat <- x %>%
  group_by(visitUID,
           "scientificName"=factor(x$scientificName, levels=listSpecies(SB)),
           .drop = FALSE) %>%
  summarise("count"=n()) %>%
  tidyr::pivot_wider(names_from = .data$scientificName,
                     values_from = .data$count,
                     values_fill = list(count = 0))

specaccum(as.data.frame(specMat))


#' Completeness analysis
#'
#' Lorem ipsum
#'
#' @references La Sorte & Somveille (2019) < https://doi.org/10.1111/ecog.04632>
#' @param nObs an object of any class (mainly resulting from \code{summariseBirds} or
#' \code{exportBirds} with  the number of observations, or visits in your desired analysis unit.
#' @return a \code{data.frame} with ignorance scores
#' @examples
#' \donttest{
#' OB <- organizeBirds(bombusObsShort, sppCol = "scientificName", simplifySppName = TRUE)
#' grid <- makeGrid(searchPolygon, gridSize = 10)
#' SB <- summariseBirds(OB, grid=grid)
#' ignorance <- exposeIgnorance(nObs=SB$spatial@data$nObs)
#' }
#' @export
#' @seealso \code{\link{summarizeBirds}}, \code{\link{exportBirds}}
completeness <- function(nObs){


}
