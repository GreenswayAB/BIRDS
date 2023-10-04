#' Create a community matrix (grid cells x species)
#'
#' A function that counts the number of observations or visits per grid cell for
#' each species.
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
communityMatrix <- function(x, sampleUnit = "observation"){

  if (!inherits(x, "SummarizedBirds")) {
    stop("The object 'x' must be of class SummarizedBirds.")
  }
  visitCol <- attr(x, "visitCol")

  allSpecies <- listSpecies(x)
  nCells <- nrow(x$spatial)
  cellID <- rownames(x$spatial)
  wNonEmpty <- whichNonEmpty(x$overlaid)

  res <- matrix(NA,
                nrow = nCells,
                ncol = length(allSpecies),
                dimnames = list(cellID,
                                allSpecies))

  if (sampleUnit == "visit") {
    for (i in wNonEmpty) {
      tmp.vis <- group_by(x$overlaid[[i]],
                          .data$scientificName,
                          !!sym(visitCol)) %>%
        summarise()
      tmp <- rowSums(table(tmp.vis))
      wSp <- match( names(tmp), allSpecies)
      res[i, wSp] <- tmp
    }
  }
  if (sampleUnit == "observation") {
    for (i in wNonEmpty) {
      tmp <- group_by(x$overlaid[[i]],
                      .data$scientificName) %>%
        summarise(n = n())
      wSp <- match( tmp$scientificName, allSpecies)
      res[i, wSp] <- tmp$n
    }
  }
  return(res[wNonEmpty,])
}



#' Creates a community matrix per grid cell (sample x species x grid cell)
#'
#' A function that counts for each grid cell the number of observations per visit
#' for each species.
#' @param x an object of class \sQuote{SummarizeBirds}.
#' @return a \code{list} with a \code{data.frame} per grid cell with counts of
#' observations for each species per visits on each non-empty grid cell.
#' @examples
#' \donttest{
#' grid <- makeGrid(searchPolygon, gridSize = 10)
#' OB <- organizeBirds(bombusObs, sppCol = "scientificName",
#'                     taxonRankCol = "taxonRank", taxonRank = "SPECIES",
#'                     simplifySppName = TRUE)
#' SB <- summarizeBirds(OB, grid=grid)
#' CM <- communityMatrixGrid(SB)
#' lCM <- lengths(CM) ## Which cells are empty
#' ## library(vegan)
#' ## R <- lapply(CM[which(lCM>0)], specaccum)
#' }
#' @export
#' @importFrom rlang .data
#' @importFrom tidyr pivot_wider
#' @seealso \code{\link{communityMatrix}}
communityMatrixGrid <- function(x){

  if (!inherits(x, "SummarizedBirds")) {
    stop("The object 'x' must be of class SummarizedBirds.")
  }
  visitCol <- attr(x, "visitCol")
  overlaid <- x$overlaid
  allSpecies <- listSpecies(x)
  nCells <- nrow(x$spatial)
  cellID <- rownames(x$spatial)
  res <- vector("list",nCells)
  names(res) <- cellID

  # wNonEmpty <- wichNonEmpty(overlaid)
  wNonEmpty <- attr(x, "nonEmptyGridCells")

  comMatList <- lapply(wNonEmpty, function(x){
    specMat <- overlaid[[x]] %>%
      group_by(.data$visitUID,
               "scientificName" = factor(.data$scientificName, levels = allSpecies),
               .drop = FALSE) %>%
      summarise("count" = n()) %>%
      pivot_wider(names_from = .data$scientificName,
                         values_from = .data$count,
                         values_fill = list(count = 0))
    return(as.data.frame(specMat))
  })

  res[wNonEmpty] <- comMatList
  return(res)
}


#' Observations by species or records-by-species matrix
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
#' @examples
#' \donttest{
#' grid <- makeGrid(searchPolygon, gridSize = 10)
#' OB <- organizeBirds(bombusObsShort, sppCol = "scientificName",
#'                     taxonRankCol = "taxonRank", taxonRank = "SPECIES",
#'                     simplifySppName = TRUE)
#' RxS <- recBySpp(OB)
#' }
#' @export
#' @importFrom tidyr pivot_wider
#' @seealso \code{\link{communityMatrix}}
recBySpp <- function(x, format = "A", location = "coordinates") {
  if (!inherits(x, "OrganizedBirds")) {
    stop("The object 'x' must be of class OrganizedBirds. See the function 'organizedBirds()'.")
  }

  if (is.null(location) | !(location %in% c("coordinates", "visit"))) {
    stop("The argument 'location' needs to be one of 'coordinates' or 'visit'")
  }
  if (location == "visit") location <- "visitUID"

  format <- tolower(format)
  if (is.null(format) | !(format %in% c("a","b"))) {
    stop("The argument 'format' needs to be one of 'a' or 'b'")
  }

  ## paste coordinates
  coord <- st_coordinates(x$spdf)
  obCoor <- cbind(coord,
                  apply(coord , 1 , paste , collapse = "-" ),
                  st_drop_geometry(x$spdf))
  colnames(obCoor)[3] <- "coordinates"

  ### typeA
  resA <- obCoor %>%
    group_by(.data$scientificName, !!sym(location)) %>%
    summarise("count" = n())
  if (format == "a") {
    return(as.data.frame(resA))
  }else{
    ### typeB
    resB <- resA %>%
      pivot_wider(names_from = .data$scientificName,
                  values_from = .data$count)
    return(as.data.frame(resB))
  }

}


# #' Completeness analysis
# #'
# #' Lorem ipsum
# #'
# #' @references
# #' @param x a community matrix
# #' @return a \code{data.frame} with ignorance scores
# #' @examples
# #' \donttest{
# #' OB <- organizeBirds(bombusObsShort, sppCol = "scientificName", simplifySppName = TRUE)
# #' grid <- makeGrid(searchPolygon, gridSize = 10)
# #' SB <- summariseBirds(OB, grid=grid)
# #' }
# #' @export
# #' @seealso \code{\link{communityMatrix}}, \code{\link{communityMatrixGrid}}
# completeness <- function(x){
#   nSpp<-sum(colSums(x[-1])>0)
#   nObsSpp<-colSums(x[-1]>0)
#   a<-sum(nObsSpp==1)
#   b<-sum(nObsSpp==2)
#   SouzaBaenaComp <- nSpp / (nSpp + (a^2/2*b))
#   return(SouzaBaenaComp)
#
#
# }
#
#
# x <- c(541, 1463, 2445, 3438, 4437, 5401, 6392, 8304, 11904, 22261)
# compute Gini coefficient
# ineq::ineq(x)
# compute

