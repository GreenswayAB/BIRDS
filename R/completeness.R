#' Observations by Species or records by species matrix
#'
#' This function prepares the organised data into a "records-by-species" matrix
#' summarizing the number of observations (records)  per species in the shape
#' required by the functions from the KnowBR package for calculating 'completeness'.
#'
#' @references Lobo et al. (2018) <https://doi.org/10.1016/j.ecolind.2018.03.077>
#' @param x an object of class \sQuote{OrganizedBirds} (organised BIRDS Spatial Dataframe).
#' @param type the type of matrix to return: type 'A' where each row is a
#' combination of species, location (unique coordinates or visits' ID) and count,
#' or 'B' where each row is a location, each column a species, and the elements
#' of the matrix the counts
#' @param location the uniqueness of the position given by the coordinates or the visits' ID
#' @return a \code{data.frame} with a record-by-species matrix
#' @export
#' @importFrom tidyr pivot_wider
#' @seealso \code{\link{organizeBirds}}
recBySpp <- function(x, type="A", location="coordinates"){
  if (class(x) != "OrganizedBirds") {
    stop("The object 'x' must be of class OrganizedBirds. See the function 'organizedBirds()'.")
  }

  if (is.null(location) | !(location %in% c("coordinates", "visit"))){
    stop("The argument 'locaiton' needs to be one of 'coordinates' or 'visit'")
  }
  if(location == "visit") location <- "visitUID"

  type <- tolower(type)
  if(is.null(type) | !(type %in% c("a","b"))){
    stop("The argument 'type' needs to be one of 'a' or 'b'")
  }

  ## paste coordinates
  coord <- coordinates(x$spdf)
  obCoor <- cbind(coord,
                  apply(coord , 1 , paste , collapse = "-" ),
                  slot(x$spdf, "data"))
  colnames(obCoor)[3] <- "coordinates"

  ### typeA
  resA <- obCoor %>%
    group_by(scientificName, !!sym(location)) %>%
    summarise("count"=n())
  if(type="a"){
    return(as.data.frame(resA))
  }else{
    ### typeB
    resB <- resA %>%
      pivot_wider(names_from = scientificName,
                  values_from = count)
    return(as.dta.frame(resB))
  }

}





#' Completness analysis
#'
#' Lorem ipsum
#'
#' @references La Sorte & Somveille (2019) < https://doi.org/10.1111/ecog.04632>
#' @param nObs an object of any class (mainly resulting from \code{summariseBirds} or
#' \code{exportBirds} with  the number of observations, or visits in your desired analysis unit.
#' @param nSpp the number of unique species observed
#' @param h the half ignorance parameter value.
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
completness <- function(SB, nSpp=NULL, h=1){


}
