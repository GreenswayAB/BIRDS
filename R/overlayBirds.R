#' Create spillover overlay for specific grid
#'
#' An internal function. Takes the resulting dataframe from the spatial overlay
#' for a specific grid cell and searches for spillover visits, i.e. visits that are split by grid cell boundaries.
#'
#' @param x A dataframe for a specific grid without spillover.
#' @param birdData An OrganizedBirds object, which is the input to the \code{\link{overlayBirds}}-function.
#' @param visitCol A character string specifying the columns that identify a visit.
#'
#' @return The overlay dataframe for a specific grid, including the spillover visits.
#' @keywords internal
createOverlayForGrid <- function(x, birdData, visitCol){
  visitsInGrid <- unique(x[visitCol])
  birdDF <- birdData[[1]]@data
  res <- birdDF[which(birdDF[,visitCol] %in% visitsInGrid[,1]),]
  return(res)
}



#' Include spillover observations
#'
#' An internal function to include spillover observations to the gridcells.
#'
#' @param x An observationsInGrid list from \code{\link{overlayBirds}}. This
#'   is also what will be modified and become the output.
#' @param birdData An OrganizedBirds object, which is the input to the \code{\link{overlayBirds}}-function.
#' @param visitCol A character string for specifying the columns that identify a visit.
#'
#' @return A modified observationsInGrid list
#' @keywords internal
includeSpillover <- function(x, birdData, visitCol){
  res <- lapply(x, createOverlayForGrid, birdData, visitCol)
  return(res)
}

#' Overlay BIRDS object and grid
#'
#' Make an overlay for an OrganizedBirds object and a grid to identify which
#' observations that fall into which grid cell.
#'
#' This function takes an OrganizedBirds object, created by
#' \code{\link{organizeBirds}}, and a polygon grid-layer, which could be created
#' by \code{\link{makeGrid}} and splits the visits in the OrganizedBirds
#' (i.e. data belonging to identified visits) to each grid cell.
#' If \code{spillOver = FALSE} the splitting is done spatially according to the overlay of observations
#' and grid cells, without further consideration of coherence for visits (visit UID).
#' If \code{spillOver = TRUE} the splitting will be done spatially in a first step,
#' but in the next step for each grid cell the function searches for other observations belonging
#' to the visits inside the gridcell (identified by visit UID) in the entire dataset and includes
#' all such observations found to the observations in the grid cell (i.e. keeping visits coherent).
#'
#' @param x An OrganizedBirds object
#' @param grid A SpatialPolygonsDataFrame object of the grid in the study area
#' @param spillOver Specifies if the function should search for observations belonging to the same
#'   visit (identified by visit UID) but fall outside of the grid cell. Default is \code{TRUE}.
#'
#' @return The output is a OverlaidBirds-class object, which is a list
#'   containing three objects;
#'   \describe{
#'     \item{\code{observationsInGrid}}{Is
#'       basically the data in the OrganizedBirds object split by each grid cell
#'       (\emph{n.b.} the use of \code{spillOver = TRUE} discussed under "Usage")}
#'     \item{\code{grid}}{The SpatialPolygonsDataFrame from the input, but cleared
#'       of data to not waste unnecessary memory}
#'    \item{\code{nonEmptyGridCells}}{An integer vector of which grid cells that have observations}
#'   }
#' @export
#' @rdname overlayBirds
#' @examples ob<-organizeBirds(bombusObs)
#' grid <- makeGrid(gotaland, gridSize = 10)
#' overlayBirds(ob, grid)
overlayBirds <- function(x, grid, spillOver = TRUE){
  UseMethod("overlayBirds")
}

##TODO make a overlayBirds.default

#' Make an overlay for an OrganizedBirds object and a grid to identify which
#' observations that fall into which grid cell.
#'
#' This function takes an OrganizedBirds object, created by
#' \code{\link{organizeBirds}}, and a polygon grid-layer, which could be created
#' by \code{\link{makeGrid}} and splits the visits in the OrganizedBirds
#' (i.e. data belonging to identified visits) to each grid cell.
#' If \code{spillOver = FALSE} the splitting is done spatially according to the overlay of observations
#' and grid cells, without further consideration of coherence for visits (visit UID).
#' If \code{spillOver = TRUE} the splitting will be done spatially in a first step,
#' but in the next step for each grid cell the function searches for other observations belonging
#' to the visits inside the gridcell (identified by visit UID) in the entire dataset and includes
#' all such observations found to the observations in the grid cell (i.e. keeping visits coherent).
#'
#' @return The output is a OverlaidBirds-class object, which is a list
#'   containing three objects;
#'   \describe{
#'     \item{\code{observationsInGrid}}{Is
#'       basically the data in the OrganizedBirds object split by each grid cell
#'       (\emph{n.b.} the use of \code{spillOver = TRUE} discussed under "Usage")}
#'     \item{\code{grid}}{The SpatialPolygonsDataFrame from the input, but cleared
#'       of data to not waste unnecessary memory}
#'    \item{\code{nonEmptyGridCells}}{An integer vector of which grid cells that have observations}
#'   }
#' @export
#' @rdname overlayBirds
#' @examples ob<-organizeBirds(bombusObs)
#' grid <- makeGrid(gotaland, gridSize = 10)
#' overlayBirds(ob, grid)
overlayBirds.OrganizedBirds<-function(x, grid, spillOver = TRUE){

  spBird<-x$spdf

  visitCol<-attr(x, "visitCol")

  if(!spBird@proj4string@projargs==grid@proj4string@projargs){
        grid <- sp::spTransform(grid, sp::CRS(spBird@proj4string@projargs))
  }

  #### Rename grid
  if (any(duplicated(names(grid)))){
    grid <- renameGrid(grid)
    warning("There are duplicated cell names in your grid. We rename them internally to 'ID1'...'IDn'.
    All results will use this nomenclature, but the order of the cells will remain unaltered.")
  }

  ObsInGridList <- sp::over(grid, spBird, returnList=TRUE)
  wNonEmpty <- unname( which( unlist(lapply(ObsInGridList, nrow)) != 0) )

  if(spillOver){
    tmp.spill <- includeSpillover(ObsInGridList[wNonEmpty], x, visitCol)
    ObsInGridList[wNonEmpty] <- tmp.spill

    wSpill <- unname(which( unlist(lapply(tmp.spill, nrow)) != unlist(lapply(ObsInGridList[wNonEmpty], nrow)) ))
    diffSpill <- unname(unlist(lapply(tmp.spill, nrow)) - unlist(lapply(ObsInGridList[wNonEmpty], nrow)))[wSpill]
    porcSpill <- diffSpill / unname(unlist(lapply(ObsInGridList[wNonEmpty], nrow)))[wSpill]

    ### Warning for too many spill over, may be a bad definition of visit
    nBigSpill <- length(porcSpill>=.5)

    if (nBigSpill >= length(wNonEmpty) * .1) {
      warning("When more than 50% of the observations on a grid cell spill over other grid cells, it is considered as a big spill over.
More than 10% of your grid cells with data have big spill overs. This may suggest that either your grid cell is too narrow
or your definition of a 'visit' is not properly defined to match your grid cell size.

Please, consider using 'exploreVisits()' to double check your assumptions.")
    }
    rm(tmp.spill, wSpill, diffSpill, porcSpill, nBigSpill)
  }

  if (class(grid) == "SpatialPolygonsDataFrame"){

    grid@data <- grid@data[,-c(1:ncol(grid@data))] ##Removes unnecessary attribut data from the input grid, if there is any

  }


  res <- list("observationsInGrid" = ObsInGridList, "grid"= grid, "nonEmptyGridCells" = wNonEmpty)

  attr(res, "visitCol") <- visitCol ##An attribute to indicate which of the visits-column should be used in further analyses
  attr(res, "spillOver") <- spillOver ##An attribute to indicate whether spillover observations should be included

  return(res)

}


