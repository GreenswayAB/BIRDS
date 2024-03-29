#' Which cells are not empty
#'
#' This function returns the ID of those cells that have some data. It is used to
#' speed up other functions
#'
#' @param x A list.
#' @return The a \code{vector} with list elements IDs.
#' @export
whichNonEmpty <- function(x){
  # res <- which( lengths(x) > 1)
  # res <- unname(which(unlist(!is.null(lapply(x, nrow) > 0)))
  res <-  which(
            unlist(
              lapply(x,
                     function(y){
                        !all(is.na(y))
                     }
              )
            )
          )

  return(res)
}

#' Create spillover overlay for specific grid
#'
#' An internal function. Takes the resulting data.frame from the spatial overlay
#' for a specific grid cell and searches for spillover visits, i.e. visits that are split by grid cell boundaries.
#'
#' @param x A data.frame for a specific grid without spillover.
#' @param birdData An OrganizedBirds object, which is the input to the \code{\link{overlayBirds}}-function.
#' @param visitCol A character string specifying the columns that identify a visit.
#'
#' @return The overlay data.frame for a specific grid, including the spillover visits.
#' @keywords internal
createOverlayForGrid <- function(x, birdData, visitCol){
  visitsInGrid <- unique(x[visitCol])

  birdDF <- st_drop_geometry(birdData$spdf)
  res <- birdDF[which(birdDF[,visitCol] %in% visitsInGrid[,1]),]
  return(res)
}



#' Include spillover observations
#'
#' An internal function to include spillover observations to the gridcells.
#' This function duplicates observations across gridcells
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

#' Include unique spillover observations
#'
#' An internal function to include spillover observations to the gridcells.
#' This function moves spillover observations to the gridcells were the focal visit has the most observations (or at random if ties)
#'
#' @param birdData An OrganizedBirds object, which is the input to the \code{\link{overlayBirds}}-function.
#' @param grid A sf or SpatialPolygonsDataFrame object of the grid over the study area
#' @param visitCol A character string for specifying the columns that identify a visit.
#'
#' @return A  ObservationsInGrid list
#' @importFrom nnet which.is.max
#' @keywords internal
includeUniqueSpillover <- function(birdData, grid, visitCol){
  if (!inherits(birdData, c("sf","OrganizedBirds","SpatialPointsDataFrame")))
    stop("Data must be of class 'OrganizedBirds', 'sf' or 'SpatialPointsDataFrame'")
  if (inherits(birdData, "OrganizedBirds")) birdData <- birdData$spdf
  if (inherits(birdData, "SpatialPointsDataFrame")) birdData <- st_as_sf(birdData)

  if (!identical(st_crs(birdData), st_crs(grid))) {
    stop("Organized data and grid do not share the same CRS")
  }

  if (inherits(grid, "sfc")) grid <- st_as_sf(grid)

  ## create ids
  if (length(grep("id", colnames(grid), ignore.case = TRUE)) == 0) {
    grid$id <- seq(nrow(grid))
  } else {
    grid$id <- st_drop_geometry(grid[, grep("id", colnames(grid),
                                            ignore.case = TRUE)[1]])
  }

  obs <- st_drop_geometry(
            suppressWarnings(
              suppressMessages(
                st_intersection(birdData, grid)
              )
            )
          )

  wNA <- which(is.na(obs$id))
  if(length(wNA)>0){
    obs <- obs[-wNA,]
  }

  crossTab <- table("grid" = obs[,"id"],
                    "visits" = obs[,visitCol])

  visits <- unique(obs[, visitCol])
  visits <- cbind(visits, "grid" = NA)

  for(v in dimnames(crossTab)$visits){
    visits[visits[, "visits"] == as.integer(v), "grid"] <- as.integer(dimnames(crossTab)$grid[nnet::which.is.max(crossTab[,v])])
    ##nnet::which.is.max is good since if it's equal number it takes one on random.
  }

  wNAvis <- which(is.na(visits[,"grid"]))
  if(length(wNAvis)>0){
    visits <- visits[-wNAvis,]
  }

  colsExc <- which(colnames(obs) == "id")

  res <- list()
  for(g in seq(nrow(grid))){
    res[[g]] <- obs[obs[, visitCol] %in% unique(visits[visits[, "grid"] == g, "visits"]), -colsExc]
  }

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
#'
#' If \code{spillOver = NULL} the splitting is done spatially according to the
#' overlay of observations and grid cells, without further consideration of
#' coherence for visits (visit UID). If \code{spillOver = c("unique", "duplicate")}
#' the splitting will be done spatially in a first step, and then:
#' if \code{(spillOver = "unique")} assigns (and moves) all observations with same visitUID
#' to the grid cell with most observations (or picks one grid cell at random if
#' there is a tie); or if \code{(spillOver = "duplicate")} duplicates all
#' observations with same visitUID across all grid cells containing at least one
#' observation with that visitUID.
#'
#' The later approach is useful when the amount of observations spilled over
#' neighbouring cells is minimal and information over the spatial extent of the
#' sampling effort is more important than sample independence.
#'
#' @param x An OrganizedBirds object
#' @param grid A sf or SpatialPolygonsDataFrame object of the grid over the study area
#' @param spillOver Specifies if the function should search for observations
#' with the same visitUID over several grid cell, and what to do with them.
#' It accepts \code{c(NULL, "unique", "duplicate")}. Default NULL.
#' @param cleanGrid logical. Whether to remove all data from the grid.
#'
#' @return The output is a OverlaidBirds-class object, which is a list
#'   containing three objects;
#'   \describe{
#'     \item{\code{observationsInGrid}}{Is
#'       basically the data in the OrganizedBirds object split by each grid cell
#'       (\emph{n.b.} the use of \code{spillOver} discussed under "Usage")}
#'     \item{\code{grid}}{The sf from the input, optionally cleared of data }
#'    \item{\code{nonEmptyGridCells}}{An integer vector of which grid cells that have observations}
#'   }
#' @export
#' @examples
#' \donttest{
#' ob <- organizeBirds(bombusObs)
#' grid <- makeGrid(gotaland, 10)
#' ovB <- overlayBirds(ob, grid) 
#' }
overlayBirds <- function(x,
                         grid,
                         spillOver = NULL,
                         cleanGrid = TRUE){
  if(!is.null(spillOver)){
    ### Bad spill over definition
    if (!spillOver %in% c("unique", "duplicate") ) stop("Unknown definition of 'spillOver'")
  } #else {  stop("Unknown definition of 'spillOver'") }
  UseMethod("overlayBirds")
}

##TODO make a overlayBirds.default

#' @export
#' @rdname overlayBirds
overlayBirds.OrganizedBirds <- function(x,
                                        grid,
                                        spillOver = NULL,
                                        cleanGrid = TRUE){
  spBird <- x$spdf

  if (inherits(spBird, "SpatialPointsDataFrame")) {
    spBird <- st_as_sf(spBird)
  }

  if (inherits(grid, c("SpatialPolygonsDataFrame", "SpatialPolygons"))) {
    grid <- st_as_sf(grid)
  }

  if (cleanGrid) {
    grid <- st_geometry(grid) ##Removes unnecessary data from the input grid, if there is any
  }

  visitCol <- attr(x, "visitCol")
  nVis <- length(unique(st_drop_geometry(spBird)[,visitCol]))
  nObs <- nrow(spBird)

  if (!identical(st_crs(spBird), st_crs(grid))) {
    grid <- st_transform(grid,
                         crs = st_crs(spBird))
  }

  #### Rename grid
  if (any(duplicated(names(grid)))) {
    grid <- renameGrid(grid)
    warning("There are duplicated cell names in your grid. We rename them internally to 'ID1'...'IDn'.
All results will use this nomenclature, but the order of the cells will remain unaltered.")
  }

  #### SPILL OVER
  ### Generic overlay
  ## overlay the data with the grid
  listGrid <- suppressMessages(st_intersects(grid, spBird))

  ObsInGridList <- list()

  for (i in seq(length(listGrid))) {
    if (length(listGrid[[i]]) == 0) {
      ObsInGridList[[i]] <- NA
    } else {
      ObsInGridList[[i]] <- st_drop_geometry(spBird[listGrid[[i]],])
    }
  }
  wNonEmpty <- whichNonEmpty(ObsInGridList)

  if (length(wNonEmpty) == 0) stop("Observations don't overlap any grid cell.")
  ### Check nObs
  nObsInGrid <- sum(unlist(lapply(ObsInGridList, nrow)))

  if ((nObs - nObsInGrid) > 0) {
    message(paste((nObs - nObsInGrid), "observations did not overlap with the grid and will be discarded."))
  }

  visitsIDGrid <- lapply(ObsInGridList[wNonEmpty], function(x) unique(x[,visitCol]))
  nSpill <- sum(duplicated(unlist(visitsIDGrid)))

  if (nSpill > 0) {
    porcSpill <- round(nSpill/nVis*100,3)
    message(paste(porcSpill, "% of the visits spill over neighbouring grid cells."))
    if (porcSpill >= 20) {
      message("20% or more of the visits spill observations over other grid cells.
This may suggest that either your grid cell is too narrow or your definition of a 'visit' is not properly defined to match your grid cell size.

Please, consider using 'exploreVisits()' to double check your assumptions.")
    }
  }

  if (!is.null(spillOver)) {
  #   if (!spillOver %in% c("unique", "duplicate") ) stop("Unknown definition of 'spillOver'")
    ### Good definition
    if (spillOver == "unique") { ### UNIQUE SPILL OVER
      ObsInGridList <- includeUniqueSpillover(spBird, grid, visitCol)
      wNonEmpty <- whichNonEmpty(ObsInGridList)

    }
    if (spillOver == "duplicate") {   ### DUPLICATE SPILL OVER
      ObsInGridList[wNonEmpty] <- includeSpillover(ObsInGridList[wNonEmpty], x, visitCol)
    }
  } #else {stop("Unknown definition of 'spillOver'")}

  res <- list("observationsInGrid" = ObsInGridList,
              "grid" = grid,
              "nonEmptyGridCells" = wNonEmpty)

  attr(res, "visitCol") <- visitCol ##An attribute to indicate which of the visits-column should be used in further analyses
  attr(res, "spillOver") <- spillOver ##An attribute to indicate how spillover observations are included

  return(res)

}


