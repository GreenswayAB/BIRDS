#' Which cells are not empty
#'
#' This function returns the ID of those cells that have some data. It is used to
#' speed up other functions
#'
#' @param x A list.
#' @return The a \code{vector} with list elements IDs.
#' @keywords internal
whichNonEmpty <- function(x){
  res <- unname(which(unlist(lapply(x, nrow)) > 0))
  return(res)
}

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
  # birdDF <- birdData[[1]]@data
  birdDF <- slot(birdData[[1]], "data")
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
#' @param grid A SpatialPolygonsDataFrame object of the grid over the study area
#' @param visitCol A character string for specifying the columns that identify a visit.
#'
#' @return A  ObservationsInGrid list
#' @importFrom sp over identicalCRS SpatialPolygonsDataFrame SpatialPolygons CRS proj4string
#' @importFrom nnet which.is.max
#' @keywords internal
includeUniqueSpillover <- function(birdData, grid, visitCol){
  if(class(birdData)=="OrganizedBirds") birdData <- birdData$spdf
  if(class(birdData)=="SpatialPointsDataFrame") birdData <- birdData
  if(!(class(birdData) %in% c("OrganizedBirds","SpatialPointsDataFrame"))) stop("Data must be of class 'OrganizedBirds' or 'SpatialPointsDataFrame'")

    # obs <- birdData@data
  obs <- slot(birdData, "data")
  visits <- unique(obs[, visitCol])
  visits <- cbind(visits, "grid" = NA)

  if(identicalCRS(birdData, grid) != TRUE){
    stop("Organized data and grid do not share the same CRS")
  }

  ## rename grids id no have integers
  for(i in 1:length(grid)){
    slot(slot(grid, "polygons")[[i]], "ID") <- as.character(i)
  }

  #Extract the unique ID from the polygons in the spdf
  ids <- data.frame(
    matrix(
      unlist(lapply(slot(grid, "polygons"),
                    function(x){
                      slot(x,"ID")}
                    ))[slot(grid, "plotOrder")],
      dimnames = list(c(), c("id"))),
    stringsAsFactors = FALSE)

  rownames(ids) <- ids[,1] #Since SpatialPolygonsDataFrame() wants to match rownames with the IDs

  spP <- SpatialPolygons(slot(grid, "polygons"),
                         slot(grid, "plotOrder"),
                         # proj4string = CRS(proj4string(grid)))
                         proj4string = slot(grid,"proj4string"))

  #A new spdf with only unique IDs, should be possible to join up with attribute data later
  spdfIDs <- SpatialPolygonsDataFrame(spP, ids)

  obs$grid <- unlist(over(birdData, spdfIDs, returnList=FALSE))


  wNA <- which(is.na(obs$grid))
  if(length(wNA)>0){
    obs <- obs[-wNA,]
  }

  crossTab <- table("grid" = obs[,"grid"],
                    "visits" = obs[,visitCol])

  for(v in dimnames(crossTab)$visits){
    visits[visits[, "visits"] == as.integer(v), "grid"] <- as.integer(dimnames(crossTab)$grid[nnet::which.is.max(crossTab[,v])])
    ##nnet::which.is.max is good since if it's equal number it takes one on random.
  }

  wNAvis <- which(is.na(visits[,"grid"]))
  if(length(wNAvis)>0){
    visits <- visits[-wNAvis,]
  }

  colsExc <- which(colnames(obs) == "grid")

  res <- list()
  for(g in 1:length(grid)){
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
#' The later approach is usefull when the amount of observations spilled over
#' neighbouring cells is minimal and information over the spatial extent of the
#' sampling effort is more important than sample independence.
#'
#' @param x An OrganizedBirds object
#' @param grid A SpatialPolygonsDataFrame object of the grid over the study area
#' @param spillOver Specifies if the function should search for observations
#' with the same visitUID over several grid cell, and what to do with them.
#'  Default is \code{NULL}. It also accepts \code{c("unique", "duplicate")}.
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
#' @importFrom sp coordinates proj4string spTransform CRS over
#' @examples
#' ob <- organizeBirds(bombusObs)
#' grid <- makeGrid(gotaland, gridSize = 10)
#' ovB <- overlayBirds(ob, grid)
overlayBirds <- function(x, grid, spillOver = NULL){
  if(!is.null(spillOver)){
    ### Bad spill over definition
    if (!spillOver %in% c("unique", "duplicate") ) stop("Unknown definition of 'spillOver'")
  }
  UseMethod("overlayBirds")
}

##TODO make a overlayBirds.default

#' @export
#' @rdname overlayBirds
overlayBirds.OrganizedBirds<-function(x, grid, spillOver = NULL){
  spBird <- x$spdf

  visitCol <- attr(x, "visitCol")
  nVis <- length(unique(spBird@data[,visitCol]))
  nObs <- nrow(spBird)

  # if(!spBird@proj4string@projargs == grid@proj4string@projargs){
  #       grid <- spTransform(grid, CRS(spBird@proj4string@projargs))
  # }
  if(!identicalCRS(spBird, grid)){
    grid <- spTransform(grid, slot(spBird, "proj4string"))
  }

  #### Rename grid
  if (any(duplicated(names(grid)))){
    grid <- renameGrid(grid)
    warning("There are duplicated cell names in your grid. We rename them internally to 'ID1'...'IDn'.
All results will use this nomenclature, but the order of the cells will remain unaltered.")
  }

  #### SPILL OVER
  ### Generic overlay
  ObsInGridList <- over(grid, spBird, returnList=TRUE)
  # wNonEmpty <- unname( which( unlist(lapply(ObsInGridList, nrow)) != 0) )
  wNonEmpty <- whichNonEmpty(ObsInGridList)

  if(length(wNonEmpty)==0) stop("Observations don't overlap any grid cell.")
  ### Check nObs
  nObsInGrid <- sum(unlist(lapply(ObsInGridList, nrow)))

  if((nObs-nObsInGrid)>0){
    message(paste((nObs-nObsInGrid), "observations did not overlap with the grid and will be discarded."))
  }

  visitsIDGrid <- lapply(ObsInGridList[wNonEmpty], function(x) unique(x[,visitCol]))
  nSpill <- sum(duplicated(unlist(visitsIDGrid)))
  if(nSpill>0){
    porcSpill<-round(nSpill/nVis*100,3)
    message(paste(porcSpill, "% of the visits spill over neighbouring grid cells."))
    if (porcSpill >= 20) {
      message("20% or more of the visits spill observations over other grid cells.
This may suggest that either your grid cell is too narrow or your definition of a 'visit' is not properly defined to match your grid cell size.

Please, consider using 'exploreVisits()' to double check your assumptions.")
    }
  }


  if(!is.null(spillOver)){
    ### Good definition
    if(spillOver == "unique"){ ### UNIQUE SPILL OVER
      # This will replace the previously defined ObsInGridList
      ObsInGridList <- includeUniqueSpillover(spBird, grid, visitCol)
      # wNonEmpty <- unname( which( unlist(lapply(ObsInGridList, nrow)) != 0) )
      wNonEmpty <- whichNonEmpty(ObsInGridList)

    } else if(spillOver == "duplicate"){   ### DUPLICATE SPILL OVER
      ObsInGridList[wNonEmpty] <- includeSpillover(ObsInGridList[wNonEmpty], x, visitCol)
    }
  }

  if (class(grid) == "SpatialPolygonsDataFrame"){
    grid@data <- grid@data[,-c(1:ncol(grid@data))] ##Removes unnecessary attribute data from the input grid, if there is any
    # slot(grid, "data") <- grid@data[,-c(1:ncol(grid@data))] ##Removes unnecessary attribute data from the input grid, if there is any
  }

  res <- list("observationsInGrid" = ObsInGridList,
              "grid"= grid,
              "nonEmptyGridCells" = wNonEmpty)

  attr(res, "visitCol") <- visitCol ##An attribute to indicate which of the visits-column should be used in further analyses
  attr(res, "spillOver") <- spillOver ##An attribute to indicate how spillover observations are included

  return(res)

}


