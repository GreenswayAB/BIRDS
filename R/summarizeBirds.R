### TEMPORAL PART ###

#' Create spillover overlay for specific grid
#'
#' An internal function. Takes the dataframe resulting from the spatial overlay
#' for a specific grid and searches for spillover visits (i.e. visits that are split by grid cell boundaries).
#'
#' @param data A dataframe for a specific grid without spillover.
#' @param birdData An OrganizedBirds object, which is the input to the \code{\link{overlayBirds}}-function.
#' @param visitCol A character string for specifying the columns that identify a visit.
#'
#' @return The overlay dataframe for a specific grid, including the spillover visits.
#' @keywords internal
getTemporal<-function(birdData, visitCol=NULL){
  if (length(birdData$observationsInGrid)>1) stop("The input should ALWAYS be a overlayBirds whith only one gridcell")

  data<-birdData$observationsInGrid[[1]] ##The input should ALWAYS be a overlayBirds whith only one gridcell.

  if (is.null(visitCol)){
    visitCol<-attr(birdData, "visitCol")
  }

  ts<-xts::xts(data[, ! names(data) %in% c("year", "month", "day")], order.by=as.Date(apply(data[, c("year", "month", "day")], 1, paste0, collapse="-")))

  tempData<-xts::apply.daily(ts, function(x){

    return(c("nObs" = length(x[,1]),
             "nVis" = length(unique(x[, visitCol])),
             "nSpp" = length(unique(x[, "scientificName"]))#,
             # "avgSll" = median(dplyr::summarise(dplyr::group_by(data, !!dplyr::sym(visitCol)), avgSLL=dplyr::n_distinct(scientificName))$avgSLL)
             ))

  })

  return(tempData)

}

### SPATIAL PART ###

#' @importFrom dplyr summarise n n_distinct
#' @keywords internal
getSpatial<-function(birdDataOL){

  dataList<-birdDataOL$observationsInGrid

  nCells<-length(dataList)

  visitCol<-attr(birdDataOL, "visitCol")

  #res<-matrix(nrow = length(dataList), ncol = 6, dimnames = list(c(), c("nObs", "nVis", "nSpp", "avgSll", "nYears", "visitsUID")))

  res<-data.frame("nObs"=as.numeric(rep(NA,nCells)),
                  "nVis"=as.numeric(rep(NA,nCells)),
                  "nSpp"=as.numeric(rep(NA,nCells)),
                  "avgSll"=as.numeric(rep(NA,nCells)),
                  "nDays"=as.numeric(rep(NA,nCells)),
                  "nYears"=as.numeric(rep(NA,nCells)),
                  "visitsUID"=as.character(rep(NA,nCells)),
                  stringsAsFactors = FALSE)

  cols2use<-c("scientificName", "year", "month", "day", visitCol)

  dataRes<-lapply(dataList[birdDataOL$nonEmptyGridCells], function(x){

    x<-x[,cols2use]
    colnames(x)<-c("scientificName", "year", "month", "day", "visitCol")

      return(c("nObs"=length(x[,"scientificName"]),
               "nVis"=length(unique(x[,"visitCol"])),
               "nSpp"=length(unique(x[,"scientificName"])),
               "avgSll"=median(summarise(group_by(x, visitCol), avgSLL=n_distinct(scientificName))$avgSLL),
               "nDays"=length(unique( paste0(x[,"year"],"-", as.numeric(x[,"month"]), "-", as.numeric(x[,"day"]))) ),
               "nYears"=length(unique(x[,"year"])),
               "visitsUID"= paste0(unique(x[,"visitCol"]), collapse = ",")
               ))
      })

  dataRes<-data.frame(matrix(unlist(dataRes), nrow=length(dataRes), byrow=TRUE), stringsAsFactors = FALSE)

  dataRes$X1<-as.numeric(dataRes$X1)
  dataRes$X2<-as.numeric(dataRes$X2)
  dataRes$X3<-as.numeric(dataRes$X3)
  dataRes$X4<-as.numeric(dataRes$X4)
  dataRes$X5<-as.numeric(dataRes$X5)
  dataRes$X6<-as.numeric(dataRes$X6)

  res[birdDataOL$nonEmptyGridCells,]<-dataRes

  rownames(res)<-row.names(birdDataOL$grid)

  resSp<-sp::SpatialPolygonsDataFrame(birdDataOL$grid, res)

  return(resSp)

}

### SPATIOTEMPORAL PART ###

## Auxiliary function for getSpatioTemporal()
#' @importFrom dplyr group_by summarise n n_distinct
#' @keywords internal
countsYearMonth<-function(x, yearsAll, visitCol){
  ## Yearly Monthly

  vars<-c("nObs", "nVis", "nSpp", "avgSll", "nDays")

  resYM<-array(0, dim = c(length(yearsAll), 13, length(vars)),
             dimnames = list(as.character(yearsAll), c(month.abb,"Year"), vars))
  resYMvisits<-array(NA, dim = c(length(yearsAll), 13),
               dimnames = list(as.character(yearsAll), c(month.abb,"Year")))

  xGBYM <- group_by(x, year, month)
  tmp <- summarise(xGBYM,
                          nObs=n(),
                          nVis=n_distinct(!!! rlang::syms(visitCol)),
                          nSpp=n_distinct(scientificName),
                          avgSll=NA,
                          nDays=n_distinct(day),
                          visitUID=paste0(unique(!!! rlang::syms(visitCol)), collapse = ",")) ## a string with all cell_visit names

  wYears<-sort(match( tmp$year, yearsAll))
  wMonths<-tmp$month

  xGBYMV <- group_by(x, year, month, !!! rlang::syms(visitCol))
  tmpSLL <- summarise(xGBYMV, SLL=n_distinct(scientificName))
  tmp$avgSll <- (summarise(tmpSLL, avgSLL=median(SLL)))$avgSLL

  for(i in 1:nrow(tmp)){
    resYM[wYears[i], wMonths[i], ] <- as.numeric(tmp[i, (1:length(vars))+2 ])
    resYMvisits[wYears[i], wMonths[i]] <- tmp$visitUID[i]
  }

  ### Only Yearly
  if(is.null(nrow(resYM[,1:12,"nObs"]))){
    resYM[,13,"nObs"] <- sum(resYM[,1:12,"nObs"], na.rm = TRUE)
    resYM[,13,"nVis"] <- sum(resYM[,1:12,"nVis"], na.rm = TRUE)
    resYM[,13,"nDays"] <- sum(resYM[,1:12,"nDays"], na.rm = TRUE)
  }else{
    resYM[,13,"nObs"] <- rowSums(resYM[,1:12,"nObs"], na.rm = TRUE)
    resYM[,13,"nVis"] <- rowSums(resYM[,1:12,"nVis"] , na.rm = TRUE)
    resYM[,13,"nDays"] <- rowSums(resYM[,1:12,"nDays"] , na.rm = TRUE)
  }

  xGBY <- group_by(x, year)

  tmp <- summarise(xGBY, nSpp=n_distinct(scientificName)  )
  wYears<-sort(match( tmp$year, yearsAll))
  resYM[wYears, 13, "nSpp"] <- as.matrix(tmp)[, 2]

  xGBYV <- group_by(x, year, !!! rlang::syms(visitCol))
  tmpSLL <- summarise(xGBYV, SLL=n_distinct(scientificName))
  tmp <- summarise(tmpSLL, avgSLL=median(SLL))

  resYM[wYears, 13, "avgSll"] <- tmp$avgSLL

  if(is.null(nrow(resYMvisits[, 1:12]))){
    resYMvisits[, 13] <- paste0(na.omit(resYMvisits[, 1:12]), collapse = ",")
  }else{
    resYMvisits[, 13] <- apply(resYMvisits[, 1:12] ,1, FUN=function(x) paste0(na.omit(x), collapse = ",") )
  }

  return(list("resYM"=resYM, "resYMvisits"=resYMvisits))
}

### Actual function
#' @importFrom dplyr summarise group_by
#' @keywords internal
getSpatioTemporal<-function(birdOverlay, visitCol=NULL){

  dataList<-birdOverlay$observationsInGrid

  if (is.null(visitCol)){
    visitCol<-attr(birdOverlay, "visitCol")
  }
  if (!(visitCol %in% colnames(dataList[[1]]))) stop(paste("There is not column called", visitCol, "in your organised dataset."))

  sppAll<-sort(as.character(
    unique(unlist(lapply(dataList, FUN=function(x)return(x$scientificName))))
    ))

  yearRng<-range(unname(unlist(lapply(dataList, FUN=function(x)return(x$year)) )))
  yearMax<-yearRng[2] ## Take from birdTemporal
  yearMin<-yearRng[1] ## Take from birdTemporal
  yearsAll<-seq(yearMin, yearMax)

  wNonEmpty <- birdOverlay$nonEmptyGridCells
  gridLength <- length(dataList)

  visitsAll<-sort(as.character(
    unique(unlist(lapply(dataList, FUN=function(x)return(x[, visitCol]))))
  ))

  # Variables to summarize Number of observations, Number of visits, Number of unique Species, Average species list length.
  vars<-c("nObs", "nVis", "nSpp", "avgSll", "nDays")

  resYM<-array(dim = c(gridLength, length(yearsAll), 13, length(vars)),
             dimnames = list(names(dataList), as.character(yearsAll), c(month.abb, "Yearly"), vars))
  resYMvisits<-array(dim = c(gridLength, length(yearsAll), 13),
               dimnames = list(names(dataList), as.character(yearsAll), c(month.abb, "Yearly")))

  for(g in wNonEmpty){
    x<-dataList[[g]]
    # cellName<-gsub("ID", "", names(dataList[wNonEmpty[i]]))
    tmpYM<-countsYearMonth(x, yearsAll, visitCol)
    resYM[g,,,]  <- tmpYM$resYM
    resYMvisits[g,,]  <- tmpYM$resYMvisits
  }

  return(list("resYM"=resYM, "resYMvisits"=resYMvisits))
}


### THE SUMMARY PART ###

#' Summarize the OrganizedBirds
#'
#' Takes a OrganizedBirds-object and a SpatialPolygons*-grid and summarizes it
#' in spatial and  temporal dimensions.
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
#' @param x An OrgnanizedBirds-object created by \code{\link{organizeBirds}}
#' @param grid A SpatialPolygons or SpatialPolygonsDataFrame-object whith
#'   grid cells for the study area. This variable is optional and can be set to
#'   NULL, which will treat the area for all observations as one single
#'   grid cell.
#' @param spillOver Specifies if the function should search for observations done during
#'   the same visit (identified by visit UID) but that fall outside the grid cell.
#'   Default is \code{NULL}. It also accepts \code{c("unique", "duplicate")}.
#'   See Details for more information on how to use this.
#' @return A SummarizedBirds-object
#' @export
#' @examples ob<-organizeBirds(bombusObs)
#' grid <- makeGrid(gotaland, gridSize = 10)
#' SB <- summarizeBirds(ob, grid)
#' nObsG<-rowSums(SB$spatioTemporal[,,13,"nObs"], na.rm = FALSE)
#' nObsG2<-SB$spatial@data$nObs
#' any(nObsG != nObsG2, na.rm = TRUE) ## Check, two ways to obtain the same
summarizeBirds<-function(x, grid, spillOver = NULL){
  UseMethod("summarizeBirds")
}

##TODO make a summarizeBirds.default

##TODO include an exclude variable in summarizeBirds that remove different parts of the results to save up memory?

#' @rdname summarizeBirds
#' @export
summarizeBirds.OrganizedBirds<-function(x, grid, spillOver = NULL){

  visitCol<-attr(x, "visitCol")

  useSpatial<-FALSE #If the function should (can) export spatial data. Changes if grid is a SpatialPolygonDF

  #The variables to export!
  temporal<-NULL
  spatial<-NULL
  spatioTemporal<-NULL
  visits<-NULL
  # speciesLists<-NULL
  #The grid is exported in the spatial variable

  if(!is.null(grid)){
    if(class(grid) %in% c("SpatialPolygonsDataFrame", "SpatialPolygons")){
      bOver <- overlayBirds(x, grid=grid, spillOver = spillOver) # To use in the spatial analysis

      areaGrid <- rgeos::gUnaryUnion(grid)
      suppressMessages(bTOver <- overlayBirds(x, grid=areaGrid, spillOver = spillOver)) #To use in the temporal analysis where the entire area could be interpreted as a single grid cell

      useSpatial <- TRUE
    }else{
      stop("The variable grid can only be of class SpatialPolygonsDataFrame, or NULL")
    }
  }else{

    areaGrid <- OB2Polygon(x)
    bTOver <- overlayBirds(x, grid=areaGrid, spillOver = spillOver)

    warning("To get the most out of summarizeBirds you should have a grid.")
  }

  #Here we use a modifyed version of bOver where the grid is only one single cell.
  #Either a dissolved version of the grid or a polygon from the extent of the organizedBirds
  temporal <- getTemporal(bTOver)
  if(useSpatial){
    #If we have spatial grid data:
    spatial <- getSpatial(bOver)
    spatioTemporal <- getSpatioTemporal(bOver,  visitCol=visitCol) #### Three elements in a list
  }else{
    spatial <- getSpatial(bTOver)
    spatioTemporal <- getSpatioTemporal(bTOver, visitCol=visitCol) #### Three elements in a list
  }

  res <- list("temporal"=temporal,
            "spatial"=spatial,
            "spatioTemporal"= spatioTemporal$resYM,
            "spatioTemporalVisits" = spatioTemporal$resYMvisits,
            # "speciesLists" = spatioTemporal$resSpp,
            "overlaid" = if(useSpatial) bOver$observationsInGrid else bTOver$observationsInGrid
            )
  class(res) <- "SummarizedBirds"
  attr(res,"visitCol") <- visitCol #The column(s) that identify a visit
  attr(res,"spillOver") <- spillOver #If spillover has been used
  attr(res,"spatial") <- useSpatial #If there is a spatial grid in the object

  return(res)

}

#' @rdname summarizeBirds
#' @aliases summarizeBirds
#' @export
summariseBirds <- summarizeBirds  ## To include the brits as well
