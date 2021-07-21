
#' Deconstruction of the overlay
#'
#' This takes the overlay element of a summarisedBirds, reconstruct it as a
#' A long data.frame and removes duplicate visits in other grid cells.
#' @param overlay A list with an element by grid cell
#' @param visitCol A character string specifying the columns that identify a visit.
#
#' @return a long data.frame
#' @keywords internal
deconstructOverlay <- function(overlay, visitCol){

  wNonEmpty <- whichNonEmpty(overlay)

  #Add a grid id to every element (grid) in the overlay-list
  for(i in seq(length(overlay))){
    # if(all(is.na(overlay[[i]]))){
    #   overlay[[i]]$grid <- integer(0)
    # }else{
    #   overlay[[i]]$grid <- i
    # }
    if(!all(is.na(overlay[[i]]))){
      overlay[[i]]$grid <- i
    }
  }

  overlay <- overlay[wNonEmpty]

  #De-construct the list
  overlay <- data.frame(data.table::rbindlist(overlay))
  #Result DF
  res <- overlay
  #A to be array with visits where spillover has been removed from.
  visitList <- integer(0)

  for(r in seq(nrow(overlay))){
    visit <- overlay[r,visitCol]
    if(any(visit==visitList)){
      #If the visit has been removed from spillovers
      next()
    }
    visitList <- c(visitList, visit)
    grid <- overlay[r,"grid"]
    #Removes the observations with the same visitID but with different grid.
    res <- res[!(res[,visitCol] == visit & res[,"grid"]!=grid),]
  }

  cols <- c("scientificName", "year", "month", "day", visitCol)

  return(res[,cols])
}

#' @keywords internal
#' @importFrom rlang .data
exportSpatial <- function(sb, timeRes, variable, method){
  spatial <- sb$spatial
  resRowNames <- rownames(spatial)
  singleGrid <- ifelse(length(resRowNames)==1, TRUE, FALSE)
  yearsAll <- as.numeric(dimnames(sb$spatioTemporal)[[2]])
  nyears <- length(yearsAll)
  visitCol <- attr(sb, "visitCol")
  if (variable == "nCells") stop("This combination of variable and dimension is not defined")

  if(variable %in% c("nObs", "nVis","nSpp","nDays", "nYears")){
    if (is.null(timeRes)){
      if (method != "sum") stop("This combination of variable and time resolution only accepts 'sum' as summary method")
      wCol<-findCols(variable, spatial, value = FALSE)
      spatial<-spatial[,wCol]
    } else if(timeRes == "yearly"){
      if(variable == "nYears") stop("This combination of variable and time resolution is not defined because it has no meaning")
      if (method != "sum") stop("This combination of variable and time resolution only accepts 'sum' as summary method")
      wCol<-findCols(variable, spatial, value = FALSE)
      tmp<-data.frame(sb$spatioTemporal[,,13, variable])## Already added accordingly
      colnames(tmp)<-if(!singleGrid) yearsAll else variable
      spatial<-st_as_sf(cbind(tmp, st_geometry(spatial)))

    } else if(timeRes == "monthly"){
      if(variable == "nYears") stop("This combination of variable and time resolution is not defined because it has no meaning")
      if (method != "sum") stop("This combination of variable and time resolution only accepts 'sum' as summary method")
      dat <- sb$spatioTemporal[,, 1:12, variable]
      dftmp <- data.frame(matrix(NA, nrow=length(resRowNames), ncol=nyears*12))
      colnames(dftmp) <- paste0(rep(yearsAll, each=12), "-", sprintf("%02d", 1:12))
      spatial <- st_as_sf(cbind(dftmp, st_geometry(spatial)))

      for(i in 1:nyears){
        start <- (i - 1) * 12 + 1
        stop <- i * 12
        if(!singleGrid){
          tmp <- data.frame(dat[,i,])
        } else {
          if(nyears==1) {
            tmp <- data.frame(dat)
          } else {
            tmp <- data.frame(dat[i,])
          }
        }
        spatial[,start:stop] <- tmp[[1]]
      } #end for loop

    } else if(timeRes == "month"){
      sumDim<-if (singleGrid) 2 else c(1,3)
      tmp <- if(nyears==1){
              sb$spatioTemporal[,,1:12, "nObs"]
            } else {
              if(variable == "nYears"){
                if (method != "sum") stop("This combination of variable and time resolution only accepts 'sum' as summary method")
                apply(sb$spatioTemporal[,,1:12, "nObs"], sumDim, function(x) sum(!is.na(x) & x!=0))
              } else {
                if (!(method %in% c("sum", "median", "mean"))) stop("This combination of variable and time resolution only accepts 'sum', 'mean' or 'median' as summary method")
                apply(sb$spatioTemporal[,,1:12, variable], sumDim, method)
              }
            }

      dfs <- data.frame("V1"=round(tmp, 2))
      colnames(dfs) <- if(!singleGrid) month.abb else variable

      spatial <- st_as_sf(cbind(dfs, st_geometry(spatial)))

    }else{
      stop("Wrong input for variable timeRes. Try NULL, \"Yearly\", \"Monthly\" or \"Month\" for dimension = \"Spatial\".")
    }
  }

  if(variable == "avgSll"){
    if (is.null(timeRes)){
      if (method != "median") stop("This combination of variable and time resolution only accepts 'median' as summary method")
      wCol<-findCols(variable, spatial, value = FALSE)
      spatial<-spatial[,wCol]
    } else {
      if (timeRes %in% c("yearly", "monthly") & method != "median") stop("This combination of variable and time resolution only accepts 'median' as summary method")
      if (timeRes == "month" & !(method %in% c("median","mean"))) stop("This combination of variable and time resolution only accepts 'median' or 'mean' as summary method")

      wNonEmpty <- whichNonEmpty(sb$overlaid)

      ncolumns <- switch(timeRes,
                         "yearly" = length(yearsAll),
                         "monthly"= length(yearsAll)*12,
                         "month"  = 12)
      resList <- lapply(1:length(sb$overlaid), function(x) rep(NA, ncolumns))
      tmpList <- lapply(sb$overlaid[wNonEmpty], function(x){
        if(timeRes == "yearly"){
          gby <- group_by(x, factor(.data$year, levels = yearsAll),
                               # !!dplyr::sym(visitCol),
                               !!sym(visitCol),
                               .drop=FALSE)
          resSLL <- summarise(gby, SLL=n_distinct(.data$scientificName))
          resAvg <- summarise(resSLL, avgSll=median(.data$SLL))
          return(resAvg$avgSll)
        } else { # monthly
          gby<-group_by(x, factor(.data$year, levels = yearsAll),
                               factor(.data$month, levels = 1:12), #, labels=month.abb
                               !!sym(visitCol),
                               .drop=FALSE)
          resSLL <- summarise(gby, SLL=n_distinct(.data$scientificName))
          resAvg <- as.data.frame(summarise(resSLL, avgSll=median(.data$SLL)))
          colnames(resAvg) <- c("year", "month", "avgSll")

          if(timeRes == "monthly"){
            res<-resAvg$avgSll
            # names(res) <- dimnames(sb$spatioTemporal)[[1]]
            return(res)
          } else if (timeRes == "month"){ # timeRes == "month"
            res<-numeric(12)
            for(m in 1:12){
              tmp <- resAvg$avgSll[which(resAvg$month==m)]
              if (sum(tmp>0) == 0) {
                res[m] <- 0
              } else {
                res[m] <- switch(method,
                                 "median"= median(tmp[tmp>0]),
                                 "mean"  = round(mean(tmp[tmp>0])), 2)
              }
            }
            # names(res) <- month.abb
            return(res)
          } else {
            stop("Wrong input for variable timeRes. Try NULL, 'Yearly', 'Monthly' or 'Month' for dimension = 'Spatial'.")
          }
        } ## end if timeRes
      })  ### end lapply

      resList[wNonEmpty]<-tmpList
      tmp<-as.data.frame(matrix(unlist(resList, use.names = TRUE),
                                nrow=dim(sb$spatioTemporal)[1],
                                ncol=ncolumns, byrow = TRUE),
                         row.names=dimnames(sb$spatioTemporal)[[1]])
      colnames(tmp)<-switch(timeRes,
                            "yearly" = yearsAll,
                            "monthly"= paste0(rep(yearsAll, each=12), "-", sprintf("%02d", 1:12)),
                            "month"  = month.abb)

      spatial <- st_as_sf(cbind(tmp, st_geometry(spatial)))
    }
  }
  return(spatial)
}

# A function to remove nonexistent combination of days like April 31. X is the result of group by with dates as factors
#' @keywords internal
removeInexDays<-function(x){
  if (!all(c("year", "month", "day") %in% colnames(x) )) stop("Input data must have the columns 'year', 'month' and 'day'")

  dates<-as.Date(paste0(x$year, "-",
                        sprintf("%02d", x$month), "-",
                        sprintf("%02d", x$day)))
  wRm <- which(is.na(dates))
  x <- x[-wRm,]
  return(x)
}

### a function to get average species list over time
#' @keywords internal
#' @importFrom rlang .data
getTemporalAvgSll<-function(obsData, timeRes, visitCol, yearsAll){
  if(timeRes=="yearly"){
    gby<-group_by(obsData, year=factor(.data$year, levels = yearsAll),
                         !!sym(visitCol), .drop=FALSE)
  } else if(timeRes %in% c("monthly", "month")){
    gby<-group_by(obsData, year=factor(.data$year, levels = yearsAll),
                         month=factor(.data$month, levels = 1:12),
                         !!sym(visitCol), .drop=FALSE)
  } else if(timeRes=="daily"){
    gby<-group_by(obsData, year=factor(.data$year, levels = yearsAll),
                         month=factor(.data$month, levels = 1:12),
                         day=factor(.data$day, levels = 1:31),
                         !!sym(visitCol), .drop=FALSE)
  } else {
    stop(paste0("Unknown timeRes: ", timeRes ))
  }
  resSLL <- summarise(gby, SLL=n_distinct(.data$scientificName))
  res <- summarise(resSLL, avgSll=median(.data$SLL))

  if(timeRes=="daily") res <- removeInexDays(res)
  return(res)
}

# A function to count pixels with data
#' @keywords internal
countIfHigher <- function(x, thr, na.rm = TRUE) {
  tmp <- ifelse(x>=thr, 1, 0)
  tmp.sum <- sum(tmp, na.rm=na.rm)
  return(tmp.sum)
}

#' @keywords internal
#' @importFrom rlang .data
#' @importFrom dplyr pull
#' @importFrom zoo zoo
#' @importFrom xts as.xts
#' @importFrom lubridate ymd
exportTemporal <- function(sb, timeRes, variable, method){
  if (variable == "nYears" && timeRes != "month")  stop("This combination of variable and time resolution is not defined because it has no meaning")
  if (is.null(timeRes)) stop("Time resolution ('timeRes') needs to be defined for dimension 'Temporal'")

  ## TODO convert to xts and use xts function period.sum(sb.xts$nObs, endpoints(sb.xts$nObs, on = "months"))
  yearsAll <- as.numeric(dimnames(sb$spatioTemporal)[[2]])
  visitCol <- attr(sb, "visitCol")
  obsData <- deconstructOverlay(sb$overlaid, attr(sb, "visitCol"))

  ## Grouping
  if(timeRes=="yearly"){
    gby<-group_by(obsData, year=factor(.data$year, levels = yearsAll), .drop=FALSE)
  } else if(timeRes %in% c("monthly", "month")){
    gby<-group_by(obsData, year=factor(.data$year, levels = yearsAll),
                          month=factor(.data$month, levels = 1:12), .drop=FALSE)
  } else if(timeRes=="daily"){
    gby<-group_by(obsData, year=factor(.data$year, levels = yearsAll),
                            month=factor(.data$month, levels = 1:12),
                            day=factor(.data$day, levels = 1:31), .drop=FALSE)
  } else {
    stop(paste0("Unknown timeRes: ", timeRes ))
  }

  if (variable %in% c("nObs", "nVis","nSpp","nDays")){
    ## Summarising
    res <- summarise(gby,
                     nObs=n(),
                     nVis=n_distinct(!!sym(visitCol)),
                     nSpp=n_distinct(.data$scientificName),
                     nDays=n_distinct(paste0(.data$year, .data$month, .data$day)))

    if(timeRes=="daily") res <- removeInexDays(res)

    if (timeRes %in% c("yearly", "monthly", "daily")){
      if (method != "sum") stop("This combination of variable and time resolution only accepts 'sum' as summary method")
      resVar <- pull(res, !!sym(variable))
      dates <- ymd(switch(timeRes,
                          "yearly" = paste0(yearsAll, "-01-01"),
                          "monthly"= paste0(res$year, "-", sprintf("%02d", res$month), "-01"),
                          "daily"  = paste0(res$year, "-", sprintf("%02d", res$month), "-", sprintf("%02d", res$day)))
                   )
      # MAKE RESVAR
      resVar <- as.xts(zoo(resVar, dates))

    } else { #month
      if (!(method %in% c("sum", "median", "mean"))) stop("This combination of variable and time resolution only accepts 'sum', 'median' or 'mean' as summary method")
      gpby <- group_by(res, .data$month)
      if (method == "sum")    resMon <-  summarise(gpby, var=sum(!!sym(variable)))
      if (method == "mean")   resMon <-  summarise(gpby, var=mean(!!sym(variable)))
      if (method == "median") resMon <-  summarise(gpby, var=median(!!sym(variable)))
      resVar <- pull(resMon, .data$var)
      names(resVar) <- month.abb
    }

  } else if (variable == "nYears"){
    ## only valid for month, prohibition set at the beginning
    if (method != "sum") stop("This combination of variable and time resolution only accepts 'sum' as summary method")
    tmp <- summarise(gby, nYear=n_distinct(.data$year))
    resMon <-  summarise(group_by(tmp, .data$month), var=sum(.data$nYear))
    resVar <- pull(resMon, .data$var)
    names(resVar) <- month.abb

  } else if (variable == "avgSll"){
    ## Group also by visit
    if (timeRes %in% c("yearly", "monthly", "daily")){
      if (method != "median") stop("This combination of variable and time resolution only accepts 'median' as summary method")
      res <- getTemporalAvgSll(obsData, timeRes, visitCol, yearsAll)
      resVar <- pull(res, .data$avgSll)

      dates <- ymd(switch(timeRes,
                          "yearly" = paste0(yearsAll, "-01-01"),
                          "monthly"= paste0(res$year, "-", sprintf("%02d", res$month), "-01"),
                          "daily"  = paste0(res$year, "-", sprintf("%02d", res$month), "-", sprintf("%02d", res$day)))
                   )
      # MAKE RESVAR
      resVar <- as.xts(zoo(resVar, dates))

    } else { #month
      if (!(method %in% c("median", "mean"))) stop("This combination of variable and time resolution only accepts 'median' or 'mean' as summary method")
      res <- getTemporalAvgSll(obsData, timeRes, visitCol, yearsAll)
      gpby <- group_by(res, .data$month)
      if (method == "mean")   resMon <-  summarise(gpby, var = round(mean(.data$avgSll),2))
      if (method == "median") resMon <-  summarise(gpby, var = median(.data$avgSll))
      resVar <- pull(resMon, .data$var)
      names(resVar) <- month.abb
    }
  ## nCells
  } else if (variable == "nCells"){
    ## actually this doesnt require the deconstrucOverlay from the begining
    if(method != "sum" && timeRes != "month")  stop("This combination of variable and time resolution only accepts 'sum' as summary method")

    resRowNames <- rownames(sb$spatial)
    singleGrid <- ifelse(length(resRowNames) == 1, TRUE, FALSE)

    if (timeRes == c("yearly")){
      if (singleGrid) {
        resVar <- countIfHigher(sb$spatioTemporal[,,13,1], thr=1)
      } else {
        resVar <- apply(sb$spatioTemporal[,,13,1], 2, countIfHigher, thr=1)
      }
      names(resVar) <- paste0(yearsAll, "-01-01")
    }

    if (timeRes %in% c("monthly", "month")){
      if (singleGrid) {
          ncellsM <- ifelse(sb$spatioTemporal[,,1:12,1]>=1, 1, 0)
      } else {
        ncellsM <- apply(sb$spatioTemporal[,,1:12,1], 2:3, countIfHigher, thr=1)} # matrix

      if (timeRes == "monthly"){
        resVar <- as.vector(t(ncellsM))
        names(resVar) <- paste0(rep(yearsAll, each=12), "-", 1:12, "-01")

      } else {
        resVar <- apply(ncellsM, 2, method)
        names(resVar) <- month.abb
      }
    }

    if (timeRes == c("daily")){
      daygrid <- lapply(sb$overlaid, function(x){
        unique(paste0(x$year, "-", x$month, "-", x$day))
      } )
      daygrid <- daygrid[daygrid != "--"]
      dayGridP <- as.character(as.Date(unlist(daygrid)))

      all.Days <- as.character(sort(as.Date(unique(unlist(daygrid)))))
      resVar <- unlist(
                  lapply(all.Days,
                         function(x){
                           sum(stringr::str_count(dayGridP, x), na.rm = TRUE)
                         }
                  )
                )
      names(resVar) <- all.Days
    }

    if (timeRes != "month") {
      resVar <- zoo(unname(resVar),
                    ymd(names(resVar)))
      resVar <- as.xts(resVar)
    }
  } else {
    stop(paste0("variable = ", variable, " is not a valid input"))
  }
  return(resVar)
}



#' Export single variables from SummarisedBirds objects
#'
#' @param x a SummarizedBirds-object
#' @param dimension a character string indicating if the export should be
#'   \code{"spatial"} or \code{"temporal"}
#' @param timeRes A character string indicating what temporal resolution should
#'   be used. For Spatial export the function accepts \code{NULL, "yearly", "monthly"}
#'   or \code{"month"}. For temporal export the function accepts \code{"yearly",
#'   "monthly", "daily"} or \code{"month"}.
#' @param variable a character string indicating which variable should be
#'   exported, \code{"nObs", "nVis", "nSpp", "nDays", "nCells"} or \code{"avgSll" }.
#'   For \code{timeRes = c(NULL, "month")} the function also accepts \code{"nYears"}.
#' @param method Only applicable to \code{timeRes = "month"}. A variable specifying which
#'   statistical method should be applied. The function accepts \code{"sum", "median", "mean"}.
#' @note the difference between \code{timeRes = "monthly"} and \code{timeRes = "month"}
#' is that the former returns "n.years x 12" values, while month summarize over
#' years and returns only 12 values aplying the method among years.
#' For more details over the possible combinations of dimensions and variables
#' please refer to the vignette "Technical details".
#' @return an xts time series (if dimension = "temporal"), a named vector (if 
#' dimension = "temporal" and timeRes = "month"), or a sf (if dimension = "spatial")
#' @export
#'
#' @examples
#' \donttest{
#' grid <- makeGrid(searchPolygon, gridSize = 10)
#' SB <- summariseBirds(organizeBirds(bombusObsShort), grid=grid)
#' EB <- exportBirds(SB, "spatial", "month", "nDays", "median")
#' }
exportBirds <- function(x, dimension, timeRes, variable, method="sum"){

  dimension <- tolower(dimension)

  if (!is.null(timeRes)){
    timeRes <- tolower(timeRes)
    if(!any(timeRes==c("yearly", "month", "monthly", "daily"))){
      stop("Not a valid timeRes")
    }
  }

  variable <- tolower(variable)
  if (any(variable == c("nobs", "nvis", "nspp", "nyears", "ndays", "ncells"))){
    variable <- paste0(substr(variable,1,1),
                       toupper(substr(variable,2,2)),
                       substr(variable, 3, nchar(variable)))
  } else if (variable == "avgsll"){
    variable <- "avgSll"
  } else {
   stop("No valid variable")
  }

  if(!any(method==c("sum", "mean", "median"))){
    stop("Not a valid method")
  }

  if(dimension == "spatial"){
    res <- exportSpatial(x, timeRes, variable, method)
    return(res)
  }else if(dimension == "temporal"){
    res <- exportTemporal(x, timeRes, variable, method)
    return(res)
  }else{
    stop("Wrong input for variable dimension. Try 'spatial' or 'temporal'.")
  }
}
