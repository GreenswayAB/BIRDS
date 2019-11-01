
###This takes the overlay reconstruct it as an data.frame and removes duplicate visits in other gridcells.
deconstructOverlay <- function(overlay, visitCol){

  for(i in 1:length(overlay)){
    if(nrow(overlay[[i]])==0){
      overlay[[i]]$grid <- integer(0)
    }else{
      overlay[[i]]$grid <- i
    }
  }

  overlay <- data.frame(data.table::rbindlist(overlay))
  res <- overlay[,]
  visitList <- integer(0)

  for(r in 1:nrow(overlay)){
    visit <- overlay[r,visitCol]
    if(any(visit==visitList)){
      next()
    }
    visitList <- c(visitList, visit)
    grid <- overlay[r,"grid"]
    res <- res[!(res[,visitCol] == visit & res[,"grid"]!=grid),]
  }

  res <-res
  cols <- c("scientificName", "year", "month", "day", visitCol)
## TODO if there is spill over and duplicates have created they have to be removed from this result
  ### Delete duplicated observations
  ## maybe with help from SB$spatial@data$visitsUID

  return(res[,cols])
}


exportSpatial <- function(sb, timeRes, variable, method){

  spatial <- sb$spatial
  resRowNames <- rownames(spatial@data)
  singleGrid <- ifelse(length(resRowNames)==1, TRUE, FALSE)
  yearsAll <- as.numeric(dimnames(sb$spatioTemporal)[[2]])
  visitCol <- attr(sb, "visitCol")
  if (variable == "nCells") stop("This combination of variable and dimension is not defined")

  if(variable %in% c("nObs", "nVis","nSpp","nDays", "nYears")){
    if (is.null(timeRes)){
      if (method != "sum") stop("This combination of variable and time resolution only accepts 'sum' as summary method")
      tmp<-data.frame(spatial@data[,variable])
      colnames(tmp)<-variable
      spatial@data<-tmp
    } else if(timeRes == "yearly"){
      if(variable == "nYears") stop("This combination of variable and time resolution is not defined because it has no meaning")
      if (method != "sum") stop("This combination of variable and time resolution only accepts 'sum' as summary method")
      tmp<-data.frame(sb$spatioTemporal[,,13, variable])## Already added accordingly
      colnames(tmp)<-ifelse(!singleGrid, yearsAll, variable)
      spatial@data <- tmp

    } else if(timeRes == "monthly"){
      if(variable == "nYears") stop("This combination of variable and time resolution is not defined because it has no meaning")
      if (method != "sum") stop("This combination of variable and time resolution only accepts 'sum' as summary method")
      dat <- sb$spatioTemporal[,, 1:12, variable]
      nyears<-ifelse(!singleGrid, dim(dat)[2], dim(dat)[1])
      spatial@data<-data.frame(matrix(NA, nrow=length(resRowNames), ncol=nyears*12))
      colnames(spatial@data) <- paste0(rep(yearsAll, each=12), "-", sprintf("%02d", 1:12))

      for(i in 1:nyears){
        start <- (i - 1) * 12 + 1
        stop <- i * 12
        spatial@data[,start:stop] <- ifelse(!singleGrid, data.frame(dat[,i,]), data.frame(dat[i,]) )[[1]]
      }

    } else if(timeRes == "month"){
      sumDim<-if (singleGrid) 2 else c(1,3)

      if(variable == "nYears"){
        if (method != "sum") stop("This combination of variable and time resolution only accepts 'sum' as summary method")
        tmp <- apply(sb$spatioTemporal[,,1:12, "nObs"], sumDim, function(x) sum(!is.na(x) & x!=0))
      } else {
        if (!(method %in% c("sum", "median", "mean"))) stop("This combination of variable and time resolution only accepts 'sum', 'mean' or 'median' as summary method")
        tmp <- apply(sb$spatioTemporal[,,1:12, variable], sumDim, method)
      }
      spatial@data <- data.frame("V1"=round(tmp, 2))
      colnames(spatial@data)<-ifelse(!singleGrid, month.abb, variable)
    }else{
      stop("Wrong input for variable timeRes. Try NULL, \"Yearly\", \"Monthly\" or \"Month\" for dimension = \"Spatial\".")
    }
  }

  if(variable == "avgSll"){
    if (is.null(timeRes)){
      if (method != "median") stop("This combination of variable and time resolution only accepts 'median' as summary method")
      tmp<-data.frame(spatial@data[,variable])
      colnames(tmp)<-variable
      spatial@data<-tmp
    } else {
      if (timeRes %in% c("yearly", "monthly") & method != "median") stop("This combination of variable and time resolution only accepts 'median' as summary method")
      if (timeRes == "month" & !(method %in% c("median","mean"))) stop("This combination of variable and time resolution only accepts 'median' or 'mean' as summary method")

      wNonEmpty <- unlist(lapply(sb$overlaid, function(x) nrow(x)>0))

      ncolumns <- switch(timeRes,
                         "yearly" = length(yearsAll),
                         "monthly"= length(yearsAll)*12,
                         "month"  = 12)
      resList <- lapply(1:length(sb$overlaid), function(x) rep(NA, ncolumns))
      tmpList <- lapply(sb$overlaid[wNonEmpty], function(x){
        if(timeRes == "yearly"){
          gby<-dplyr::group_by(x, factor(year, levels = yearsAll),
                               !!dplyr::sym(visitCol),
                               .drop=FALSE)
          resSLL <- dplyr::summarise(gby, SLL=n_distinct(scientificName))
          resAvg <- dplyr::summarise(resSLL, avgSll=median(SLL))
          return(resAvg$avgSll)
        } else { # monthly
          gby<-dplyr::group_by(x, factor(year, levels = yearsAll),
                               factor(month, levels = 1:12), #, labels=month.abb
                               !!dplyr::sym(visitCol),
                               .drop=FALSE)
          resSLL <- dplyr::summarise(gby, SLL=n_distinct(scientificName))
          resAvg <- as.data.frame(dplyr::summarise(resSLL, avgSll=median(SLL)))
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

      spatial@data<-tmp
    }
  }
  return(spatial)
}

####### Temporal

### a funciton to remove inexistent combination of days like april 31. X is the result of group by with dates as factors
removeInexDays<-function(x){
  if (!all(c("year", "month", "day") %in% colnames(x) )) stop("Input data must have the columns 'year', 'month' and 'day'")

  dates<-as.Date(paste0(x$year, "-", sprintf("%02d", x$month), "-", sprintf("%02d", x$day)))
  wRm <- which(is.na(dates))
  x <- x[-wRm,]
  return(x)
}

getTemporalAvgSll<-function(obsData, timeRes, visitCol, yearsAll){
  if(timeRes=="yearly"){
    gby<-group_by(obsData, year=factor(year, levels = yearsAll),
                         !!dplyr::sym(visitCol), .drop=FALSE)
  } else if(timeRes %in% c("monthly", "month")){
    gby<-group_by(obsData, year=factor(year, levels = yearsAll),
                         month=factor(month, levels = 1:12),
                         !!dplyr::sym(visitCol), .drop=FALSE)
  } else if(timeRes=="daily"){
    gby<-group_by(obsData, year=factor(year, levels = yearsAll),
                         month=factor(month, levels = 1:12),
                         day=factor(day, levels = 1:31),
                         !!dplyr::sym(visitCol), .drop=FALSE)
  } else {
    stop(paste0("Unknown timeRes: ", timeRes ))
  }
  resSLL <- summarise(gby, SLL=n_distinct(scientificName))
  res <- summarise(resSLL, avgSll=median(SLL))

  if(timeRes=="daily") res <- removeInexDays(res)
  return(res)
}

## a function to count pixels with data
countIfHigher <- function(x, thr, na.rm = TRUE) {
  tmp <- ifelse(x>=thr, 1, 0)
  tmp.sum <- sum(tmp, na.rm=na.rm)
  return(tmp.sum)
}


exportTemporal <- function(sb, timeRes, variable, method){
  if (variable == "nYears" & timeRes != "month")  stop("This combination of variable and time resolution is not defined because it has no meaning")
  if (is.null(timeRes)) stop("Time resolution ('timeRes') needs to be defined for dimension 'Temporal'")

  yearsAll <- as.numeric(dimnames(sb$spatioTemporal)[[2]])
  visitCol <- attr(sb, "visitCol")
  obsData <- deconstructOverlay(sb$overlaid, attr(sb, "visitCol"))

  ## Grouping
  if(timeRes=="yearly"){
    gby<-group_by(obsData, year=factor(year, levels = yearsAll), .drop=FALSE)
  } else if(timeRes %in% c("monthly", "month")){
    gby<-group_by(obsData, year=factor(year, levels = yearsAll),
                                  month=factor(month, levels = 1:12), .drop=FALSE)
  } else if(timeRes=="daily"){
    gby<-group_by(obsData, year=factor(year, levels = yearsAll),
                                  month=factor(month, levels = 1:12),
                                  day=factor(day, levels = 1:31), .drop=FALSE)
  } else {
    stop(paste0("Unknown timeRes: ", timeRes ))
  }

  if (variable %in% c("nObs", "nVis","nSpp","nDays")){
    ## Summarising
    res <- summarise(gby,
                     nObs=n(),
                     nVis=n_distinct(!!dplyr::sym(visitCol)),
                     nSpp=n_distinct(scientificName),
                     nDays=n_distinct(paste0(year,month,day)))

    if(timeRes=="daily") res <- removeInexDays(res)

    if (timeRes %in% c("yearly", "monthly", "daily")){
      if (method != "sum") stop("This combination of variable and time resolution only accepts 'sum' as summary method")
      resVar <- dplyr::pull(res, !!dplyr::sym(variable))
      names(resVar) <- switch(timeRes,
                              "yearly" = paste0(yearsAll, "-01-01"),
                              "monthly"= paste0(res$year, "-", sprintf("%02d", res$month), "-01"),
                              "daily"  = paste0(res$year, "-", sprintf("%02d", res$month), "-", sprintf("%02d", res$day)))
      # MAKE RESVAR to xts::as.xts()
      resVar <- xts::as.xts(resVar)

    } else { #month
      if (!(method %in% c("sum", "median", "mean"))) stop("This combination of variable and time resolution only accepts 'sum', 'median' or 'mean' as summary method")
      if (method == "sum")    resMon <-  summarise(group_by(res, month), var=sum(!!dplyr::sym(variable)))
      if (method == "mean")   resMon <-  summarise(group_by(res, month), var=mean(!!dplyr::sym(variable)))
      if (method == "median") resMon <-  summarise(group_by(res, month), var=median(!!dplyr::sym(variable)))
      resVar <- dplyr::pull(resMon, var)
      names(resVar) <- month.abb
    }

  } else if (variable == "nYears"){
    ## only valid for month, prohibition set at the begining
    tmp <- summarise(gby, nYear=n_distinct(year))
    resMon <-  summarise(group_by(tmp, month), var=sum(nYear))
    resVar <- dplyr::pull(resMon, var)
    names(resVar) <- month.abb

  } else if (variable == "avgSll"){
    ## Group also by visit
    res <- getTemporalAvgSll(obsData, timeRes, visitCol, yearsAll)
    if (timeRes %in% c("yearly", "monthly", "daily")){
      if (method != "median") stop("This combination of variable and time resolution only accepts 'median' as summary method")
      resVar <- dplyr::pull(res, avgSll)
      names(resVar) <- switch(timeRes,
                              "yearly" = paste0(yearsAll, "-01-01"),
                              "monthly"= paste0(res$year, "-", sprintf("%02d", res$month), "-01"),
                              "daily"  = paste0(res$year, "-", sprintf("%02d", res$month), "-", sprintf("%02d", res$day)))
      resVar <- xts::as.xts(resVar)

    } else { #month
      if (!(method %in% c("median", "mean"))) stop("This combination of variable and time resolution only accepts 'median' or 'mean' as summary method")
      if (method == "mean")   resMon <-  summarise(group_by(res, month), var = round(mean(avgSll),2))
      if (method == "median") resMon <-  summarise(group_by(res, month), var = median(avgSll))
      resVar <- dplyr::pull(resMon, var)
      names(resVar) <- month.abb
    }
  ## nCells
  } else if (variable == "nCells"){
    ## actually this doesnt require the deconstrucOverlay from the begining
    if(method != "sum" & timeRes != "month")  stop("This combination of variable and time resolution only accepts 'sum' as summary method")

    resRowNames <- rownames(sb$spatial@data)
    singleGrid <- ifelse(length(resRowNames)==1, TRUE, FALSE)

    if (timeRes == c("yearly")){
      if (singleGrid) {
        resVar <- countIfHigher(sb$spatioTemporal[,,13,1], thr=1)
      } else {
        resVar <- apply(sb$spatioTemporal[,,13,1], 2, countIfHigher, thr=1)}
      names(resVar) <- paste0(yearsAll, "-01-01")
    }

    if (timeRes %in% c("monthly", "month")){
      if (singleGrid) {
        if(length(yearsAll)==1){
          ncellsM <- ifelse(sb$spatioTemporal[,,1:12,1]>=1, 1, 0)
        } else {
          ncellsM <- countIfHigher(sb$spatioTemporal[,,1:12,1], thr=1)
        }
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
      daygrid<-lapply(sb$overlaid, function(x) unique(paste0(x$year, "-", x$month, "-", x$day)))
      dayGridP<-as.character(as.Date(unlist(daygrid)))

      all.Days <- as.character(sort(as.Date(unique(unlist(daygrid)))))
      resVar <- unlist(
        lapply(all.Days, FUN=function(x){
          #length(grep(pattern = x, as.Date(unlist(daygrid))))
          sum(stringr::str_count(dayGridP, x), na.rm = TRUE)
        })
      )
      names(resVar) <- all.Days
    }

    if (timeRes != "month") resVar <- xts::as.xts(resVar)

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
#' @param timeRes A character string indicating what tempral resolution should
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
#' @return an xts time series (if dimension = "temporal"),
#' a named vector (if dimension = "temporal" and timeRes = "month"),
#' or a SpatialPolygonsDataFrame (if dimension = "spatial")
#' @export
#'
#' @examples
#' grid <- makeGrid(searchPolygon, gridSize = 10)
#' SB <- summariseBirds(organizeBirds(bombusObs), grid=grid)
#' EB <- exportBirds(SB, "spatial", "month", "nDays", "median")

exportBirds <- function(x, dimension, timeRes, variable, method="sum"){

  dimension <- tolower(dimension)

  if (!is.null(timeRes)){
    timeRes <- tolower(timeRes)
  }

  variable <- tolower(variable)
  if (any(variable == c("nobs", "nvis", "nspp", "nyears", "ndays", "ncells"))){
    variable <- paste0(substr(variable,1,1), toupper(substr(variable,2,2)), substr(variable, 3, nchar(variable)))
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
    return(NULL)
    stop("Wrong input for variable dimension. Try 'spatial' or 'temporal'.")
  }


}
