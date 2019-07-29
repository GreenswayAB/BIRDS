
##This takes the overlay reconstruct it as an data.frame and removes duplicate visits in other gridcells.
deconstructOverlay<-function(overlay, visitCol){

  for(i in 1:length(overlay)){
    if(nrow(overlay[[i]])==0){
      overlay[[i]]$grid<-integer(0)
    }else{
      overlay[[i]]$grid<-i
    }
  }

  overlay<-data.frame(data.table::rbindlist(overlay))

  res<-overlay[,]

  visitList<-integer(0)

  for(r in 1:nrow(overlay)){

    visit<-overlay[r,visitCol]

    if(any(visit==visitList)){
      next()
    }

    visitList<-c(visitList, visit)

    grid<-overlay[r,"grid"]

    res<-res[!(res[,visitCol] == visit & res[,"grid"]!=grid),]

  }

  cols<-c("scientificName", "year", "month", "day", visitCol)

  return(res[,cols])

}


exportSpatial <- function(sb, scale, variable, method){

  res<-sb$spatial

  resRowNames<-rownames(res@data)

###No temporal scale
  if(is.null(scale)){
    if( #Check if the combination of method and variable is allowed
      (method=="sum" & any(variable==c("nObs", "nVis", "nSpp", "nYears", "nDays"))) |
      (method=="median" & variable=="avgSll")){
      if(variable != "nDays"){
        #If the variable not is nDays it's already in the dataset
        res<-res[,variable]
      }else{
        #If the variable is nDays it has to be extracted from the overlay

        res@data<-res@data[,-c(1:ncol(res@data))]

        res$nDays<-unlist(lapply(sb$overlaid, function(x){
          length(unique(apply(x, 1, function(y){
            paste0(y["year"],"-",y["month"],"-",y["day"])
            })))
          }))
      }
    }else{
      stop(paste0("variable = \"", variable, "\" and method = \"", method, "\" is not an allowed combination."))
    }

###Summarize yearly
  }else if(scale == "yearly"){
    if(#Check if the combination of method and variable is allowed
      (method=="sum" & any(variable==c("nObs", "nVis", "nSpp", "nDays"))) |
      (method=="median" & variable=="avgSll")){
      if(variable != "nDays"){
        #If the variable not is nDays it's already in the dataset
        res@data<-data.frame(sb$spatioTemporal[,,13,variable])
      }else{
        #If the variable is nDays it has to be extracted from the overlay

        res@data<-res@data[,-c(1:ncol(res@data))]

        years<-range(as.numeric(dimnames(sb$spatioTemporal)[[2]]))

        for(y in years[1]:years[2]){
          res[[as.character(y)]]<-unlist(lapply(sb$overlaid, function(x){
            length(unique(apply(x[x[,"year"]==y,], 1, function(y){
              paste0(y["year"],"-",y["month"],"-",y["day"])
            })))
          }))
        }
      }

      colnames(res@data)<-dimnames(sb$spatioTemporal)[2][[1]]

    }else if(variable=="nYears"){
      stop("Variable nYears cannot be exported for timeRes=\"yearly\"")
    }else{
      stop(paste0("variable = \"", variable, "\" and method = \"", method, "\" is not an allowed combination."))
    }
###Summarize monthly
  }else if(scale == "monthly"){
    if(#Check if the combination of method and variable is allowed
      (method=="sum" & any(variable==c("nObs", "nVis", "nSpp", "nDays"))) |
      (method=="median" & variable=="avgSll")){

      if(variable != "nDays"){
        #If the variable not is nDays it's already in the dataset
        res@data<-res@data[,-c(1:ncol(res@data))]

        dat<-sb$spatioTemporal[,,1:12,variable]

        for(i in 1:dim(dat)[2]){
          start<-(i-1)*12+1
          stop<-i*12
          res@data[,start:stop]<-data.frame(dat[,i,])
        }

        colnames(res@data)<-paste0(rep(attributes(sb$spatioTemporal)$dimnames[[2]],each=12),"-",1:12)
      }else{
        #If the variable is nDays it has to be extracted from the overlay
        res@data<-res@data[,-c(1:ncol(res@data))]

        years<-range(as.numeric(dimnames(sb$spatioTemporal)[[2]]))
        month<-1:12

        for(y in years[1]:years[2]){
          for(m in month){
            res[[paste0(as.character(y),"-",as.character(m))]]<-unlist(lapply(sb$overlaid, function(x){
              length(unique(apply(x[x[,"year"]==y & x[,"month"]==m,], 1, function(y){
                paste0(y["year"],"-",y["month"],"-",y["day"])
              })))
            }))
          }
        }
      }

    }else if(variable=="nYears"){
      stop("Variable nYears cannot be exported for timeRes=\"monthly\"")
    }else{
      stop(paste0("variable = \"", variable, "\" and method = \"", method, "\" is not an allowed combination."))
    }

##Summarize month
  }else if(scale == "month"){
    if(any(variable==c("nObs", "nVis", "nSpp"))){
      if(any(method==c("sum","median", "mean"))){
        res@data<-data.frame(apply(sb$spatioTemporal[,,1:12, variable], c(1,3), method))
      }else{
        stop("Wrong input for method, try \"sum\",\"median\" or \"mean\".")
      }
    }else if(variable=="avgSll"){
      if (any(method==c("median", "mean"))){
        res@data<-data.frame(apply(sb$spatioTemporal[,,1:12, variable], c(1,3), method))
      }else{
        stop(paste0("variable = \"", variable, "\" and method = \"", method,
                    "\" is not an allowed combination. \nOnly method = \"median\" is allowed for avgSll"))
      }
    }else if(variable=="nYears"){
      if (method=="sum"){
        res@data<-res@data[,-c(1:ncol(res@data))]

        month<-1:12

        for(m in month){
          res[[as.character(m)]]<-unlist(lapply(sb$overlaid, function(x){
            length(unique(x[x[,"month"]==m,"year"]))
          }))
        }

        colnames(res@data)<-month.abb

      }else{
        stop(paste0("variable = \"", variable, "\" and method = \"", method,
                    "\" is not an allowed combination. \nOnly method = \"sum\" is allowed for nYears"))
      }
    } else if(variable=="nDays"){
      # if (method %in% c("sum", "mean", "median")){
        res@data<-res@data[,-c(1:ncol(res@data))]

        res@data<-as.data.frame(
          matrix(
          unlist(lapply(sb$overlaid, function(x){
            vec.res<-numeric(12)
            # names(vec.res)<-1:12
            tmp<-summarise(
                    summarise(group_by(x, month,year), nDaysY=n_distinct(day)),
                    nDays=switch(method,
                                        "sum" = sum(nDaysY),
                                        "mean" = mean(nDaysY),
                                        "median" = median(nDaysY)) )
            vec.res[tmp$month]<-tmp$nDays
            return(vec.res)
          })), ncol=12, byrow=TRUE)
        )

        colnames(res@data)<-month.abb
    }

  }else{
    stop("Wrong input for variable timeRes Try NULL, \"Yearly\", \"Monthly\" or \"Month\" for dimension = \"Spatial\".")
  }

  return(res)

}

getTemporalAvgSll<-function(sb, scale){
  ##This function works only for temporal resolution yearly, monthly and daily


  visitCol<-attr(sb, "visitCol")

  #We need to deconstruct the overlay to a dataframe again

  obsData<-deconstructOverlay(sb$overlaid, attr(sb, "visitCol"))

  if(scale=="yearly"){
   res<-dplyr::group_by(obsData, year, !!dplyr::sym(visitCol))
  }else if(scale=="monthly"){
    res<-dplyr::group_by(obsData, year, month, !!dplyr::sym(visitCol))
  }else if(scale=="daily"){
    res<-dplyr::group_by(obsData, year, month, day, !!dplyr::sym(visitCol))
  }else(
    stop(paste0("Unknown timeRes: ",scale ))
  )

  res<-dplyr::summarise(res,SLL=n_distinct(scientificName))

  res <- summarise(res,avgSLL=median(SLL))

  return(res)

}




exportTemporal <- function(sb, scale, variable, method){

  if(any(scale==c("yearly", "monthly", "daily"))){

    if(any(variable==c("nObs", "nVis", "nSpp"))){

      if(method != "sum"){
        stop(paste0("method = \"",method, "\" is not a valid input for variable = \"",variable, "\"."))
      }

      if(scale=="yearly"){
        byScale<-xts::apply.yearly(sb$temporal[,variable], sum)
        dates<-as.character(lubridate::year(byScale))
        yRange<-range(lubridate::year(byScale))
        range<-as.character(yRange[1]:yRange[2])
      }else if(scale=="monthly"){
        byScale<-xts::apply.monthly(sb$temporal[,variable], sum)
        dates<-paste0(lubridate::year(byScale),"-", lubridate::month(byScale))
        yRange<-range(lubridate::year(byScale))
        range<-paste0(rep(yRange[1]:yRange[2],each=12),"-",1:12)
      }else if(scale=="daily"){
        byScale<-sb$temporal[,variable]
        dates<-as.character(lubridate::date(byScale))
        range<-as.character(seq(lubridate::date(byScale[1,]),lubridate::date(byScale[nrow(byScale),]), by="days"))
      }

      res<-c(rep(0, length(range)))
      names(res)<-range

      res[dates]<-byScale[,1]



    }else if(variable=="avgSll"){

      if(method != "median"){
        stop(paste0("method = \"",method, "\" is not a valid input for variable = \"",variable, "\"."))
      }

      byScale<-getTemporalAvgSll(sb, scale)

      if(scale=="yearly"){
        dates<-as.character(byScale$year)
        range<-as.character(range(byScale$year)[1]:range(byScale$year)[2])
      }else if(scale=="monthly"){
        dates<-paste0(byScale$year,"-", byScale$month)
        yRange<-range(byScale$year)
        range<-paste0(rep(yRange[1]:yRange[2],each=12),"-",1:12)
      }else if(scale=="daily"){
        dates<-as.character(as.Date(paste0(byScale$year,"-", byScale$month, "-", byScale$day)))
        range<-as.character(seq(as.Date(dates[1]),as.Date(dates[length(dates)]), by="days"))
      }


      res<-c(rep(0, length(range)))
      names(res)<-range

      res[dates]<-as.data.frame(byScale)[,"avgSLL"]


    }else if(variable=="nDays"){

      if(scale=="daily"){
        stop("Cannot calculate varibale nDays for tempRes = \"Daily\".")
      }

      if(method != "sum"){
        stop(paste0("method = \"",method, "\" is not a valid input for variable = \"",variable, "\"."))
      }

      if(scale=="yearly"){
        byScale<-xts::apply.yearly(sb$temporal, nrow)
        dates<-as.character(lubridate::year(byScale))
        yRange<-range(lubridate::year(byScale))
        range<-as.character(yRange[1]:yRange[2])
      }else{
        byScale<-xts::apply.monthly(sb$temporal, nrow)
        dates<-paste0(lubridate::year(byScale),"-", lubridate::month(byScale))
        yRange<-range(lubridate::year(byScale))
        range<-paste0(rep(yRange[1]:yRange[2],each=12),"-",1:12)
      }

      res<-c(rep(0, length(range)))
      names(res)<-range

      res[dates]<-byScale[,1]



    }else{
      stop(paste0("variable = \"",variable, "\" is not a valid input"))
    }

### Month
  }else if(scale=="month"){

    res=c(rep(0, 12))
    names(res)=month.abb

    if(any(variable==c("nObs", "nVis", "nSpp"))){

      #Would have been less cod and more readable if the if statement was inside the for-loop
      # but its more efficient to not have a if-statement evaluated 12 times
      if(method=="sum"){
        for(m in 1:12){
          res[m]<-sum(sb$temporal[lubridate::month(sb$temporal)==m,variable])
        }
      }else if(method=="mean"){
        for(m in 1:12){
          res[m]<-sum(sb$temporal[lubridate::month(sb$temporal)==m,variable])/
            (max(lubridate::year(sb$temporal))-min(lubridate::year(sb$temporal))+1)
        }
      }else{
        for(m in 1:12){
          val<-sb$temporal[lubridate::month(sb$temporal)==m,variable]

          ##We need to calcualte a yearly median for month m and add 0 for month with no data.

          valDF<-data.frame(val)
          valDF$year<-lubridate::year(val)

          valDF<-summarise(dplyr::group_by(valDF, year),res=sum(!!dplyr::sym(variable)))

          res[m]<-median(c(valDF$res,
                          rep(0, (max(lubridate::year(sb$temporal))-min(lubridate::year(sb$temporal))+1)-length(valDF$res))))

        }
      }

    }else if(variable=="avgSll"){

      t<-getTemporalAvgSll(sb, "monthly")

      if(method == "median"){
        for(m in 1:12){
          res[m]<-median(as.data.frame(t[t[,"month"]==m,])[,"avgSLL"])
        }
      }else if(method == "mean"){
        for(m in 1:12){
          res[m]<-mean(as.data.frame(t[t[,"month"]==m,])[,"avgSLL"])
        }
      }else{
        stop(paste0("method = \"",method, "\" is not a valid input for variable = \"",variable, "\"."))
      }
    }else if (variable == "nYears"){

      if(method != "sum"){
        stop(paste("method = \"",method, "\" is not a valid input for variable = \"",variable, "\"."))
      }

      t<-xts::apply.monthly(sb$temporal, nrow)

      for(m in 1:12){
        res[m]<-length(unique(lubridate::year(t[lubridate::month(t)==m,])))
      }

    }else if(variable == "nDays"){

      t<-xts::apply.monthly(sb$temporal, nrow)

      if(method=="sum"){
        for(m in 1:12){
          res[m]<-sum(t[lubridate::month(t)==m,1])
        }
      }else if(method=="mean"){
        for(m in 1:12){
          res[m]<-mean(t[lubridate::month(t)==m,1])
        }
      }else{
        for(m in 1:12){
          res[m]<-median(t[lubridate::month(t)==m,1])
        }
      }


    }else{
      stop(paste0("variable = ",variable, "\" is not a valid input for timeRes = \"Month\"."))
    }



  }else{
    stop("Wrong input for variable timeRes Try \"Yearly\", \"Monthly\", \"Daily\" or \"Month\" for dimension = \"Temporal\".")
  }
  return(res)
  }



######## Summarize yearly
#   if(scale == "yearly"){
#     if(#Check if the combination of method and variable is allowed
#       (method=="sum" & any(variable==c("nObs", "nVis", "nSpp", "nDays"))) |
#       (method=="median" & variable=="avgSll")){
#       if(variable != "nDays"){
#         #If the variable not is nDays it's already in the dataset
#         if(variable!="avgSll"){
#           #If the varible not is avgSll it can simply be calculated from the xts
#
#           yearly<-xts::apply.yearly(sb$temporal[,variable], sum)
#
#           years<-range(lubridate::year(yearly))
#
#           res<-c(rep(0, 1+years[2]-years[1]))
#           names(res)<-as.character(years[1]:years[2])
#
#           res[as.character(lubridate::year(yearly))]<-yearly[,1]
#
#         }else{
#
#           yearly<-getTemporalAvgSll(sb, scale)
#           years<-range(yearly[,"year"])
#
#           res<-c(rep(0, 1+years[2]-years[1]))
#           names(res)<-as.character(years[1]:years[2])
#
#           res[as.character(yearly$year)]<-as.data.frame(yearly)[,2]
#
#         }
#
#       }else{
#         yearly<-xts::apply.yearly(sb$temporal, nrow)
#
#         years<-range(lubridate::year(yearly))
#
#         res<-c(rep(0, 1+years[2]-years[1]))
#         names(res)<-as.character(years[1]:years[2])
#
#         res[as.character(lubridate::year(yearly))]<-yearly[,1]
#
#       }
#     }else if(variable=="nYears"){
#       stop("Variable nYears cannot be exported for timeRes=\"yearly\"")
#     }else{
#       stop(paste0("variable = \"", variable, "\" and method = \"", method, "\" is not an allowed combination."))
#     }
#
#
# ###Summarize monthly
#   }else if(scale == "monthly"){
#
#
#
#
# ###Summarize daily
#   }else if(scale == "daily"){
#
# ###Summarize month
#   }else if(scale == "month"){
#
# ###Else wrong input
#   }else{
#     stop("Wrong input for variable timeRes Try \"Yearly\", \"Monthly\", \"Daily\" or \"Month\" for dimension = \"Temporal\".")
#   }
#



#' Export single variables from SummarisedBirds objects
#'
#' @param x a SummarizedBirds-object
#' @param dimension a character string indicating if the export should be
#'   \code{"Spatial"} or \code{"Temporal"}
#' @param timeRes A character string indicating what tempral resolution should
#'   be used. For Spatial export the function accepts \code{NULL, "Yearly", "Monthly"}
#'   or \code{"Month"}. For temporal export the function accepts \code{"Yearly",
#'   "Monthly", "Daily"} or \code{"Month"}.
#' @param variable a character string indicating which variable should be
#'   exported, \code{"nObs", "nvis", "nSpp"} or \code{"avgSll" }.
#'   For \code{scale = "Month"} the function also accepts \code{"nYears"} and for all except
#'   \code{scale = "Daily"} the function accepts \code{"nDays"}.
#' @param method Only applicable to \code{timeRes = "Month"}. A variable specifying which
#'   statistical method should be applied. The function accepts \code{"sum", "median", "mean"}.
#' @note the difference between Monthly and Month is that the former returns
#' "n.years x 12" values, while Month summarize over years and returns only 12 values.
#' For more details over the possible combinations of dimensions and variables please
#' refer to the vignette "Technical details".
#' @return a named vector or a SpatialPolygonsDataFrame depending on the
#' dimension, temporal or spatial, respectively
#' @export
#'
#' @examples
#' grid<-makeGrid(searchPolygon, gridSize = 10)
#' SB <- summariseBirds(organizeBirds(bombusObs), grid=grid)
#' EB <- exportBirds(SB, "Spatial", "Month", "nDays", "median")

exportBirds <- function(x, dimension, timeRes, variable, method="sum"){

  dimension<-tolower(dimension)
  if(!is.null(timeRes)){
    timeRes<-tolower(timeRes)
  }
  variable<-tolower(variable)
  if(any(variable==c("nobs", "nvis", "nspp", "nyears", "ndays"))){
    variable<-paste0(substr(variable,1,1),toupper(substr(variable,2,2)), substr(variable,3,nchar(variable)))
  }else if(variable=="avgsll"){
    variable<-"avgSll"
  }else{
   stop("No valid variable")
  }

  if(!any(method==c("sum", "mean", "median"))){
    stop("Not a valid method")
  }

  if(dimension == "spatial"){
    res<-exportSpatial(x, timeRes, variable, method)
  }else if(dimension == "temporal"){
    res<-exportTemporal(x, timeRes, variable, method)
  }else{
    stop("Wrong input for variable dimension Try \"Spatial\" or \"Temporal\".")
  }

  return(res)


}
