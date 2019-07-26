#' Organize the date-column(s)
#'
#' Organize the date-column(s) in a dataframe to three columns
#'
#' @param date A dataframe with at least the columns specified in cols
#' @param cols A character vector with the column names for the dates specified.
#'   It can either be one column formatted as "yyyy-mm-dd" or a vector of
#'   length=3. If the vector is named as "year", "month", "day" it will take
#'   these column names. Otherwise, it will take the column names and interpret
#'   the first as year, the second as month and the third as day.
#'
#' @return A data.frame with the columns "year", "month", "day"
#' @export
#' @examples
#' ymd<-as.Date(Sys.Date())+1:5
#' organizeDate(as.data.frame(ymd), "ymd")
#' @keywords internal
organizeDate <- function(date, cols){

  names(cols)<-tolower(names(cols))

  dateVector<-array(dim=c(nrow(date), 3), dimnames = list(c(),c("year", "month", "day")))

  if(length(cols)==3){
    if(any(names(cols) == "year", na.rm = TRUE) & any(names(cols) == "month", na.rm = TRUE) & any(names(cols) == "day", na.rm = TRUE)){
      dateVector[,"year"]<-date[cols["year"]][[1]]
      dateVector[,"month"]<-date[cols["month"]][[1]]
      dateVector[,"day"]<-date[cols["day"]][[1]]
    }else{
      dateVector[,"year"]<-date[cols[1]][[1]]
      dateVector[,"month"]<-date[cols[2]][[1]]
      dateVector[,"day"]<-date[cols[3]][[1]]
    }

    date<-apply(dateVector, 1, paste0, collapse = "-")

    date<-array(date, dim=c(length(date), 1), dimnames = list(c(),c("ymd")))

    cols<-"ymd"

  }

  if(length(cols)==1){

    res<-array(dim= c(0,3), dimnames = list(c(),c("year", "month", "day")))

    for(i in 1:nrow(date)){

      d<-tryCatch(as.Date(date[i,cols[1]]),
                  error=function(e){
                    stop(paste0("Could not create date for row: ",i))
                  })
      if(d==0){
        stop("Could not create date")
      }

      res<-rbind(res,c(lubridate::year(d), lubridate::month(d), lubridate::day(d)))


    }

    return(res)

  }else{
    stop("Could not create date")
  }

}

### HANDLE THE VISITS ###

#' Create unique visits IDs
#'
#' Takes a dataframe and a vector of column names and classifies each row of the
#' dataframe based on the combination of values in the specified columns.
#'
#' What a visit should be is not always clearly defined and extractable in a
#' dataset. A reasonable assumption is that a visit could be identified from the records
#' made by one person on a certain day and at a specific location or site. The
#' default value for the variable column is therefor that a visit is identified
#' by the Darwin Core variables \code{c("locality", "day", "month", "year",
#' "recordedBy")}.
#'
#' @param x a dataframe or OrganizedBirds-class including at least the
#'   specified columns that are used to identify a visit.
#' @param columns A vector with the names of the columns that are used to identify a visit.
#'   Default is the Darwin Core variables \code{c("locality", "day", "month",
#'   "year", "recordedBy")}.
#'
#' @return A vector of the same length as the number of rows as the dataframe
#'   with a unique number for each combination of the values in the specified
#'   columns.
#' @export
#'
#' @examples
#' OB <- organizeBirds(bombusObs)
#' tmp.vis <- createVisits(bombusObs, columns=c("locality", "day", "month", "year"))
#' visits(OB, name = "visNoRecorder", useAsDefault = TRUE) <- tmp.vis
createVisits<-function(x, columns=c("locality", "day", "month", "year", "recordedBy")){

  if(any(class(x)=="data.frame")){
    return(as.integer(factor(apply(x[columns], 1, paste0, collapse=""))))
  }else if (class(x)=="data.frame"){

  }else{

  }


}


#' Get/set the visits
#'
#' Gets or sets the visits identifier for a OrganizedBirds-class.
#'
#' @usage visits(x, name=NULL)
#' @aliases \sQuote{visits<-}
#' @param x An OrganizedBirds-object
#' @param name The name of the visit column. Default is \code{NULL}, which will
#'   get/write to the predefined visit column (\code{visitUID}).
#' @param useAsDefault Specifies if the defined column in \code{name} should be used as
#'   the default column for the visits in further analysis. If name is
#'   \code{NULL} and \code{useAsDefault = TRUE}, \code{value} will be written to
#'   column (\code{visitUID}) and that column will be set to default.
#'
#' @export
#'
#' @examples
#' ob<-organizeBirds(bombusObs)
#' attr(ob, "visitCol")
#' vis<-visits(ob)
#' tmp.vis <- createVisits(bombusObs, columns=c("locality", "day", "month", "year"))
#' visits(ob, name = "visNoRecorder", useAsDefault = TRUE) <- tmp.vis
#' vis2<-visits(ob)
#' attr(ob, "visitCol")
visits<-function(x, name=NULL){

  if(class(x)!="OrganizedBirds"){
    stop("Cannot get the visits from other than a OrganizedBirds-class")
  }

  if(is.null(name)){
    name<-attr(x, "visitCol")
  }

  return(x[[1]]@data[,name])


}

#' @rdname visits
#' @param value the value to assign
#' @export
'visits<-'<-function(x, name=NULL, useAsDefault = TRUE, value){

  if(is.null(name)){
    name<-"visitUID"
  }

  if(class(x)=="OrganizedBirds"){

    x[[1]]@data[,name]<-value

    if(useAsDefault){
      attr(x, "visitCol")<-name
    }

  }else if(length(dim(x)) == 2){
    if(any(colnames(x)==name)){
      x[,name]<-value
    }else{
      cName<-colnames(x)
      cName<-c(cName, name)
      x<-cbind(x, value)
      colnames(x)<-cName
    }

    if(useAsDefault){
      warning("Cannot set the default visit column for any object other than a OrganizedBirds. useAsDefault=TRUE will not be used.")
    }

  }else{
    stop("Cannot create a visit column to nothing other than a OrganizedBirds-class or a object with two dimensions")
  }

  return(x)

}

#' Extract observation data
#'
#' Extract the observation data from a OrganizedBirds-object
#'
#' @param x An OrganizedBirds-object
#' @param ... additional arguments for \code{\link{obsData}}
#'
#' @return A dataframe
#' @export
#'
#' @examples ob<-organizeBirds(bombusObs)
#' obsData(ob)
obsData<-function(x, ...){
  UseMethod("obsData")
}

#' Extract observation data
#' @param x An OrganizedBirds-object
#' @export
obsData.OrganizedBirds<-function(x){

  return(x$spdf@data)

}

#TODO add an example to organizeBirds

#'Organize a dataframe to a usable format
#'
#'Takes a dataframe with reported species observations and reformats it to a
#'OrganizedBirds-class that can be used in further analyses with the
#'BIRDS-package.
#'
#'An OrganizedBirds-class is essentially a list containing one element, a
#'SpatialPointsDataFrame. This SpatialPointsDataFrame has its data formatted in
#'a way that the other functions in the BIRDS-package can use further on. It
#'also has the attribute \code{"visitCol"}, which indicates which column in the
#'dataframe holds the visit identifier. The visit identifier is created by
#'the function \code{\link{createVisits}}, which creates a unique id for each
#'combination of the values in the defined columns.
#'
#'The variable \code{timeCol} can be formatted differently. If the variable is a
#'named vector with the names "Year", "Month" and "Day" (letter capitalization
#'does not matter) it will use the variable named year as the year column and so
#'on. Otherwise it will use the first variable as year, the second as month and
#'the third as day, if there is a vector of length three or more. If the vector
#'is of only length one it will interpret the column as a date column formatted
#'as "yyyy-mm-dd".
#'
#' @param x A dataframe or a SpatialPointsDataFrame containing at least a
#'  column for species name, one or several columns for date of observation, one or
#'  several columns for identifying a visit and, if it is not spatial, coordinate
#'  columns.
#' @param sppCol A character string of the column name for the column which is
#'  holding the species names. Default is the Darwin Core standard name
#'  \code{"scientificName"}.
#'@param timeCol A character vector (named or not) of the names for the
#'  column(s) which is holding the observation dates. Default is the Darwin Core
#'  standard column names \code{c("Year"="year", "Month"="month", "Day"="day")}.
#' @param visitsIdentifier A character vector of the names for the columns that
#'  are holding the information that identifies a visit. Default is the Darwin
#'  Core standard column names \code{c("locality", "day", "month", "year",
#'  "recordedBy")}.
#' @param xyCols A character vector of the names for the columns that are holding
#'  the coordinates for the observations. The order should be longitude(x),
#'  latitude(y). Default is the Darwin Core standard column names
#'  \code{c("decimalLongitude", "decimalLatitude")}. Only applicable to non-
#'  spatial dataframes.
#' @param dataCRS A character string for the dataframe CRS (Coordinate Reference
#'  System). Default is \code{"+init=epsg:4326"}, which is WGS 84. This is only
#'  applicable to non-spatial dataframes, since a spatial dataframes already
#'  should have this information.
#' @param taxonRankCol the name of the column containing the taxonomic rank for the observation.
#' That is the minimum taxonomic identification level.
#' @param taxonRank a string or vector of strings containing the taxonomic ranks to keep.
#' Only evaluated if taxonRankCol is not \code{NULL}
#' @param simplifySppName wheter to remove everything else that is not the species name (authors, years and intraspecific epithets). Default set to FALSE
#'
#' @return A OrganizedBirds-class.
#' @export
#'
#' @examples organizeBirds(bombusObs)
#' @seealso \code{\link{createVisits}} to create unique visits IDs,
#'  \code{\link{visits}} to get or set the visit IDs to this class,
#'  \code{\link{obsData}} to retrieve the dataframe from this class.
#' @aliases organiseBirds
organizeBirds<-function(x, sppCol = "scientificName", timeCol = c("Year"="year", "Month"="month", "Day"="day"),
                        visitsIdentifier = c("locality", "day", "month", "year", "recordedBy"),
                        xyCols=c("decimalLongitude", "decimalLatitude"), dataCRS = "+init=epsg:4326",
                        taxonRankCol=NULL, taxonRank=c("SPECIES","SUBSPECIES","VARIETY"), simplifySppName=FALSE){

  # Check the type of data
  if(any(class(x) == "data.frame")){
    x<-as.data.frame(x)
    sp::coordinates(x) <- xyCols
    sp::proj4string(x) <- dataCRS
  }else if(any(class(x) == "SpatialPointsDataFrame")){
    ## Just continue... :)
  }else{
    stop("Unknown format for argument 'x'. The variable should be of class data.frame or SpatialPointsDataFrame.")
  }

  if(sp::proj4string(x)!="+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"){
    x<-sp::spTransform(x, sp::CRS("+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  }

  df<-x@data

  # Check if user wants to leave a certain level
  if (!is.null(taxonRankCol)){
    if (taxonRankCol %in% colnames(df)){
      wIn<-which(df[, taxonRankCol] %in% taxonRank)
      nOut<-nrow(df)-length(wIn)
      if (length(wIn)>0){
        x<-x[wIn,]
        df<-df[wIn,]
      }
      message(paste0(nOut, " observations did not match with the specified taxon rank and were removed."))
    } else { stop(paste0("Taxon Rank: there is no column called ", taxonRankCol))}
  }

  # Simplify species names to reduce epitets and author names
  if (!is.null(simplifySppName)){
    splitLits<-strsplit(as.character(df[, sppCol]), "\ ")

    simpleNamesList<-lapply(splitLits, FUN=function(x){
      if (length(x)==1){ # Only genus
        return(x)
      }
      if (length(x)>=2){ # Genus and more
      #   return(paste(x[1:2], collapse=" ")  )
      # }
      # if (length(x)==2){
        if(grepl("(?!-)(?!/)[[:punct:]]|[A-Z]", x[2], perl = TRUE)){ # Genus and more (find punctuation except for - and /)
          return(x[1])
        } else {
          return(paste(x[1:2], collapse=" ")  ) # only species
        }
      }
    } )
    df[, sppCol]<-unlist(simpleNamesList)
  }


  sp<-df[sppCol]

  time<-organizeDate(df[timeCol], timeCol)

  visitUID<-createVisits(df,visitsIdentifier)

  df<-cbind(sp, time, visitUID)

  colnames(df)[1]<-"scientificName"

  x@data<-df

  res<-list(x)

  names(res)<-"spdf"

  class(res)<-c("OrganizedBirds")
  attr(res, "visitCol")<-"visitUID"

  return(res)

}

organiseBirds<-organizeBirds ## To include the brits as well
