#' Organize the date-column(s)
#'
#' Organize the date-column(s) in a dataframe to three columns
#'
#' @param x A dataframe with at least the columns specified in cols
#' @param cols A character vector with the column names for the dates specified.
#'   It can either be one column formatted as "yyyy-mm-dd" or a vector of
#'   length=3. If the column names are "year", "month" and "day" it will take
#'   these column names. Otherwise, it will take the column names and interpret
#'   the first as year, the second as month and the third as day.
#'
#' @return A data.frame with the columns "year", "month", "day"
#' @export
#' @examples
#' ymd<-as.Date(Sys.Date())+1:5
#' organizeDate(as.data.frame(ymd), "ymd")
#' @keywords internal
organizeDate <- function(x, cols){
  if (!length(cols) %in% c(1,3)) stop("Could not create date, please specify wither one or three column names")
  stdTimeCols<-c("year", "month", "day")

  if (all(cols %in% colnames(x))){
    if(ncol(x)>1) x<-x[,cols]
    colnames(x)<-tolower(colnames(x))

    cols<-tolower(cols)

    if(length(cols)==3){
      if(all(stdTimeCols %in% cols)){
        # print("just keep going")
      } else if(any(stdTimeCols %in% cols)){
        wColNot<-which(!(cols %in% stdTimeCols))
        for(i in 1:length(wColNot)){
          x$placeholder <- as.matrix(x[, cols[wColNot]])
          names(x)[names(x) == "placeholder"] <- stdTimeCols[wColNot[i]]
        }
      } else { #if(!any(stdTimeCols %in% cols)){
        x$year  <- x[,cols[1]]
        x$month <- x[,cols[2]]
        x$day   <- x[,cols[3]]
      }

      x$day<-ifelse(is.na(x$day), 1, x$day)
      x$month<-ifelse(is.na(x$month), 1, x$month)
      return(x)
    }
    if(length(cols) == 1){
      dateVector<-array(dim=c(nrow(x), 3), dimnames = list(c(), stdTimeCols))
      dateYMD <- as.Date(x[,cols])

      x$year  <- lubridate::year(dateYMD)
      x$month <- lubridate::month(dateYMD)
      x$day   <- lubridate::day(dateYMD)

      return(x)
    }
  } else {
    stop("One or more specified column names are not present in the input data frame")
  }
}


#' Simplify species names
#'
#' Removes infraspecific epithets, authors and years from scientific names
#'
#' @param df A dataframe with at least the column specified in sppCol
#' @param sppCol A character vector with the column names for the species names.
#' @return A vector vith data.frame with a scientific names up to specific names
#' @export
simplifySpp <- function(df, sppCol){
  splitLits<-strsplit(as.character(df[, sppCol]), "\ ")

  simpleNamesList<-lapply(splitLits, FUN=function(x){
    if (length(x)==1){ # Only genus
      return(x)
    }
    if (length(x)>=2){ # Genus and more
      if(grepl("(?!-)(?!/)[[:punct:]]|[A-Z]", x[2], perl = TRUE)){ # Genus and more (find punctuation except for - and /)
        return(x[1])
      } else {
        return(paste(x[1:2], collapse=" ")  ) # only species
      }
    }
  } )
  simpleSpp <- unlist(simpleNamesList)
  return(simpleSpp)
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
#' default value for the variable column is therefore that a visit is identified
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
#' @examples
#' OB <- organizeBirds(bombusObs)
#' tmp.vis <- createVisits(bombusObs, columns=c("locality", "day", "month", "year"))
#' visits(OB, name = "visNoRecorder", useAsDefault = TRUE) <- tmp.vis
createVisits<-function(x, columns=c("locality", "day", "month", "year", "recordedBy")){

  if(any(class(x)=="data.frame")){
    return(as.integer(factor(apply(x[columns], 1, paste0, collapse=""))))
  }else if (class(x)=="OrganizedBirds"){
    return(as.integer(factor(apply(x$spdf@data[columns], 1, paste0, collapse=""))))
  }else{
    stop("x must be a 'data.frame' or an 'OrganizedBirds'")
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
#' @rdname visits
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
#' @return A dataframe
#' @export
#'
#' @examples
#' ob<-organizeBirds(bombusObs)
#' obsData(ob)
obsData<-function(x){
  UseMethod("obsData")
}

#' @rdname obsData
#' @export
obsData.OrganizedBirds<-function(x){

  return(x$spdf@data)

}

#TODO add an example to organizeBirds

#'Organize a dataframe to a usable format
#'
#'Takes a dataframe with reported species observations and reformats it, using
#' visit identifiers, to an OrganizedBirds-class that can be used in further
#' analyses with the BIRDS-package.
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
#' @param sppCol A character string with the column name for the column for the
#' species names. Default is the Darwin Core standard name \code{"scientificName"}.
#'@param timeCol A character vector with the names for the column(s) holding the
#' observation dates. Default is the Darwin Core standard column names
#' \code{c("year", "month", "day")}.
#' @param visitsIdentifier A character vector of the names for the columns that
#'  are holding the information that identifies a visit. Default is the Darwin
#'  Core standard column names \code{c("locality", "day", "month", "year",
#'  "recordedBy")}.
#' @param presenceCol A character string with the column name for the column for the
#' precense status. Default is \code{NULL}.
#' @param xyCols A character vector of the names for the columns that are holding
#'  the coordinates for the observations. The order should be longitude(x),
#'  latitude(y). Default is the Darwin Core standard column names
#'  \code{c("decimalLongitude", "decimalLatitude")}. Only applicable to non-
#'  spatial dataframes.
#' @param dataCRS A character string for the dataframe CRS (Coordinate Reference
#'  System). Default is \code{"+init=epsg:4326"}, which is WGS 84. This is only
#'  applicable to non-spatial dataframes, since a spatial dataframes already
#'  should have this information.
#' @param taxonRankCol the name of the column containing the taxonomic rank for
#' the observation.
#' That is the minimum taxonomic identification level.
#' @param taxonRank a string or vector of strings containing the taxonomic ranks to keep.
#' Only evaluated if taxonRankCol is not \code{NULL}
#' @param simplifySppName wheter to remove everything else that is not the species
#' name (authors, years and intraspecific epithets). Default set to FALSE
#'
#' @return a `SpatialPointsDataFrame` wrapped into an object of class OrganizedBirds, with additional attributes.
#' @export
#'
#' @examples organizeBirds(bombusObs)
#' @seealso \code{\link{createVisits}} to create unique visits IDs,
#'  \code{\link{visits}} to get or set the visit IDs to this class,
#'  \code{\link{simplifySpp}} to simplify species names,
#'  \code{\link{obsData}} to retrieve the dataframe from this class.
#' @aliases organiseBirds
organizeBirds<-function(x,
                        sppCol = "scientificName",
                        timeCol = c("year", "month", "day"),
                        visitsIdentifier = c("locality", "day", "month", "year", "recordedBy"),
                        presenceCol = NULL,
                        xyCols = c("decimalLongitude", "decimalLatitude"),
                        dataCRS = "+init=epsg:4326",
                        taxonRankCol=NULL,
                        taxonRank=c("SPECIES","SUBSPECIES","VARIETY"),
                        simplifySppName=FALSE){

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

  if (any(duplicated(colnames(df)))){
    stop("There are duplicated column names in the dataset")
  }

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
  if (!is.null(simplifySppName) && simplifySppName == TRUE){
    df[, sppCol] <- simplifySpp(df, sppCol)
  }

  time<-organizeDate(df, timeCol)

  sp<-df[sppCol]

  visitUID<-createVisits(df, visitsIdentifier)

  if (is.null(presenceCol)){
    df<-cbind(sp, time, visitUID)
  } else {
    presence <- df[, presenceCol]
    presence <- ifelse(presence>=1, 1, 0)
    df <- cbind(sp, time, visitUID, presence)
  }

  colnames(df)[1]<-"scientificName"

  x@data<-df

  ## clean unreadable dates
  wNA <- is.na(df$year)
  if (sum(wNA)>1){
    x <- x[-wNA,]
    print(paste(length(wNA), " records deleted"))
  }

  res<-list(x)

  names(res)<-"spdf"

  class(res)<-c("OrganizedBirds")
  attr(res, "visitCol")<-"visitUID"

  return(res)

}
#' @rdname organizeBirds
#' @export
organiseBirds<-organizeBirds ## To include the brits as well
