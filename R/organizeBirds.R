#' Find a text among column names
#'
#' Finds a text among column names and return it as it is
#'
#' @param pattern character string containing a string or regular expression
#' to be matched in the given character vector
#' @param df A data.frame among whichs column names to look for the pattern
#' @param exact A logical flag whether to return all matches or just that text.
#' This is always case insensitive.
#' @param value A logical flag (to be passed to grep()). If FALSE, a vector
#' containing the (integer) indices of the matches determined by grep is returned,
#' and if TRUE, a vector containing the matching elements themselves is returned.
#'
#' @return A vector with the column names that match the pattern
#' @export
#' @keywords internal
findCols <- function(pattern, df, exact=FALSE, value = TRUE){
  if (missing(pattern)) stop("The argument 'pattern' must be supplied.")
  if (missing(df)) stop("The argument 'df' must be supplied.")

  patternE <- if (exact) paste("^", pattern, "$", sep = "") else pattern

  if (length(patternE) == 1) {
    res <- grep(patternE, names(df), ignore.case = TRUE, value = value)
  }
  if (length(patternE) > 1) {
    res <- lapply(patternE, grep, names(df), ignore.case = TRUE, value = value)
  }
  return(res)
}

#' Organize the date-column(s)
#'
#' Organize the date-column(s) in a data.frame to three columns
#'
#' @param x A data.frame with at least the columns specified in 'columns'
#' @param columns A character vector with the column names for the dates specified.
#'   It can either be one column formatted as "yyyy-mm-dd" or a vector of
#'   length=3. If the column names are "year", "month" and "day" it will take
#'   these column names. Otherwise, it will take the column names and interpret
#'   the first as year, the second as month and the third as day.
#'
#' @return A data.frame with the columns "year", "month", "day"
#' @export
#' @examples
#' ymd<-as.Date(Sys.Date())+1:5
#' id<-1:5
#' organizeDate(data.frame("id"=id,
#'                         "ymd"=as.character(ymd)),
#'              "ymd")
#' @keywords internal
organizeDate <- function(x, columns){
  if (!length(columns) %in% c(1,3)) stop("Could not create date, please specify either one or three column names")

  stdTimeCols <- c("year", "month", "day")

  cols.df <- findCols(columns, x, exact = TRUE)

  if (all(lengths(cols.df) > 0)) {
    cols.df <- unlist(cols.df)

    if (ncol(x) > 1) x <- x[,cols.df]
    cols.df <- tolower(cols.df)

    if (length(cols.df) == 3) {
      colnames(x) <- tolower(colnames(x))
      if (all(stdTimeCols %in% cols.df)) {
        ## all is just fine
        x <- x[,stdTimeCols]
        return(x)
      } else {
        ### This part here assumes the 3 defined columns are in the right order
        ### and gives the standard name
        message("This function assumes that the 3 given column names represent 'year', 'month' and 'day' in that specific order.")
        if (any(stdTimeCols %in% cols.df)) {

          wColNot <- which(!(stdTimeCols %in% cols.df))
          for (i in 1:length(wColNot)) {
            # x$placeholder <- as.matrix(x[, cols.df[wColNot[i]]])
            x$placeholder <- x[, cols.df[wColNot[i]]]
            names(x)[names(x) == "placeholder"] <- stdTimeCols[wColNot[i]]
          }

        } else {
          ## if none of the columns names are standard then makes them standard
          x$year  <- x[,cols.df[1]]
          x$month <- x[,cols.df[2]]
          x$day   <- x[,cols.df[3]]
        }
      }

      ## empty years are not tolerated! but days and months are given value 1
      if (sum(is.na(x$day)) > 0) message(paste("There were", sum(is.na(x$day)),"empty days that were given the value 1"))
      if (sum(is.na(x$month)) > 0) message(paste("There were", sum(is.na(x$month)),"empty months that were given the value 1"))
      x$day <- ifelse(is.na(x$day), 1, x$day)
      x$month <- ifelse(is.na(x$month), 1, x$month)
      res <- x
    }

    if (length(cols.df) == 1) {
      res <- data.frame(matrix(nrow = length(x), ncol = length(stdTimeCols)))
      colnames(res) <- stdTimeCols
      dateYMD <- as.Date(x)

      res$year  <- lubridate::year(dateYMD)
      res$month <- lubridate::month(dateYMD)
      res$day   <- lubridate::day(dateYMD)

      # res
    }
  } else {
    stop("One or more specified column names are not present in the input data set.")
  }
  ## clean unreadable dates (cleaning also the spatial points)
  wNA <- is.na(res)
  if (sum(wNA) > 1) {
    res <- res[-wNA,]
    message(paste(length(wNA), " records deleted because the date was unreadable."))
  }
  return(res)
}


#' Simplify species names
#'
#' Removes infraspecific epithets, authors and years from scientific names
#'
#' @param df A data.frame with at least the column specified in sppCol
#' @param sppCol A character vector with the column names for the species names.
#' @return A vector with data.frame with a canonical name given by taxize::gbif_parse(),
#' that is a scientific name with up to 3 elements and no authorship
#' @importFrom taxize gbif_parse
#' @export
simplifySpp <- function(df, sppCol){

  gbifdf <-  gbif_parse(df[, sppCol])
  simpleSpp <- gbifdf$canonicalname

  # splitLits <- strsplit(as.character(df[, sppCol]), "\ ")
  #
  # simpleNamesList<-lapply(splitLits, FUN=function(x){
  #   if (length(x) == 1){ # Only genus
  #     return(x)
  #   }
  #   if (length(x) >= 2){ # Genus and more
  #     if(grepl("(?!-)(?!/)[[:punct:]]|[A-Z]", x[2], perl = TRUE)){ # Genus and more (find punctuation except for - and /)
  #       return(x[1])
  #       # TODO cases like #Falco peregrinus, 1771" will wrongly end like "Falco"
  #       # but without this line it will wrongly take Falco Lineus, 1771 as Falco Lineus
  #       # is the key on the capital letter?
  #     } else {
  #       return(paste(x[1:2], collapse=" ")  ) # only species
  #     }
  #   }
  # } )
  # simpleSpp <- unlist(simpleNamesList)

  return(simpleSpp)
}


#### HANDLE THE VISITS ####
#' Create unique IDs based on a grid
#'
#' Takes a spatial points data.frame and a grid and gets the overlay IDs.
#'
#' @param x a sf ('sf') or a SpatialPointsDataFrame ('sp') (with the observations).
#' @param grid a sf ('sf') or a SpatialPolygon object ('sp') (a grid is expected) 
#' defining the maximum extent of visits effort.
#' @param idcol column name for the grid names or ids
#'
#' @return A vector of the same length as the number of rows (observations) as x
#'   with a unique number corresponding to the grid's ID.
#'
#' @export
getGridIDs <- function(x, grid, idcol="id"){
  if (!inherits(x, c("SpatialPointsDataFrame", "sf"))) {
    stop("The argument 'x' can only be of class sf or SpatialPointsDataFrame")
  }

  if (inherits(x, "SpatialPointsDataFrame")) x <- st_as_sf(x)

  if (!inherits(grid, c("SpatialPolygonsDataFrame", "SpatialPolygons", "sf"))) {
    stop("The argument 'grid' can only be of class sf, SpatialPolygonsDataFrame or SpatialPolygons")
  }

  if (inherits(grid, c("SpatialPolygonsDataFrame", "SpatialPolygons"))) grid <- st_as_sf(grid)

  if (is.null(idcol)) idcol = "id"
  #### Rename grid
  if (is.null(colnames(grid))) {
    grid <- st_sf(data.frame(paste0("ID", seq(length(grid))), st_geometry(grid)))
    colnames(grid)[1] <- idcol
  } else {
    if (!idcol %in% colnames(grid)) stop(paste0("The column '",idcol,"' was not found in the grid"))
    if (any(duplicated(grid[,idcol]))) {
      grid <- renameGrid(grid, idcol)
      warning("There are duplicated cell names in your grid. We rename them internally to 'ID1'...'IDn'.
All results will use this nomenclature, but the order of the cells will remain unaltered.")
    }
  }

  x <- st_transform(x,
                    crs = st_crs(3857))
  grid <- st_transform(grid,
                       crs = st_crs(3857))
  inter <- st_intersects(x, grid)
  res <- sapply(inter,
                function(z) if (length(z) == 0) NA_integer_ else z[1])
  return(res)

}



#' Create unique visits IDs
#'
#' Takes a data.frame and a vector of column names and classifies each row of the
#' data.frame based on the combination of values in the specified columns.
#'
#' What a visit should be is not always clearly defined and extractable in a
#' dataset. A reasonable assumption is that a visit could be identified from the
#' records made by one person on a certain day and at a specific location or site.
#' The default value for the variable column is therefore that a visit is identified
#' by the Darwin Core variables \code{c("locality", "day", "month", "year",
#' "recordedBy")}.
#'
#' @param x An object of class \code{data.frame}, \code{sf} or \code{SpatialPointsDataFrame}
#' including at least the columns specified that are used to identify a visit.
#' @param idCols A vector with the names of the columns other (than time columns)
#' that are used to identify a visit. This variable cannot be empty. At least the
#' recorders name or any other ID must be provided. Default is the Darwin Core
#' variables \code{c("locality", "recordedBy")}.
#' @param timeCols A vector with the names of the time columns that are used to
#' identify a visit.  If timeCols=NULL then time is ignored to create a visit ID.
#' Default is the Darwin Core variables \code{c("day", "month", "year")}.
#' @param grid Either \code{NULL} to be ignored or an object of class
#' \code{sf}, \code{SpatialPolygons} or \code{SpatialPolygonsDataFrame} defining
#' the maximum extent of visits effort. Then x must be an object of class sf
#' @param gridIdCol column in grid with ids.
#'
#' @return A vector of the same length as the number of rows as the data.frame
#'   with a unique number for each combination of the values in the specified
#'   columns.
#' @export
#' @examples
#' OB <- organizeBirds(bombusObs)
#' tmp.vis <- createVisits(bombusObs,
#'                         idCols=c("locality", "recordedBy"),
#'                         timeCols=c("day", "month", "year"),
#'                         gridIdCol = "id")
#' visits(OB, name = "visNoRecorder", useAsDefault = TRUE) <- tmp.vis
createVisits <- function(x,
                         idCols = c("locality", "recordedBy"),
                         timeCols = c("day", "month", "year"),
                         grid = NULL,
                         gridIdCol){

  if (inherits(x, c("data.frame", "SpatialPointsDataFrame", "sf"))) {
    if (inherits(x, "sf")) {
      df <- st_drop_geometry(x)
      sfdf <- x
    } else if (inherits(x, "SpatialPointsDataFrame")) {
      df <- x@data
      sfdf <- st_as_sf(x)
    } else if (inherits(x, "data.frame")) {
      df <- as.data.frame(x) ## in case it is a data.table or some other weird class
      sfdf <- NULL
    }

    if (all(idCols == "")) idCols <- NULL
    if (all(timeCols == "")) timeCols <- NULL

    if (!is.null(grid)) {
      if (inherits(grid, c("SpatialPolygons", "SpatialPolygonsDataFrame", "sf"))
         & !is.null(sfdf)) {
        df[,"gridID"] <- getGridIDs(sfdf, grid, gridIdCol)
      } else if (inherits(grid, c("character", "numeric"))) {
        if (length(grid) == nrow(df)) {
          df[,"gridID"] <- grid
        } else stop("If grid is a vector it should be as long as the number of observations.")
      } else stop("The argument 'grid' should be a geometry or a vector with IDs.")

      gridID <- "gridID"

    } else {
      gridID <- NULL
    }

    columns <- c(gridID, idCols, timeCols)
    if (length(columns) == 0) stop("At least one of the arguments 'idCols','timeCols','grid' needs to be defined.")

    cols.df <- findCols(columns, df, exact = TRUE)

    if (all(lengths(cols.df) > 0)) {
      cols.df <- unlist(cols.df)
      res <- as.integer(factor(apply(df[cols.df], 1, paste0, collapse = "")))
      return(res)
    } else {
      stop("Some or any of the column names were not found in the data set.")
    }

  } else {
    stop("Argument 'x' must be a 'data.frame' or an 'OrganizedBirds'")
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
#' get/write to the predefined visit column (\code{visitUID}).
#' @param useAsDefault Specifies if the defined column in \code{name} should be 
#' used as the default column for the visits in further analysis. If name is
#' \code{NULL} and \code{useAsDefault = TRUE}, \code{value} will be written to
#' column (\code{visitUID}) and that column will be set as default.
#' @param value the value to assign
#' @return Either extracts a vector with visits IDs from an object of class 
#' 'OrganizedBirds' if used as \code{visits()}, or it binds a column with visit 
#' IDs to the spatial data.frame within the OrganizedBirds-object if used as 
#' \code{'visits<-'}
#' @export
#' @examples
#' ob <- organizeBirds(bombusObs)
#' attr(ob, "visitCol")
#' vis <- visits(ob)
#' tmp.vis <- createVisits(bombusObs,
#'                         idCols=c("locality"),
#'                         timeCols = c("day", "month", "year"),
#'                         gridIdCol = "id")
#' visits(ob, name = "visNoRecorder", useAsDefault = TRUE) <- tmp.vis
#' vis2 <- visits(ob)
#' attr(ob, "visitCol")
visits <- function(x, name=NULL) {

  if (!inherits(x, "OrganizedBirds")) {
    stop("Cannot get the visits from other than a OrganizedBirds-class")
  }

  if (is.null(name)) {
    name <- attr(x, "visitCol")
  }

  return(st_drop_geometry(x[[1]][,name]))
}

#' @rdname visits
#' @export
"visits<-" <- function(x,
                       name=NULL,
                       useAsDefault = TRUE,
                       value){

  if (is.null(name)) {
    name <- "visitUID"
  }

  if (inherits(x, "OrganizedBirds")) {

    x$spdf[,name] <- value

    if (useAsDefault) {
      attr(x, "visitCol") <- name
    }

  }else if (length(dim(x)) == 2) {
    if (any(colnames(x) == name)) {
      x[,name] <- value
    }else{
      cName <- colnames(x)
      cName <- c(cName, name)
      x <- cbind(x, value)
      colnames(x) <- cName
    }

    if (useAsDefault) {
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
#' @return A data.frame
#' @export
#'
#' @examples
#' ob <- organizeBirds(bombusObs)
#' head(obsData(ob))
obsData <- function(x) {
  UseMethod("obsData")
}

#' @rdname obsData
#' @export
obsData.OrganizedBirds <- function(x) {
  if (inherits(x$spdf, "SpatialPointsDataFrame")) return(x$spdf@data)
  if (inherits(x$spdf, "sf")) return(st_drop_geometry(x$spdf))
}


#### Main Function #####

#'Organize a data.frame to a usable format
#'
#'Takes a data.frame with reported species observations and reformats it, using
#' visit identifiers, to an OrganizedBirds-class that can be used in further
#' analyses with the BIRDS-package.
#'
#'An OrganizedBirds-class is essentially a list containing a spatial element.
#'After version 0.2, the resulting element is of class \code{sf}. However, we
#'now add a parameter for backwards compatibility. It also accepts the inputs
#'in both formats. This sf has its data formatted in a way that the other
#'functions in the BIRDS-package can use further on. It also has the attribute
#'\code{"visitCol"}, which indicates which column in the data.frame holds the
#'visit identifier. The visit identifier is created by the function
#'\code{\link{createVisits}}, which creates a unique id for each combination of
#'the values in the defined columns.
#'
#'The variable \code{timeCol} can be formatted differently. If the variable is a
#'named vector with the names "Year", "Month" and "Day" (letter capitalization
#'does not matter) it will use the variable named year as the year column and so
#'on. Otherwise it will use the first variable as year, the second as month and
#'the third as day, if there is a vector of length three or more. If the vector
#'is of only length one it will interpret the column as a date column formatted
#'as "yyyy-mm-dd".
#'
#' @param x A data.frame, sf or a SpatialPointsDataFrame containing at least a
#'  column for species name, one or several columns for date of observation, one or
#'  several columns for identifying a visit and, if it is not spatial, coordinate
#'  columns.
#' @param sppCol A character string with the column name for the column for the
#' species names. Default is the Darwin Core standard name \code{"scientificName"}.
#' @param idCols A character vector of the names for the columns that
#'  are holding the information that identifies a visit. Default is the Darwin
#'  Core standard column names \code{c("locality", "day", "month", "year",
#'  "recordedBy")}.
#' @param timeCols A character vector with the names for the column(s) holding the
#' observation dates. Default is the Darwin Core standard column names
#' \code{c("year", "month", "day")}.
#' @param timeInVisits A flag indicating whether visits are defined by
#' the time definition or not, and to which resolution. Default is 'day'.
#' Alternatives are \code{c("day", "month", "year", NULL)}. Time is anyhow
#' organised into three columns year, month, day.
#' @param grid Either \code{NULL} to be ignored or an object of class
#' \code{SpatialPolygons} or \code{SpatialPolygonsDataFrame} as identifier of
#' the visits spatial extent.
#' @param presenceCol A character string with the column name for the column for the
#' presence status. Default is \code{NULL}.
#' @param xyCols A character vector of the names for the columns that are holding
#'  the coordinates for the observations. The order should be longitude(x),
#'  latitude(y). Default is the Darwin Core standard column names
#'  \code{c("decimalLongitude", "decimalLatitude")}. Only applicable to non-
#'  spatial data.frames.
#' @param dataCRS A character string or numeric for the data.frame CRS (Coordinate Reference
#'  System). Default is \code{4326}, which is WGS 84. This is only
#'  applicable to non-spatial data.frames, since a spatial data.frames already
#'  should have this information.
#' @param taxonRankCol the name of the column containing the taxonomic rank for
#' the observation.
#' That is the minimum taxonomic identification level.
#' @param taxonRank a string or vector of strings containing the taxonomic ranks to keep.
#' Only evaluated if taxonRankCol is not \code{NULL}
#' @param simplifySppName Logical. whether to remove everything else that is not the species
#' name (authors, years). Default set to FALSE, else leaves a canonical name given
#' by taxize::gbif_parse(), that is a scientific name with up to 3 elements.
#' @param spOut Logical. Whether the result should be a SpatialPolygon (sp) or an sf.
#'
#' @import sf
#' @importFrom stats IQR median na.omit  quantile var
#' @importFrom grDevices boxplot.stats
#' @importFrom graphics barplot layout legend mtext par plot
#' @return a `SpatialPointsDataFrame` wrapped into an object of class OrganizedBirds,
#' with additional attributes.
#' @export
#'
#' @examples OB <- organizeBirds(bombusObs)
#' @seealso \code{\link{createVisits}} to create unique visits IDs,
#'  \code{\link{visits}} to get or set the visit IDs to this class,
#'  \code{\link{simplifySpp}} to simplify species names,
#'  \code{\link{obsData}} to retrieve the data.frame from this class.
#' @aliases organiseBirds
organizeBirds <- function(x,
                          sppCol = "scientificName",
                          idCols = c("locality", "recordedBy"),
                          timeCols = c("year", "month", "day"),
                          timeInVisits = "day",
                          grid = NULL,
                          presenceCol = NULL,
                          xyCols = c("decimalLongitude", "decimalLatitude"),
                          dataCRS = 4326,
                          taxonRankCol = NULL,
                          taxonRank = c("SPECIES","SUBSPECIES","VARIETY"),
                          simplifySppName = FALSE,
                          spOut = FALSE){

  crs <- st_crs(as.numeric(dataCRS))
  stdTimeCols <- c("year", "month", "day")

  # Check the type of data
  if (inherits(x, "data.frame")) {
    x <- as.data.frame(x)

    xyColsl.df <- unlist(findCols(xyCols, x))
    if (length(xyColsl.df) == 0) stop("The column names defined for the coordinates could not be found in the data set")
    if (length(xyColsl.df) == 1) stop("The column names defined for the coordinates must be two. Check your values")

    if (length(xyColsl.df) > 0) {
      if (length(xyColsl.df) > 2) { ## if too many matches try exact=TRUE
        xyColsl.df <- unlist(findCols(xyCols, x, exact = TRUE))
        if (length(xyColsl.df) == 0) stop("The column names defined for the coordinates could not be found in the data set")
      }
      x <- st_as_sf(x, coords = xyColsl.df)
      st_crs(x) <- st_crs(as.numeric(dataCRS))
      xdf <- st_drop_geometry(x)

    ### TODO Add message if CRS is not compatible with coordinates?? Do it with try.catch

    } else { stop("The column names defined for the coordinates could not be found in the data set")}
  } else if (inherits(x, "SpatialPointsDataFrame")) {
    x <- st_as_sf(x)
    xdf <- st_drop_geometry(x)
  } else if (inherits(x, "sf")) {
    xdf <- st_drop_geometry(x)## Just continue... :)
  } else {
    stop("The argument 'x' should be of class data.frame, sf or SpatialPointsDataFrame.")
  }

  if (st_crs(x) != crs) {
    x <- st_transform(x, crs = crs)
  }

  ### Check the column names
  if (any(duplicated(tolower(names(x))))) {
    stop("There are duplicated column names in the dataset (Note: case insensitive check).")
  }

  # Check if user wants to leave a certain level
  if (!is.null(taxonRankCol)) {
    TRCol.df <- findCols(taxonRankCol, x, exact = TRUE)
    if (length(TRCol.df) > 0) {
      exact.taxonRank <- paste0("\\b", taxonRank, "\\b") ## exact match
      wIn <- unique(
                unlist(
                  lapply(exact.taxonRank,
                         grep,
                         st_drop_geometry(x)[, TRCol.df],
                         ignore.case = TRUE,
                         value = FALSE
                        )
                )
              )

      if (length(wIn) > 0) {
        nOut <- nrow(x) - length(wIn)
        x <- x[wIn,]
        if (nOut > 0) message(paste0(nOut, " observations did not match with the specified taxon rank and were removed."))
      } else { stop(paste0("No observation match with the specified taxon rank(s).")) }
    } else { stop(paste0("Taxon Rank: there is no column called ", taxonRankCol))}
  }

  # Simplify species names to reduce epithets and author names
  sppCol.df <- findCols(sppCol, x, exact = TRUE)
  if (length(sppCol.df) > 0) {
    if (!is.null(simplifySppName) && simplifySppName == TRUE) {
      x[, sppCol.df] <- simplifySpp(st_drop_geometry(x), sppCol.df)
    }
  } else { stop(paste0("Species name: there is no column called ", sppCol))}

  ## column name control defined in the function organizeDate()
  x[, stdTimeCols] <- organizeDate(st_drop_geometry(x), timeCols)


  ## colum name control defined in the function visitUID()
  ## Time is optional in the visits
  if (is.null(timeInVisits)) {
    timeColsVis <- timeInVisits
  } else {
    timeColsVis <- switch(timeInVisits,
                          "day" = c("year", "month", "day"),
                          "month" = c("year", "month"),
                          "year" = "year") ## Else NULL
  }

  x[,"visitUID"] <- createVisits(x,
                                 idCols = idCols,
                                 timeCols = timeColsVis,
                                 grid = grid,
                                 gridIdCol = NULL)

  #### Preparing the output as we want it
  res.df <- x[,c(sppCol.df, stdTimeCols, "visitUID", "geometry")]

  if (!is.null(presenceCol)) {
    presenceCol.df <- findCols(presenceCol, x)
    if (length(presenceCol.df) > 0) {
      presence <- x[, presenceCol.df]
      presence <- ifelse(presence >= 1, 1, 0)
      res.df[,"presence"] <- presence
    } else {stop(paste0("Presence: there is no column called ", presenceCol))}
  }

  colnames(res.df)[1] <- "scientificName"

  #### Add the visits SLL to each visits
  x <- res.df
  if (spOut) x <- as_Spatial(x)

  res <- list(x)

  names(res) <- "spdf"

  class(res) <- c("OrganizedBirds")
  attr(res, "visitCol") <- "visitUID"

  return(res)
}

#' @rdname organizeBirds
#' @export
organiseBirds <- organizeBirds ## To include the Brits as well
