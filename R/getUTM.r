#' internal function. Depends on the polygon object utmZones
#' @param sf a sf
#' @keywords internal
getUTMzone <- function(sf){
  
  points <- st_cast(sf, to = "POINT")
  
  ##Find which UTM-zones that have the most points
  utmZonesTr <- st_transform(utmZones,
                             crs = st_crs(points)  )

  utmZone <- suppressMessages(suppressWarnings(st_intersection(points, utmZonesTr)))
  freqZones <- table(utmZone$ZONE[utmZone$ZONE != 0 ]) ## Zone 0 is both north and south so we check for it later
  maxZones <- names(freqZones)[which(freqZones == max(freqZones))]

  alternatives <- rep(FALSE, length(maxZones))
  names(alternatives) <- maxZones

  ## To check on zone 0 points we need to check row A, B, Y and Z
  freqRows <- table(utmZone$ROW_)

  if(is.null(maxZones)){
    ##First we need to check if there is points in any other zone.
    ##If not we accept anything more than zero
    if(any(sum(freqRows[c("A", "B")]) > 0, sum(freqRows[c("Y", "Z")]) > 0)){
      alternatives<-c("0" = FALSE)
    }
  }else{
    ##Else we check if there is more or as many points in zone 0 as in any other zone.
    if(any(sum(freqRows[c("A", "B")]) > freqZones[maxZones],
           sum(freqRows[c("Y", "Z")]) > freqZones[maxZones])){
      alternatives<-c("0" = FALSE)
    }else if(any(sum(freqRows[c("A", "B")]) == freqZones[maxZones],
                 sum(freqRows[c("Y", "Z")]) == freqZones[maxZones])){
      alternatives["0"]<-FALSE
    }
  }

  ##Goes through the zones with most points and checks if other points are to far away.
  for(a in names(alternatives)){
    if(as.integer(a)>0){
      ##If not in polar regions
      if(a != "1" && a != "60"){
        ##If not at the "edge" of the world
        aint <- as.integer(a)
        alternatives[a] <- all(as.integer(names(freqZones)) %in%  c(aint-1, aint, aint+1, 0))
      }else{
        ##We're at the edge of the world, or at least the edge of the UTM world map
        if(a == "1"){
          alternatives[a] <- all(names(freqZones) %in%  c("60", "1", "2", "0")) ##These are the zones we accept
        }else{
          alternatives[a] <- all(names(freqZones) %in%  c("59", "60", "1", "0")) ##These are the zones we accept
        }
      }
    }else{
      ## We are in the polar regions!
      alternatives[a] <- any(
        all(utmZone$ROW_ %in%  c("X", "Y", "Z")), ## We're in the nothern hemisphere!
        all(utmZone$ROW_ %in%  c("A", "B", "C")) ## Now we're down south!
      )
    }
  }

  ##Results
  res <- list("zone" = NULL) #, "msg" = NULL)

  if(sum(alternatives) == 0){
    ##No acceptable zones found
    message("There is no UTM zone where all points overlap with or with its adjacent zones.\n")
    # return(res)
  }else if (sum(alternatives) == 1){
    ## One zone found with points only in adjecent zone found.
    if(all(names(alternatives) == "0")){
      if(all(utmZone$ROW_ %in%  c("X", "Y", "Z"))){
        res$zone <- "0N"
      }else{
        res$zone <- "0S"
      }
    }else{
      res$zone <- as.integer(names(alternatives)[alternatives])
    }
  }else{
    ## Two adjacent zones with same number of points were found.
    ## If one of them is zone 0 the other must be in row X or Z, hence we choose zone 0
    if("0" %in% names(alternatives)){
      if(all(utmZone$ROW_ %in%  c("X", "Y", "Z"))){
        res$zone <- "0N"
      }else{
        res$zone <- "0S"
      }
      message("The points are split over two UTM-zones but close to the polar regions. Therefore zone 0 is chosen.\n")
    }

    coord <- do.call(rbind, st_geometry(points)) #coordinates(spdf)
    ctr <- geosphere::geomean(coord)
    meanPoint<-st_as_sf(data.frame("X"=ctr[1], "Y"=ctr[2]), coords=c("X", "Y"))
    st_crs(meanPoint) <- st_crs(points)
    utmMeanZone <- suppressMessages(st_intersection(meanPoint, utmZones))
    res$zone <- as.integer(utmMeanZone$ZONE)
    message("The points are split over two UTM-zones. The zone with the centroid for all the points was chosen.\n")
  }
  return(res$zone)
}

# Set of functions to create a grid cells from a custom polygon and convert it to API proof strings

#' A wrapper around getUTMzone and produce a crs object string
#'
#' @param x an object of class \sQuote{OrganizedBirds}, \sQuote{sf} or \sQuote{SpatialPointsDataFrame}
#' @return a EPSG integer code for an appropriate UTM zone
#' @export
#' @examples
#' OB <- organizeBirds(bombusObs)
#' getUTMproj(OB)
getUTMproj <- function(x){
  if (!any(class(x) %in% c("OrganizedBirds", "sf", "sfc", "SpatialPointsDataFrame")))
    stop("input data is neither an object of class 'OrganizedBirds', 'sf', 'sfc' or 'SpatialPointsDataFrame'")

  if (any(class(x) == "OrganizedBirds")) {
    spdf <- x$spdf
    if(any(class(spdf) == "SpatialPointsDataFrame")){
      spdf <- st_as_sf(spdf)
    } else {
      spdf <- spdf
    }
  } else if(any(class(x) == "SpatialPointsDataFrame")){
    spdf <- x
    spdf <- st_as_sf(spdf)
  } else {
    spdf <- x
  }

  if (is.na(st_crs(spdf)))
    stop("The polygon has no coordinate projection system (CRS) associated")

  utmZone <- suppressMessages(getUTMzone(spdf))

  if(!is.null(utmZone)){
    if(utmZone == "0N"){
      #"+init=epsg:5041"
      epsg <- 5041 #"+proj=stere +lat_0=90 +lat_ts=90 +lon_0=0 +k=0.994 +x_0=2000000 +y_0=2000000 +datum=WGS84 +units=m +no_defs" #"+init=epsg:5041"
    }
    if(utmZone == "0S"){
      #"+init=epsg:5042"
      epsg <- 5042 #"+proj=stere +lat_0=-90 +lat_ts=-90 +lon_0=0 +k=0.994 +x_0=2000000 +y_0=2000000 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
    }
    if(is.integer(utmZone)){
      proj4 <- paste0("+proj=utm +zone=", utmZone)
      epsg <- as.numeric(rgdal::showEPSG(proj4))
    }
  } else { epsg <- NULL}
  return(epsg)
}
