#' Create the polygon for the study area by drawing into a world map
#'
#' @param lat initial geographical coordinate for latitude in decimal degrees (EPSG:4326)
#' for the map to start at. Default = \dQuote{0}.
#' @param lng initial geographical coordinate for longitude in decimal degrees (EPSG:4326)
#' for the map to start at. Default = \dQuote{0}.
#' @param zoom initial zoom level for the map. Range 1 - 19. Default = \dQuote{1}.
#' @param editor type of editor for the drawing tools. Options are \dQuote{leafpm} (default)
#' and \dQuote{leaflet.extras}. Requires additional packages \code{leafpm} and
#' \code{leaflet.extras}, respectively.
#' @return an object of class \sQuote{sf} with a polygon (only the first one drawn)
#' with geodesic coordinates in WGS84 (ESPG:4326).
#' @export
#' @examples
#' if(interactive()){
#'  polygon <- drawPolygon()
#' }
#' @importFrom magrittr %>%
#' @importFrom sf st_transform st_crs st_geometry_type
#' @importFrom mapedit editMap
drawPolygon <- function(lat = 0,
                        lng = 0,
                        zoom = 1,
                        editor = "leafpm") {
    # Maybe add possibility to specify center coordinates and zoom
    map <- leaflet::leaflet() %>%
      leaflet::addTiles(options = leaflet::tileOptions(minZoom = 1, continuousWorld = FALSE)) %>%
      leaflet::setView(lng = lng, lat = lat, zoom = zoom) %>%
      leaflet::setMaxBounds(lng1 = -220, lat1 = 86, lng2 = 220, lat2 = -86)  #%>%

    areaDrawn <- editMap(map,
                         editor = "leafpm",
                         viewer = shiny::paneViewer(),
                         title = "Draw a polygon where to place the grid")
    area <- areaDrawn$finished$geometry

    # Observe the draw input
    areaTypes <- st_geometry_type(area)
    # check if there is a polygon
    if ("POLYGON" %in% areaTypes) {
        wPoly <- which(areaTypes == "POLYGON")
        if (length(wPoly) > 1)
            warning("Only the first polygon will go through")
        polygon <- st_as_sf(area[wPoly[1]])

        # polygon <- spTransform(polygon, CRSobj = CRS(crswkt))
        polygon <- st_transform(polygon,
                                crs = st_crs(4326))
    } else {
        stop("There are no polygones in your drawing")
    }
    return(polygon)
}  ## end draw polygon


#' Create the minimum circle containing the points
#'
#' This function is based on the function shotGroups::getMinCircle() that uses
#' the Skyum algorithm based on the convex hull. http://www.cs.au.dk/~gerth/slides/sven14.pdf
#'
#' @param spdf an object of class \sQuote{sf} or \sQuote{SpatialPointsDataFrame}
#' with defined CRS.
#' @param crs a number defining a projected CRS. It is very important
#' that the selected CRS is accurate in the study area. This is not the CRS for
#' the argument 'spdf' which should be defined internally. This is the CRS used to
#' make a flat circle. Some UTM variant is recommended. See \code{\link{getUTMproj}}
#' @return a polygon object of class \sQuote{sf} with geodesic
#' coordinates in WGS84 (ESPG:4326).
#' @seealso \code{\link{getUTMproj}}
#' @importFrom shotGroups getMinCircle
#' @export
makeCircle<-function(spdf, crs=NULL){
    # error not a SpatialPolygon
    if(any(class(spdf) %in% c("SpatialPoints", "SpatialPointsDataFrame", "sf", "sfc")))
      stop("The input object is neither of class 'sf', SpatialPoints' nor 'SpatialPointsDataFrame'")

    if(any(class(spdf) %in% c("SpatialPoints", "SpatialPointsDataFrame") )){
      spdf <- st_as_sf(spdf)
    }

    ## error no CRS in spdf
    if (is.na(st_crs(spdf)) ) stop("The polygon has no coordinate projection system (CRS) associated")

    ## error crs not defined
    if (is.null(crs)) stop("crs needs to be defined")
    crs <- as.numeric(crs)

    spdf <- st_transform(spdf,
                         crs = st_crs(crs))


    if (!is.na(	st_is_longlat(spdf)) && st_is_longlat(spdf) )
      warning("Spatial object is not projected; this function expects planar coordinates")

    coord <- do.call(rbind, st_geometry(spdf)) #coordinates(spdf)
    coordPaste <- apply(coord, 1, paste0, collapse = ",")
    coordUnique <- matrix(coord[!duplicated(coordPaste)], ncol = 2)

    if (nrow(coordUnique) > 1) {
      # the minumum circle that covers all points
      # lwgeom::st_minimum_bounding_circle
      mincirc <- getMinCircle(coordUnique)
      mincircSP<-st_as_sf(data.frame("X"=mincirc$ctr[1], "Y"=mincirc$ctr[2]),
                          coords=c("X", "Y"))
      st_crs(mincircSP) <- st_crs(crs)

      circle <- st_buffer(mincircSP,
                          dist = mincirc$rad,
                          quadsegs = 10)
      circle <- st_transform(circle,
                             crs = st_crs(4326))

    } else {
        stop("More than one unique set of coordinates are needed to make a minimum circle polygon.")
    }

    row.names(circle) <- as.character(1:length(row.names(circle)))

    return(circle)
}


#' Create the polygon for the study area from a data set of class \sQuote{OrganizedBirds}
#'
#' @param x an object of class \sQuote{OrganizedBirds}, \sQuote{sf} or \sQuote{SpatialPointsDataFrame}
#' @param shape which type of polygon should be made from the data:
#' \itemize{
#'   \item a bounding box (\dQuote{bBox} or \dQuote{bounding box}; i.e. the smallest bounding rectangle
#'   that contains all points),
#'   \item a convex hull (\dQuote{cHull} or \dQuote{convex hull}; i.e. the smallest
#'   convex set that contains all the points).
#'   \item the minimum circle (\dQuote{minCircle} or \dQuote{min circle}; i.e. the smallest
#'   circle that covers all the points).
#' }
#' @return an object of class \sQuote{sf} with a polygon with geodesic coordinates
#'  in WGS84 (ESPG:4326).
#' @examples
#' \donttest{
#'   ob <- organizeBirds(bombusObs)
#'   polygon <- OB2Polygon(ob, shape = "cHull")
#' }
#' @export
OB2Polygon <- function(x, shape="bBox") {

  if (is.null(shape)) shape <- "bBox"

  if (!any(class(x) %in% c("OrganizedBirds", "sf", "SpatialPointsDataFrame")))
           stop("input data is neither an object of class 'OrganizedBirds', 'sf' or 'SpatialPointsDataFrame'")

  if (class(x) == "OrganizedBirds") {
      spdf <- x$spdf
      if(any(class(spdf) == "SpatialPointsDataFrame")){
        spdf <- st_as_sf(spdf)
      }
  } else if(any(class(x) == "SpatialPointsDataFrame")){
          spdf <- x
          spdf <- st_as_sf(spdf)
  } else if(any(class(x) == "sf")){
    spdf <- x
  }

  if (is.na(st_crs(spdf)))
    stop("The polygon has no coordinate projection system (CRS) associated")

  crs <- st_crs(getUTMproj(spdf))

  spdf <- st_transform(spdf,
                       crs = crs)

  coord <- do.call(rbind, st_geometry(spdf))
  coordPaste <- apply(coord, 1, paste0, collapse = ",")
  coordUnique <- matrix(coord[!duplicated(coordPaste)], ncol = 2)

  if (shape %in% c("bBox", "bounding box")){
      polygon <- st_as_sfc(st_bbox(spdf))
  }
  if (shape %in% c("cHull", "convex hull")) {
      if (nrow(coordUnique) > 2) {
          polygon <- spdf %>%
            st_union() %>%
            # dplyr::group_by() %>%
            # dplyr::summarise() %>%
            st_convex_hull()
      } else {
          stop("More than two unique set of coordinates is needed to make a
               convex hull polygon.")
      }
  }

  if (shape %in% c("minCircle", "min circle")) {
    polygon <- makeCircle(spdf, crs = crs)
  } # end shape conditions

  polygon <- st_transform(polygon, crs = st_crs(4326))
  return(polygon)
}

#' Rename the cells in a grid
#'
#' Takes a sf* and renames it to "ID1":"IDn".
#' @param grid an object of class \sQuote{sf}.
#' @param idcol column name with names or ids
#' @return the same input object with known names
#' @keywords internal
renameGrid <- function(grid, idcol="id"){
  nrows <- nrow(grid)
  grid[, idcol] <- paste0("ID", seq(nrows))
  return(grid)
}


#' Make a grid
#'
#' Makes a grid adapted to the purpose of this package and simplifying options
#' from the  \code{sf} package. The central concept of the BIRDS package is the
#' definition of the field visit, and most likely, your grid size will define the
#' maximum area a person can explore during a day. Use the function
#' \code{exploreVisits()} to assess if your definition of visit aligns with your
#'  grid size.
#' @param poly an object of class \sQuote{sf},  \sQuote{SpatialPolygon} or
#' \sQuote{SpatialPolygonDataFrame}
#' @param gridSize width of the cells in Km. It defines the central assumption
#' of this package that is the maximum area a person can explore during a day.
#' Be aware, that the spatial extent of a visit is dependent on the taxonomic
#' group, and many other variables. Maximum recommended for this package 10 km
#' if there is no reliable definition for the spatial extent for visits.
#' @param buffer shall the grid cells include the polygon border? Then \code{TRUE}
#' (default = \code{FALSE}).
#' @param hexGrid shall the grid cells be hexagonal? Then \code{TRUE} (default).
#' Else squared grid cells.
#' @param offset numeric of length 2 with lower left corner coordinates (x, y)
#' of the grid. If it is left empty (\code{NULL}, default), then takes default
#' values \code{st_bbox(x)[c("xmin", "ymin")]}.
#' @param simplify simplifies the polygon geometry. Complicated polygons (those
#' with much detail) make this function run slower.
#' @param tol numerical tolerance value for the simplification algorithm. Set to
#' 0.01 as default.
#' @return an object of class \sQuote{sf} with a set of polygons conforming to a
#' grid of equal-area cells, with geodesic coordinates in WGS84 (ESPG:4326).
#' @note Depending on the total number of grid cells the computations may take
#' time. If there are more than 500 cells on any dimension a warning message will
#' be displayed. Grid cells must be smaller than the sampling area. If the grid
#' cell size is wider than the polygon on any dimension an error message will be
#' displayed.
#' @examples
#' \donttest{
#' grid <- makeGrid(gotaland, gridSize = 10)
#' }
#' @seealso \code{\link{drawPolygon}}, \code{\link{renameGrid}}, \code{\link{OB2Polygon}}, \code{\link{exploreVisits}}
#' @import sf
#' @export
makeGrid <- function(poly,
                     gridSize,
                     hexGrid = TRUE,
                     offset = NULL,
                     buffer = FALSE,
                     simplify = FALSE,
                     tol = 0.01) {

    gridSizeM <- gridSize * 1000 # in meters

    if (!any(class(poly) %in% c("sfc","sf","SpatialPolygons", "SpatialPolygonsDataFrame"))) {
        stop("Entered polygon is not an sf, SpatialPolygon nor SpatialPolygonsDataFrame")
    }

    if (any(class(poly) %in% c("SpatialPolygons", "SpatialPolygonsDataFrame"))) {
      poly <- st_as_sf(poly)
    }
    ## error no CRS
    if (is.na(st_crs(poly))) {
        stop("The polygon has no coordinate projection system (CRS) associated")
    }

    poly <- st_transform(poly,
                         crs = st_crs(getUTMproj(poly)))

    ## If many polygons instead of a multipolygon
    
    if(length(st_geometry(poly)) > 1) poly <- st_union(poly)

    if(is.null(offset)){
      offset <- st_bbox(poly)[c("xmin", "ymin")]
    } else {
      if(length(offset) != 2|| !all(is.integer(offset)) ||!is.numeric(offset))
        stop("Offset should be either NULL or numeric of length 2; lower left corner coordinates (x, y) of the grid")
    }

    # observe the grid cell and study area polygon get the difference in
    # longitude/latitude to make the condition
    corners <- st_as_sfc(st_bbox(poly)) %>% st_cast("POINT")
    distCor <- st_distance(corners[c(1,2,3)])
    dif <- as.numeric(c(distCor[1,2], distCor[3,2]))
    # dif <- as.numeric(abs(diff(matrix(st_bbox(poly), ncol=2))))

    if (any(gridSizeM >= dif)) {
      stop("Grid cells must be smaller than the sampling area")
    }
    if (any(gridSizeM <= dif/500)) {
      message("Grid cells are too many (>=500), this may result in very long computation times")
    }

    if(simplify){
      poly <- st_simplify(poly, dTolerance = tol)
    }

    if (buffer) {
      poly <- st_buffer(poly, dist = gridSizeM)
    }

    grid <- st_make_grid(poly,
                         cellsize = gridSizeM,
                         square = !hexGrid,
                         offset = offset,
                         what = "polygons")

    cells <- st_intersects(poly, grid)[[1]]
    grid <- grid[cells]

    grid <- st_transform(grid, crs = st_crs(4326))
    return(grid)
}


# #' Make a discrete global grid
# #'
# #' Construct a discrete global grid system (dggs) object over a preferred polygon.
# #'
# #' This function depends on a package that is no longer on CRAN. You can
# #' find it in its GitHub repository \url{https://github.com/r-barnes/dggridR}.
# #' Also, this may generate odd results for very large rectangles, because putting
# #' rectangles on spheres is weird... as you should know, if you're using this package.
# #'  Use the function \code{exploreVisits()} to assess if your definition of visit
# #'  aligns with your grid size.
# #' @param polygon an object of class \sQuote{SpatialPolygon} or
# #' \sQuote{SpatialPolygonDataFrame}
# #' @param gridSize width of the cells in Km. It defines the central assumption
# #' of this package that is the maximum area a person can explore during a day.
# #' Be aware, that the spatial extent of a visit is dependent on the taxonomic group, and many other variables.
# #' Maximum recomended for this package 10 km if there is no reliable definition
# #' for the spatial extent for visits.
# #' @param buffer shall the grid cells include the polygon border? Then \code{TRUE}
# #' (default = \code{FALSE}).
# #' @param topology Shape of cell. shall the grid cells be hexagonal, diamonds or
# #' triangular? Options are: \dQuote{hexagon}, \dQuote{diamond}, \dQuote{tirangle}.
# #' Default: \dQuote{hexagon}.
# #' @param aperture How finely subsequent resolution levels divide the grid. Options are: 3, 4.
# #' Only applicable for \code{topology = "hexagon"}. Default for \code{topology = "hexagon"} is 3,
# #' else \code{aperture = 4}.
# #' @param simplify simplifies the polygon geometry using the Douglas-Peuker algorithm  (from rgeos package).
# #' Complicated polygons (those with much detail) make this function run slower.
# #' @param tol numerical tolerance value for the simplification algorith. Set to 0.01 as default.
# #' @return an object of class \sQuote{SpatialPolygon} with a set of polygons
# #' conforming to a grid of equal-area cells, with geodesic coordinates in WGS84 (ESPG:4326).
# #' @note Depending on the total number of grid cells the computations may take time.
# #' If there are more than 100 cells on any dimension a warning message will be displayed.
# #' Grid cells must be smaller than the sampling area. If the grid cell size is wider than the polygon on any dimension
# #' an error message will be displayed.
# #' @seealso \code{\link{drawPolygon}}, \code{\link{renameGrid}}, \code{\link{OB2Polygon}}, \code{\link{exploreVisits}}
# #' @importFrom dplyr group_map
# #' @importFrom rlang .data
# #' @importFrom utils installed.packages
# #' @export
# makeDggrid <- function(poly,
#                      gridSize,
#                      buffer = FALSE,
#                      topology = "hexagon",
#                      aperture = 3,
#                      simplify=FALSE,
#                      tol=0.01) {
#
#   if(!"dggridR" %in% names(installed.packages()[,1])) stop("This function requires the package 'dggridR' that is not currently installed.
# intalled. As this package may not currently be on CRAN, please consider installing it with 'remotes::install_github('r-barnes/dggridR')'")
#
#   #Construct a global grid with cells approximately 1000 m across
#   topology <- toupper(topology)
#
#   aperture <- if(topology == "HEXAGON"){
#     if(aperture == 3 || aperture == 4){
#       aperture
#     }else{
#       stop("aperture can only be 3 or 4.")
#     }
#   }else{
#     4
#   }
#
#   dggs <- dggridR::dgconstruct(spacing=gridSize, metric=TRUE, precision=10,
#                       resround='nearest', topology = topology, aperture = aperture)
#
#   # error not a SpatialPolygon
#   if (!(class(poly) %in% c("SpatialPolygons", "SpatialPolygonsDataFrame"))) {
#     stop("Entered polygon is not a SpatialPolygon nor SpatialPolygonsDataFrame")
#   }
#
#   ## error no CRS
#   if (is.na(slot(poly,"proj4string"))) {
#     stop("The polygon has no coordinate projection system (CRS) associated")
#   }
#
#   ## simplify if takes too long to make the grid
#   if (simplify) {
#     ##TODO use tryCatch()
#     poly <- rgeos::gSimplify(poly, tol = tol)
#   }
#
#   # Transform to WGS84 pseudo-Mercator
#   if (buffer) {
#     # Needs to be projected
#     polygonProj <- suppressWarnings(spTransform(poly,
#                                                 CRSobj = CRS("+init=epsg:3857")) )
#     polygonBuffer <- rgeos::gBuffer(polygonProj, width = gridSize*1000)
#   } else {polygonBuffer <- polygon}
#
#   polygonGeod <- suppressWarnings(spTransform(polygonBuffer,
#                                               CRSobj = CRS("+init=epsg:4326")))
#
#   extent <- polygonGeod@bbox
#   #Get the grid cell boundaries for cells on the polygon extent
#   grid <- dggridR::dgrectgrid(dggs, extent[2,1], extent[1,1], extent[2,2], extent[1,2])
#
#   gridPolList <- grid %>%
#     group_by(.data$cell) %>%
#     group_map(.f=function(.x,...){
#       Polygons(
#         list(Polygon(cbind(.x$long, .x$lat))),
#         ID=strsplit(as.character(unique(.x$group)),"[.]")[[1]][1])
#     })
#   gridPol <- SpatialPolygons(gridPolList,
#                              proj4string = CRS("+init=epsg:4326") )
#   gridPolInt <- vector()
#
#   for(i in 1:length(gridPol)){
#     gridPolInt[i] <-rgeos::gIntersects(polygonGeod, gridPol[i,])
#   }
#   gridPol <- gridPol[gridPolInt, ]
#
#   return(gridPol)
# }


#' Convert a grid into a web query string.
#'
#' Converts a grid (or any SpatialPolygon for that matter) into a web query string.
#' @param grid an object of class \sQuote{SpatialPolygon-class} or
#' \sQuote{SpatialPolygonDataFrame-class}.
#' @return a character string with coordinates separated by \dQuote{\%20} and pairs by \dQuote{,}.
#' @export
gridAsString <- function(grid) {
    # error not a SpatialPolygon
    if (!any(class(grid) %in% c("sf","SpatialPolygons", "SpatialPolygonsDataFrame"))) {
        stop("Entered grid is not a sf, SpatialPolygon nor SpatialPolygonsDataFrame")
    }
    if (any(class(grid) %in% c("SpatialPolygons", "SpatialPolygonsDataFrame"))) {
      grid <- st_as_sf(grid)
    }
    grid <- st_transform(grid, crs = st_crs(4326))

    ncells <- length(grid)
    polyStrg <- list()
    for (i in 1:ncells){

      polyStrg[[i]] <- gsub(" ","%20",
                            gsub(", ", ",",
                                 gsub( "))", "",
                                       gsub("POLYGON ((", "",
                                            st_as_text(grid[i]),
                                            fixed=TRUE),
                                       fixed=TRUE),
                                 fixed=TRUE),
                            fixed=TRUE)

    }


    return(polyStrg)
}
