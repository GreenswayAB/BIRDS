# Set of functions to create a grid cells from a custom polygon and convert it to API proof strings

#' Create the polygon for the study area by drawing into a world map
#'
#' @param lat initial geographical coordinate for latititude in decimal degrees (EPSG:4326)
#' for the map to start at. Default = \dQuote{0}.
#' @param lng initial geographical coordinate for longitude in decimal degrees (EPSG:4326)
#' for the map to start at. Default = \dQuote{0}.
#' @param zoom initial zoom level for the map. Range 1 - 19. Default = \dQuote{1}.
#' @param editor type of editor for the drawing tools. Options are \dQuote{leafpm} (default)
#' and \dQuote{leaflet.extras}. Requires additional packages \code{leafpm} and
#' \code{leaflet.extras}, respectively.
#' @return an object of class \sQuote{SpatialPolygon-class} with a polygon
#' (only the first one drawn) with geodesic coordinates in WGS84 (ESPG:4326).
#' @export
#' @examples
#' \donttest{
#' # polygon <- drawPolygon()
#' }
#' @importFrom magrittr %>%
drawPolygon <- function(lat = 0,
                        lng = 0,
                        zoom = 1,
                        editor = "leafpm") {
    # Maybe add possibility to specify center coordinates and zoom
    map <- leaflet::leaflet() %>%
      leaflet::addTiles(options = leaflet::tileOptions(minZoom = 1, continuousWorld = FALSE)) %>%
      leaflet::setView(lng = lng, lat = lat, zoom = zoom) %>%
      leaflet::setMaxBounds(lng1 = -220, lat1 = 86, lng2 = 220, lat2 = -86)  #%>%

    areaDrawn <- mapedit::editMap(map,
                                  editor = "leafpm",
                                  viewer = shiny::paneViewer(),
                                  title = "Draw a polygon where to place the grid")
    area <- areaDrawn$finished$geometry

    # Observe the draw input
    areaTypes <- sf::st_geometry_type(area)
    # check if there is a polygon
    if ("POLYGON" %in% areaTypes) {
        wPoly <- which(areaTypes == "POLYGON")
        if (length(wPoly) > 1)
            cat("Only the first polygon will go through")  ## Is this necesary?
        polygon <- as(area[wPoly[1]], "Spatial")
        polygon <- sp::spTransform(polygon, CRSobj = sp::CRS("+init=epsg:4326"))
    } else {
        stop("There are no polygones in your drawing")
    }
    return(polygon)
}  ## end draw polygon


#' Create the minimum circle containing the points
#'
#' @param spdf an object of class \sQuote{SpatialPointsDataFrame} with geodesic
#' coordinates in WGS84 (ESPG:4326).
#' @return an object of class \sQuote{SpatialPolygon} with a polygon with geodesic
#' coordinates in WGS84 (ESPG:4326).
#' @export
makeCircle<-function(spdf){
    # error not a SpatialPolygon
    if(!(class(spdf) %in% c("SpatialPoints", "SpatialPointsDataFrame"))) {
        stop("The input object is neither of class 'SpatialPoints' nor 'SpatialPointsDataFrame'")
    }
    ## error no CRS
    if (is.na(sp::proj4string(spdf))) {
        stop("The polygon has no coordinate projection system (CRS) associated")
    }

    spdf <- sp::spTransform(spdf, CRSobj = sp::CRS("+init=epsg:4326"))

    coord <- sp::coordinates(spdf)
    coordPaste <- apply(coord, 1, paste0, collapse = ",")
    coordUnique <- matrix(coord[!duplicated(coordPaste)], ncol = 2)

    if (nrow(coordUnique) > 1) {
        # the minumum circle that covers all points
        ctr <- rgeos::gCentroid(spdf) # center
        dist <- max(geosphere::distGeo(ctr, coordUnique)) # diagonal to the corner
        ctrProj<-sp::spTransform(ctr, sp::CRS("+init=epsg:3857"))
        circle <- rgeos::gBuffer(spgeom = ctrProj, width = dist*2, quadsegs = 10)
        circle <- sp::spTransform(circle, CRSobj = sp::CRS("+init=epsg:4326"))
    } else {
        stop("More than one unique set of coordinates is needed to make a minimum circle polygon.")
    }

    row.names(circle)<-as.character(1:length(row.names(circle)))

    return(circle)
}


#' Create the polygon for the study area from a dataset of class \sQuote{OrganizedBirds}
#'
#' @param df an object of class \sQuote{OrganizedBirds} or \sQuote{SpatialPointsDataFrame}
#' @param shape which type of polygon should be made from the data:
#' \itemize{
#'   \item a bounding box (\dQuote{bBox} or \dQuote{bounding box}; i.e. the smallest bounding rectangle
#'   that contains all points),
#'   \item a convex hull (\dQuote{cHull} or \dQuote{convex hull}; i.e. the smallest
#'   convex set that contains all the points).
#'   \item the minimum circle (\dQuote{buffCircle} or \dQuote{buffer circle}; i.e. the smallest
#'   circle that covers all the points).
#' }
#' @return an object of class \sQuote{SpatialPolygon} with a polygon with geodesic
#' coordinates in WGS84 (ESPG:4326).
#' @examples
#' orgDf <- organizeBirds(bombusObs)
#' polygon <- OB2Polygon(orgDf, shape = "cHull")
#' @export
OB2Polygon <- function(df, shape="bBox") {
    if (class(df) == "OrganizedBirds") {
        spdf <- df$spdf
    } else {
        if(class(df) == "SpatialPointsDataFrame"){
            spdf <- df
            spdf <- sp::spTransform(spdf, CRSobj = sp::CRS("+init=epsg:4326"))
        } else {
            stop("input data is neither an object of class 'OrganizedBirds' or 'SpatialPointsDataFrame'")
        }
    }

    ## error no CRS
    if (is.na(sp::proj4string(spdf))) {
        stop("The polygon has no coordinate projection system (CRS) associated")
    }

    coord <- sp::coordinates(spdf)
    coordPaste <- apply(coord, 1, paste0, collapse = ",")
    coordUnique <- matrix(coord[!duplicated(coordPaste)], ncol = 2)

    if (is.null(shape)) shape<-"bBox"

    if (shape %in% c("bBox", "bounding box")){
        bboxMat<-sp::bbox(spdf)
        polygonCoord <- matrix(c(bboxMat[1,1], bboxMat[2,1],
                           bboxMat[1,1], bboxMat[2,2],
                           bboxMat[1,2], bboxMat[2,2],
                           bboxMat[1,2], bboxMat[2,1],
                           bboxMat[1,1], bboxMat[2,1]), ncol = 2, nrow = 5, byrow = TRUE)
        polygon <- sp::SpatialPolygons(
            list(
                sp::Polygons(
                    list(
                        sp::Polygon( polygonCoord )
                        ),
                    ID="1")
                ),
            proj4string=sp::CRS("+init=epsg:4326")
            )
    }

    if (shape %in% c("cHull", "convex hull")) {
        if (nrow(coordUnique) > 2) {
            hpts <- grDevices::chull(coord)
            hpts <- c(hpts, hpts[1])
            polygon<-sp::SpatialPolygons(
                list(
                    sp::Polygons(
                        list(
                            sp::Polygon( coord[hpts, ] )
                            ),
                        ID="1")
                    ),
                proj4string=sp::CRS("+init=epsg:4326")
                )
        } else {
            stop("More than two unique set of coordinates is needed to make a 
                 convex hull polygon.")
        }
    }

    if (shape %in% c("buffCircle", "buffer circle")) {
        polygon<-makeCircle(spdf)
    } # end shape conditions

    # polygon <- sp::spTransform(polygon, CRSobj = sp::CRS("+init=epsg:4326"))
    return(polygon)
}

#' Rename the cells in a grid
#'
#' Takes a SpatialPolygon* and renames it to "ID1":"IDn".
#' @return the same input object with known names
renameGrid<-function(grid){
    for(i in 1:length(grid)){
      # grid@polygons[[i]]@ID <- gsub("ID", "", grid@polygons[[i]]@ID)
      grid@polygons[[i]]@ID <- paste0("ID", i)
    }
  return(grid)
}


#' Make a grid
#'
#' Makes a grid adapted to the purpose of this package and simplifing options
#' from the  \code{sp} package. The central concept of the BIRDS package is the
#' definition of the field visit, and most likely, your grid size will define the
#' maximum area a person can explore during a day. Use the function
#' \code{exploreVisits()} to assess if your definition of visit aligns with your
#'  grid size.
#' @param polygon an object of class \sQuote{SpatialPolygon} or
#' \sQuote{SpatialPolygonDataFrame}
#' @param gridSize width of the cells in Km. It defines the central assumption
#' of this package that is the maximum area a person can explore during a day.
#' Be aware, that the spatial extent of a visit is dependent on the taxonomic group, and many other variables.
#' Maximum recomended for this package 10 km if there is no reliable definition
#' for the spatial extent for visits.
#' @param buffer shall the grid cells include the polygon border? Then \code{TRUE}
#' (default = \code{FALSE}).
#' @param hexGrid shall the grid cells be hexagonal? Then \code{TRUE} (default).
#' Else squared grid cells.
#' @param offset the offset (position) of the grid (from \code{spsample} methods).
#' If it is left empty (\code{NULL}, default), then takes default values.
#' For squared grid cells the default is set to \code{c(0.5,0.5)} ("centric systematic").
#' For hexagonal grid cells the default is set to \code{c(0,0)}.
#' @param simplify simplifies the polygon geometry using the Douglas-Peuker algorithm  (from rgeos package).
#' Complicated polygons (those with much detail) make this function run slower.
#' @param tol numerical tolerance value for the simplification algorith. Set to 0.01 as default.
#' @return an object of class \sQuote{SpatialPolygon} with a set of polygons
#' conforming to a grid of equal-area cells, with geodesic coordinates in WGS84 (ESPG:4326).
#' @note Depending on the total number of grid cells the computations may take time.
#' If there are more than 100 cells on any dimension a warning message will be displayed.
#' Grid cells must be smaller than the sampling area. If the grid cell size is wider than the polygon on any dimension
#' an error message will be displayed.
#' @examples
#' grid <- makeGrid(gotaland, gridSize = 10)
#' @seealso \code{\link{drawPolygon}}, \code{\link{renameGrid}}, \code{\link{OB2Polygon}}, \code{\link{exploreVisits}}
#' @export
makeGrid <- function(polygon, 
                     gridSize, 
                     buffer = FALSE, 
                     hexGrid = TRUE, 
                     offset=NULL, 
                     simplify=FALSE, 
                     tol=0.01) {
  
    gridSizeM <- gridSize * 1000 # in meters
    gridSizeDg <- gridSize/111  # because on average 1 degree is 111 km

    # error not a SpatialPolygon
    if (!(class(polygon) %in% c("SpatialPolygons", "SpatialPolygonsDataFrame"))) {
        stop("Entered polygon is not a SpatialPolygon nor SpatialPolygonsDataFrame")
    }

    ## error no CRS
    if (is.na(sp::proj4string(polygon))) {
        stop("The polygon has no coordinate projection system (CRS) associated")
    }

    ## simplify if takes too long to make the grid
    if (simplify) {
      ##TODO apply tryCatch()
      polygon <- rgeos::gSimplify(polygon, tol = tol)
    }

    # Transform to WGS84 pseudo-Mercator
    polygonProj <- sp::spTransform(polygon, CRSobj = sp::CRS("+init=epsg:3857"))
    if (buffer) {
        # Needs to be projected
        polygonProjBuffer <- rgeos::gBuffer(polygonProj, width = gridSizeM)
    } else {polygonProjBuffer <- polygonProj}

    polygonGeod <- sp::spTransform(polygonProjBuffer, CRSobj = sp::CRS("+init=epsg:4326"))

    # observe the grid cell and study area polygon get the difference in
    # longitude/latitude to make the condition
    dif <- diff(t(polygonProj@bbox))

    if (any(gridSizeM >= dif)) {
        stop("Grid cells must be smaller than the sampling area")
    }
    if (any(gridSizeM <= dif/100)) {
        warning("Grid cells are too small, this may result in very long computation times")
    }

    # Create the actual grid
    if (hexGrid == TRUE) {
        if (is.null(offset)){
            offset <- c(0, 0)
        }
        points <- sp::spsample(polygonGeod, type = "hexagonal",
                               offset = offset,
                               cellsize = gridSizeDg)
        grid <- sp::HexPoints2SpatialPolygons(points)
    } else {
        if (is.null(offset)){
            offset <- c(0.5, 0.5)
        }
        points <- sp::spsample(polygonGeod, type = "regular",
                               offset = offset,
                               cellsize = gridSizeDg)
        grid <- sp::as.SpatialPolygons.GridTopology(sp::points2grid(points),
                                                    proj4string = sp::CRS("+init=epsg:4326"))
        wcells <- sp::over(points, grid)
        grid <- grid[wcells, ]
    }
    return(grid)
}



#' Convert a grid into a web query string.
#'
#' Converts a grid (or any SpatialPolygon for that matter) into a web query string.
#' @param grid an object of class \sQuote{SpatialPolygon-class} or
#' \sQuote{SpatialPolygonDataFrame-class}.
#' @return a character string with coordinates separated by \dQuote{\%20} and pairs by \dQuote{,}.
#' @export
gridAsString <- function(grid) {
    # error not a SpatialPolygon
    if (!(class(grid) %in% c("SpatialPolygons", "SpatialPolygonsDataFrame"))) {
        stop("Entered grid is not a SpatialPolygon nor SpatialPolygonsDataFrame")
    }

    ncells <- length(grid)
    polyStrg <- list()
    for (i in 1:ncells) polyStrg[[i]] <- paste0(apply(grid@polygons[i][[1]]@Polygons[[1]]@coords, 1, paste0, collapse = "%20"), collapse = ",")

    return(polyStrg)
}

