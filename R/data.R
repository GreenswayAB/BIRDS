#' @title Searching polygon covering Götaland, Sweden.
#'
#' @description An arbitrary polygon covering a portion of Sweden, used as search parameter.
#' @format A SpatialPolygonsDataFrame with 1 polygon with CRS(\dQuote{+init=epsg:4326})
"searchPolygon"

#' @title Götaland, Sweden.
#'
#' @description A polygon describing the territorial contour the collective province of Götaland, Sweden. Includes
#' holes for water bodies.
#' @format A SpatialPolygonsDataFrame with 1 polygon with CRS(\dQuote{+init=epsg:4326})
"gotaland"

#' @title Species observations of bryophytes in Götaland, Sweden
#'
#' @description A random sample of 10,000 observations of bryophytes from an original dataset
#' of 178,765 observations. The observations were accessed via \url{https://www.gbif.org/}.
#' Citation for the original dataset: GBIF.org (03 April 2019) GBIF Occurrence
#' Download \url{https://doi.org/10.15468/dl.ijr8gw}. \cr{}
#' Searching parameters\cr{}
#' And: (All must apply)
#' \describe{
#'   \item{Country or area}{Sweden}
#'   \item{Publisher}{
#'      \itemize{
#'         \item{b8323864-602a-4a7d-9127-bb903054e97d}
#'         \item{28eb1a3f-1c15-4a95-931a-4af90ecb574d}
#'         \item{6ba9a8cc-513a-4a51-bf93-6f5de8040a96}
#'         \item{92f51af1-e917-49bc-a8ed-014ed3a77bec}
#'         \item{f314b0b0-e3dc-11d9-8d81-b8a03c50a862}
#'         }
#'      }
#'   \item{Year}{Between start of 2000 and end of 2018}
#'   \item{Geometry}{POLYGON((10.55786 59.23218,10.55786 58.90465,11.15112 57.75108,11.8103 57.24339,11.94214 56.58369,12.68921 55.31664,14.35913 55.20395,15.08423 55.70236,16.73218 55.82597,17.45728 57.36209,18.11646 58.47072,18.84155 59.29955,10.55786 59.23218))}
#'   \item{Has coordinate}{true}
#'   \item{Scientific name}{Bryophyta}
#'   \item{Has geospatial issue}{false}
#' }
#'
#' @format A data frame with 10,000 rows and 45 variables following DarwinCore standard \url{https://dwc.tdwg.org/}.
#' @source \url{https://www.gbif.org/occurrence/download/0007732-190320150433242}
"bryophytaObs"


#' @title Species observations for the genus \emph{Bombus} spp. in Götaland, Sweden.
#'
#' @description A random sample of 10,000 observations for the genus \emph{Bombus} spp.
#' from an original dataset of 25,848 observations. The observations were accessed via \url{https://www.gbif.org/}.
#' Citation for the original dataset: GBIF.org (03 April 2019) GBIF Occurrence
#' Download \url{https://doi.org/10.15468/dl.jhthmb} \cr{}
#' Searching parameters\cr{}
#' And: (All must apply)
#' \describe{
#'   \item{Country or area}{Sweden}
#'   \item{Publisher}{
#'      \itemize{
#'         \item{b8323864-602a-4a7d-9127-bb903054e97d}
#'         \item{28eb1a3f-1c15-4a95-931a-4af90ecb574d}
#'         \item{6ba9a8cc-513a-4a51-bf93-6f5de8040a96}
#'         \item{92f51af1-e917-49bc-a8ed-014ed3a77bec}
#'         \item{f314b0b0-e3dc-11d9-8d81-b8a03c50a862}
#'         }
#'      }
#'   \item{Year}{Between start of 2000 and end of 2018}
#'   \item{Geometry}{POLYGON((10.55786 59.23218,10.55786 58.90465,11.15112 57.75108,11.8103 57.24339,11.94214 56.58369,12.68921 55.31664,14.35913 55.20395,15.08423 55.70236,16.73218 55.82597,17.45728 57.36209,18.11646 58.47072,18.84155 59.29955,10.55786 59.23218))}
#'   \item{Has coordinate}{true}
#'   \item{Scientific name}{Bombus Latreille, 1802}
#'   \item{Has geospatial issue}{false}
#' }
#'
#' @format A data frame with 10,000 rows and 45 variables following DarwinCore standard \url{https://dwc.tdwg.org/}
#' @source \url{https://www.gbif.org/occurrence/download/0007731-190320150433242}
"bombusObs"

#' @title A simple empty summarisedBirds object.
#'
#' @description An empty summarisedBirds object used to dinamically test for validity
#' of export parameter combinations \code{exportBirds()} in the sister package shinyBirds
#' @format An object of class summarisedBirds
'simpleSB'
