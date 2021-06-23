#' Create ignorance scores
#'
#' Ignorance scores are a proxy for the lack of sampling effort, computed by making
#' the number of observations relative to a reference number of observations that
#' is considered to be enough to reduce the ignorance score by half (henceforth
#' the Half-ignorance approach). The algorithm behind the Ignorance Score is
#' designed for comparison of bias and gaps in primary biodiversity data across
#' taxonomy, time and space
#' @references Ruete (2015) <doi:10.3897/BDJ.3.e5361>
#' @param nObs an object of any class (mainly resulting from \code{summariseBirds} or
#' \code{exportBirds} with  the number of observations, or visits in your desired analysis unit.
#' @param nSpp the number of unique species observed
#' @param h the half ignorance parameter value.
#' @return a \code{data.frame} with ignorance scores
#' @examples
#' \donttest{
#' OB <- organizeBirds(bombusObsShort, sppCol = "scientificName", simplifySppName = TRUE)
#' grid <- makeGrid(searchPolygon, gridSize = 10)
#' SB <- summariseBirds(OB, grid=grid)
#' ignorance <- exposeIgnorance(nObs=SB$spatial$nObs)
#' }
#' @export
#' @seealso \code{\link{summarizeBirds}}, \code{\link{exportBirds}}
exposeIgnorance<-function(nObs, nSpp=NULL, h=1){
  if (!is.null(nSpp)){
    ObsInd<-nObs/nSpp
    Ign<-h/(h + ObsInd)
  } else {
    Ign<-h/(h + nObs)
  }
}
