#' A function to create Ignorance Scores
#'
#' A function to create Ignorance Scores.
#' Read more here ...
#TODO define analysis unit: grid cell or time
#' @param nObs an object of any class (mainly resulting from summariseBirds() or
#' exportBirds() with  the number of observations, or visits in your desired analysis unit.
#' @param nSpp the number of unique species observed
#' @param h the half ignorance parameter value.
#' @return a \code{data.frame} with ignorance scores
#' @examples
#' OB <- organizeBirds(bryophytaObs, sppCol = "scientificName", simplifySppName = TRUE)
#' grid <- makeGrid(searchPolygon, gridSize = 10)
#' SB <- summariseBirds(OB, grid=grid)
#' ignorance <- exposeIgnorance(nObs=SB$spatial@data$nObs)
#' @export
#' @seealso \code{\link{summarizeBirds}}, \code{\link{exportBirds}}
exposeIgnorance<-function(nObs, nSpp=NULL, h=1){
  ## make them vectors

  if (!is.null(nSpp)){
    ObsInd<-nObs/nSpp
    Ign<-h/(h + ObsInd)
  } else {
    Ign<-h/(h + nObs)
  }

  # IgnCom<-IgnComb(ObsInd, Nobs, input$obs50,input$obs50spp)
}
