#' Relative Observation Trends
#'
#' @param x an object of class \sQuote{SummarizeBirds}.
#' @param scale \code{"Yearly", "Monthly"} or \code{"Daily"}
#'
#' @param focalSp the focal sp to look for.
#'
#' @return A timeseri
#' @export
#'
#' @examples
relObsTrends<-function(x, scale, focalSp){

  spData<-deconstructOverlay(x$overlaid, attr(x, "visitCol"))

  spData$group<-if(scale=="yearly"){
    spData$year
  }else if (scale=="monthly"){
    paste0(spData$year,"-",spData$month)
  }else if(scale=="daily"){
    paste0(spData$year,"-",spData$month,"-",spData$day)
  }else{
    stop("Unknown scale")
  }

  allN<-dplyr::summarise(dplyr::group_by(spData,group),all=dplyr::n())
  spN<-dplyr::summarise(dplyr::group_by(spData[spData$scientificName==focalSp,],group),sp=dplyr::n())
  res<-merge(allN, spN, by="group", all.x=TRUE)

  res$relObs<-res$sp/res$all

  return(res)

}
