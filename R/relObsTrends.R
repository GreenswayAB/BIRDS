#' Relative Observation Trends
#'
#' @param x an object of class \sQuote{SummarizeBirds}.
#' @param timeRes the time resolution:  \code{"Yearly", "Monthly"} or \code{"Daily"}
#'
#' @param focalSp the focal sp to look for.
#'
#' @return A timeseri
#' @export
#'
#' @examples
relObsTrends<-function(x, timeRes, focalSp){
  if (class(x) != "SummarizedBirds") {
    stop("The object 'x' must be of class SummarizedBirds.")
  }
  if (is.null(focalSp)) {
    stop("Please, define the focal species to search for.")
  }
  timeRes <- tolower(timeRes)
  yearsAll <- as.numeric(dimnames(x$spatioTemporal)[[2]])

  spData<-deconstructOverlay(x$overlaid, attr(x, "visitCol"))

  spData$group<-if(timeRes=="yearly"){
    spData$year
  }else if (timeRes=="monthly"){
    paste0(spData$year,"-",spData$month)
  }else if(timeRes=="daily"){
    paste0(spData$year,"-",spData$month,"-",spData$day)
  }else{
    stop("Unknown time resolution")
  }

  allN<-summarise(group_by(spData,group),all=n())
  spN<-summarise(group_by(spData[spData$scientificName==focalSp,],group),sp=n())
  res<-merge(allN, spN, by="group", all.x=TRUE)

  groupYMD<-strsplit(res$group,split = "-")
  year<-as.numeric(unlist(lapply(groupYMD, function(x) x[1])))
  month<-as.numeric(unlist(lapply(groupYMD, function(x) x[2])))
  day<-as.numeric(unlist(lapply(groupYMD, function(x) x[3])))

  rownames(res) <- switch(timeRes,
                          "yearly" = paste0(year, "-01-01"),
                          "monthly"= paste0(year, "-", sprintf("%02d", month), "-01"),
                          "daily"  = paste0(year, "-", sprintf("%02d", month), "-", sprintf("%02d", spData$day)))

  res<-res[,-1]
  res$relObs<-res$sp/res$all

  res <- xts::as.xts(res)
  return(res)

}
