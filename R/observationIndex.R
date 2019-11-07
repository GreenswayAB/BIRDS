#' Relative observation index (Temporal)
#'
#' This function extracts the proportion of observations for a focal species to
#' the all observations (including the focal species) over time.
#' @param x an object of class \sQuote{SummarizeBirds}.
#' @param timeRes the time resolution:  \code{"Yearly", "Monthly"} or \code{"Daily"}
#' @param focalSp the focal sp to look for.
#'
#' @return An xts timeseries
#' @export
#'
#' @examples
obsIndexTemporal<-function(x, timeRes, focalSp=NULL){
  if (class(x) != "SummarizedBirds") {
    stop("The object 'x' must be of class SummarizedBirds.")
  }
  if (is.null(focalSp)) {
    stop("Please, define the focal species to search for.")
  }
  timeRes <- tolower(timeRes)
  yearsAll <- as.numeric(dimnames(x$spatioTemporal)[[2]])

  spData<-deconstructOverlay(x$overlaid, attr(x, "visitCol"))

  spData$dates<-as.Date(switch(timeRes,
                     "yearly" = paste0(spData$year, "-01-01"),
                     "monthly"= paste0(spData$year, "-", sprintf("%02d", spData$month), "-01"),
                     "daily"  = paste0(spData$year, "-", sprintf("%02d", spData$month), "-", sprintf("%02d", spData$day))))

  res<-if(timeRes=="yearly"){
    xts::xts(rep(NA, length(seq(min(spData$dates), max(spData$dates), by="year"))),
             seq(min(spData$dates), max(spData$dates), by="year"))
  }else if (timeRes=="monthly"){
    xts::xts(rep(NA, length(seq(min(spData$dates), max(spData$dates), by="month"))),
             seq(min(spData$dates), max(spData$dates), by="month"))
  }else if(timeRes=="daily"){
    xts::xts(rep(NA, length(seq(min(spData$dates), max(spData$dates), by="day"))),
             seq(min(spData$dates), max(spData$dates), by="day"))
  }else{
    stop("Unknown time resolution")
  }


  allN<-summarise(group_by(spData,dates),all=n())
  spN<-summarise(group_by(spData[spData$scientificName==focalSp,],dates),sp=n())

  allN<-xts::xts(allN$all, allN$dates)
  spN<-xts::xts(spN$sp, spN$dates)

  res<-merge(res,allN,join='left')
  res<-merge(res,spN,join='left', fill=0)

  res<-res[,-1]

  res$relObs<-res$spN/res$allN

  return(res)

}
