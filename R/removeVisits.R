#' Remove "bad" observations belonging to the shortest visits
#'
#' This function removes observations based on the visists effort or quality.
#' Effort or quality could be given most often by species list length
#' (that is the number of species observed during the visit, SLL). However,
#' in some cases there is only one or few species observed but in great numbers
#' and spred across a big survey area. Then the effort may not be small.
#' If user may find it necesarry to remove those observations belonging to visits
#' with an effort lower than a threshold, or a certain percentage of the "worst"
#' observations, then this function will help.
#' Please note that this function removes all observations belonging to visits
#' that fullfil the criteria, and that percentage of "lower quality" visits in
#' the sample is not the same as the the percentage of "lower quality" observations.
#'
#' The REMOVAL IS BY QUANTILE and complete visits, therefore if you keep 90 you may get only 50% of all obs
#'
#' @param x an object of class \sQuote{OrganizedBirds} (organised BIRDS Spatial Dataframe).
#' See \code{\link{organizeBirds}}
#' @param ev an object of class \sQuote{data.frame} from exploreVistis.
#' @param criteria the criteria to rank "good visits". Accepts c("SLL", "nObs", "effortDiam", "medianDist")
#' @param percent the percentage (i.e. 0 - 100) of observation to keep, or NULL.
#' (default=0.75)
#' @param minCrit the minimum accepted of a given criteria in the data set (default=NULL).
#'
#' @note If both 'percent' and 'minCrit' are defined then 'percent' prevails.
#'
#' @return An update OrganisedBirds dataset
#' @export

removeObs <- function(x, ev, criteria = "SLL", percent=75,  minCrit = NULL){
  #check x class = Organised birds
  if (class(x) == "OrganizedBirds") {
    obs <- x$spdf@data
    visitCol <- attr(x, "visitCol")
  } else {
    stop("The object 'x' must be of class OrganizedBirds. See the function 'organizedBirds()'.")
  }

  #check x class = Organised birds
  if (class(ev) != "data.frame") {
    stop("The object 'ev' must be of class data.frame resulting from exploreVisits(). See the function 'exploreVisits()'.")
  }
  if(!(criteria %in% c("SLL", "nObs", "effortDiam", "medianDist"))) stop("The argument 'criteria' needs to be one of c('SLL', 'nObs', 'effortDiam', 'medianDist')")

  if(!is.null(percent)){ #check if percent use it else minCrit, else error
    percent<-percent/100
    if(0<=percent & percent <=1){#check percent between 0-1
      wEV <- match(obs[,visitCol], ev$visitUID)
      obs$crit <- ev[wEV,criteria]

      q <- floor(unname(quantile(obs$crit, probs = 1 - percent))) ## the lowest integer fulfilling this quantile
      goodVisits <- unique(obs[which( obs$crit > q),"visitUID"])

      wKeep <- which(obs[,visitCol] %in% goodVisits)
      percLeft <- length(wKeep) / nrow(obs)
      nextStep <- q - 1

      while(percLeft < percent){ # while 1
        if(nextStep <= 0) break(paste0("Nothing else to remove. The result is ", round(percLeft*100, 2), "% of the original observations set"))
        nextVisits <- unique(obs[which( obs$crit >= nextStep &  obs$crit < nextStep + 1), "visitUID"])
        wKeepNext <- which(obs[,visitCol] %in% nextVisits)
        nToAdd <- (percent - percLeft) * nrow(obs) ## if those to add are less

        if(length(wKeepNext)<=nToAdd){
          wKeep <- c(wKeep, wKeepNext)
        }else{
          nextVisitSample <- sample(nextVisits)
          i=0
          while(percLeft < percent){ # while 2
            i=i+1
            if(i > length(nextVisitSample)) break("nothing left to add, lets look at the next class")
            wKeepSample <- which(obs[,visitCol] %in% nextVisitSample[i])
            nObs2keep<-length(wKeep) + length(wKeepSample)
            percLeft.tmp <- nObs2keep / nrow(obs)
            if(percLeft.tmp <= percent){
              wKeep <- c(wKeep, wKeepSample)
              percLeft <- length(wKeep) / nrow(obs)
            } else {
              next("Chunks too big, next...")
            }
          } ## end while 2
        }
        # print(sum(duplicated(wKeep)))
        percLeft <- length(wKeep) / nrow(obs)
        nextStep <- nextStep-1
      } # end of while 1
    }else{ stop("The argument 'percent' must be a number between 0 and 1")}
  # if percent is NULL
  }else if(!is.null(minCrit)){
    # check minCrit integer >1
    if(minCrit > 0){
      goodVisits <- ev[which(ev[,criteria] >= minCrit),"visitUID"]
      wKeep <- which(obs[,visitCol] %in% goodVisits)
      percLeft <- length(wKeep) / nrow(obs)
    }else{stop("The argument 'minCrit' must be > 0")}
  }else{stop("Either 'percent' or 'minCrit' must be supplied")}

  x$spdf <- x$spdf[wKeep,]
  cat(paste0("The result is ", round(percLeft*100, 2), "% of the original observations set"))
  return(x)
}
