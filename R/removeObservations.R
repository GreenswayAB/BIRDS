#' Remove observations belonging to the shortest ("worst") visits
#'
#' This function removes observations based on the visists effort or quality.
#' Visit effort or quality could be given most often by species list length
#' (that is, the number of species observed during the visit, SLL). However,
#' in some cases there could be only one or few species observed but in great
#' numbers each and spred across a big surveyed area. The effort then may not be
#' small. If the user may find it necesary to remove those observations belonging
#' to visits with an effort lower than a threshold, or a certain percentage of
#' the "worst" observations, then this function will help.
#'
#' Please note: this function removes all observations belonging to visits
#' that fullfil the criteria. Also, the percentage of "lower quality" visits in
#' the sample is not necessarily the same as the the percentage of "lower quality"
#' observations. The removal of observations is done stepwise by quantile therefore
#' you may get a lower percentage than the aimed given than all remaining visists
#' are too large to be included completely. This may happen particularly with
#' samller datasets.
#'
#' @param x an object of class \sQuote{OrganizedBirds} (organised BIRDS Spatial
#' Dataframe). See \code{\link{organizeBirds}}
#' @param ev an object of class \sQuote{data.frame} from exploreVistis.
#' @param criteria the criteria to rank "good visits". Accepts c("SLL", "nObs",
#' "effortDiam", "medianDist")
#' @param percent the percentage (i.e. 0 - 100) of observation to keep, or NULL.
#' (default = 75)
#' @param minCrit the minimum accepted of a given criteria in the data set
#' (default = NULL).
#' @param stepChunk if the search for observations includes too many in a given
#' quality stage, the search takes progressively smaller fractions of the dataset
#' in steps. This argumente controls for how small fractions are discarded on
#' each step. If stepChunk = 0.05 (default) means that in the first step 95% of
#' the observations will be tested, then 95% x 95% = 90.25%, and so on until an
#' adequate number of observations are obtained. Increase this argument if you
#' see the function takes too long.
#'
#' @note If both 'percent' and 'minCrit' are defined then 'percent' prevails.
#' @return An updated OrganisedBirds dataset
#' @examples
#' \donttest{
#' OB <- organizeBirds(bombusObs, sppCol = "scientificName", simplifySppName = TRUE)
#' EV <- exploreVisits(OB)
#' OBshorter <- removeObs(OB, EV, percent = 75)
#' }
#' @export

removeObs <- function(x,
                      ev,
                      criteria = "SLL",
                      percent=75,
                      minCrit = NULL,
                      stepChunk=0.05){
  #check x class = Organised birds
  if (class(x) == "OrganizedBirds") {
    obs <- x$spdf@data
    visitCol <- attr(x, "visitCol")
  } else {
    stop("The object 'x' must be of class OrganizedBirds. See the function 'organizedBirds()'.")
  }

  evColnamesRef <- c("visitUID", "day", "month", "Month", "year", "date",
                     "centroidX", "centroidY",  "nObs", "SLL", "effortDiam",
                     "medianDist", "iqrDist", "nOutliers")
  #check ev class = data.frame
  if (class(ev) != "data.frame" |
      !all(evColnamesRef %in% colnames(ev)) ) {
    stop("The object 'ev' must be of class data.frame resulting from exploreVisits(). See the function 'exploreVisits()'.")
  }

  if(!(criteria %in% c("SLL", "nObs", "effortDiam", "medianDist"))){
    stop("The argument 'criteria' needs to be one of c('SLL', 'nObs', 'effortDiam', 'medianDist')")
  }

  #check: if percent use it, else minCrit, else error
  if(!is.null(percent)){
    percent <- percent / 100

    #check percent between 0-1
    if(0 <= percent & percent <= 1){
      wEV <- match(obs[, visitCol], ev$visitUID)
      obs$crit <- ev[wEV, criteria]

      ## the lowest integer fulfilling the quantile
      q <- floor(unname(quantile(obs$crit, probs = 1 - percent)))
      # goodVisits <- unique(obs[which( obs$crit > q),"visitUID"])
      goodVisits <- unique(obs[which( obs$crit > q), visitCol])

      wKeep <- which(obs[,visitCol] %in% goodVisits)
      percLeft <- length(wKeep) / nrow(obs)
      nextStep <- q

      #while(round(percLeft, 3) < percent){ # while 1
      #Cannot have a while here. The function should only return the best effort observations.
      #Whith a while it will go throug all efforts and try to add, trying to make a subset as close to the
      #percentage level as polsible.
      #It should only look at the next best visit after good visits and se if it could be added.
      if(round(percLeft, 3) < percent){
        if(nextStep <= 0){
          message(paste0("Nothing else to remove. The result is ",
                         round(percLeft * 100, 2),
                         "% of the original observations set"))
        }else{
          nextVisits <- unique(obs[which( obs$crit >= nextStep &
                                            obs$crit < nextStep + 1),
                                   "visitUID"])
          wKeepNext <- which(obs[,visitCol] %in% nextVisits)
          nToAdd <- (percent - percLeft) * nrow(obs) ## if those to add are less

          if(! nToAdd < 1){
            # No more rows to remove
            if(length(wKeepNext)<=nToAdd){
              wKeep <- c(wKeep, wKeepNext)
            }else{
              # Placing the visits in a random order.
              nextVisitSample <- nextVisits[sample.int(length(nextVisits))]
              wKeepSample <- which(obs[,visitCol] %in% nextVisitSample)

              # pick a big chunk
              chunkPerc <- 1
              while(length(wKeepSample) >= nToAdd){
                chunkPerc <- chunkPerc * (1-stepChunk)
                chunk <- floor(length(nextVisitSample) * chunkPerc)
                if(chunk == 0){
                  # If chunk == 0 nextVisitSample[1:chunk] will be the same as nextVisitSample[1],
                  # which will take us to an infinite loop.
                  chunk <- 1
                  break()
                }
                wKeepSample <- which(obs[,visitCol] %in% nextVisitSample[1:chunk])
              }
              # redefine which visits are left
              if(exists("chunk")){
                nextVisitSample <- nextVisitSample[-(1:chunk)]
              }

              nObs2keep <- length(wKeep) + length(wKeepSample)
              percLeft.tmp <- nObs2keep / nrow(obs)
              if(percLeft.tmp <= percent){
                wKeep <- c(wKeep, wKeepSample)
                percLeft <- length(wKeep) / nrow(obs)
              }

              ### Slowly complete until reaching the target
              i=0
              while(round(percLeft, 3) < percent){ # while 2
                i=i+1
                if(i > length(nextVisitSample)){
                  break() # nothing left to add, lets look at the next class
                }
                wKeepSample <- which(obs[,visitCol] %in% nextVisitSample[i])
                nObs2keep <- length(wKeep) + length(wKeepSample)
                percLeft.tmp <- nObs2keep / nrow(obs)
                if(percLeft.tmp <= percent){
                  wKeep <- c(wKeep, wKeepSample)
                  percLeft <- length(wKeep) / nrow(obs)
                } else {
                  message("Chunks too big, next...\n")
                  next()
                }
              } ## end while 2
            }
            percLeft <- length(wKeep) / nrow(obs)
            nextStep <- nextStep - 1
          }
        }

      } # end of if (not while)
    }else{
      stop("The argument 'percent' must be a number between 0 and 1")
    }
  # if percent is NULL
  }else if(!is.null(minCrit)){
    # check minCrit integer >1
    if(minCrit > 0){
      goodVisits <- ev[which(ev[,criteria] >= minCrit),"visitUID"]
      wKeep <- which(obs[,visitCol] %in% goodVisits)
      percLeft <- length(wKeep) / nrow(obs)
    }else{
      stop("The argument 'minCrit' must be > 0")
    }
  }else{
    stop("Either 'percent' or 'minCrit' must be supplied")
  }

  x$spdf <- x$spdf[wKeep,]
  message(paste0("The result is ",
                 round(percLeft*100, 2),
                 "% of the original observations set"))
  return(x)
}
