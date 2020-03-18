context("Export BIRDS")

library(BIRDS)

SB<-summariseBirds(x=organiseBirds(bombusObs[1:1000,]),
                   grid=makeGrid(gotaland, 50, buffer = TRUE, simplify=TRUE))

sampleGridCell<-sample(which(unlist(lapply(SB$overlaid, nrow))!=0),1)

sampleDate<-lubridate::date(SB$temporal[sample(1:nrow(SB$temporal),1),])

visitUID<-"visitUID"
scientificName<-"scientificName"

dimVar<-c("Spatial", "Temporal")
timeResVar<-c("NULL", "Yearly","Monthly", "Daily", "Month")
varVar<-c("nObs", "nVis", "nSpp", "avgSll", "nYears", "nDays")
methVar<-c("sum","median", "mean")

exportShouldWork<-array(TRUE, dim=c(length(dimVar), length(timeResVar), length(varVar), length(methVar)),
                        dimnames = list(dimVar, timeResVar, varVar, methVar))

#nYears for Spatial FALSE
exportShouldWork["Spatial",c("Yearly","Monthly"),"nYears",]<-FALSE

#Daily for Spatial FALSE
exportShouldWork["Spatial","Daily",,]<-FALSE

#NULL for Temporal FALSE
exportShouldWork["Temporal", "NULL",,]<-FALSE

#nYears for Temporal FALSE
exportShouldWork["Temporal", c("NULL", "Yearly", "Monthly", "Daily"), "nYears",]<-FALSE

#nDays FALSE
# exportShouldWork["Temporal", "Daily", "nDays",]<-FALSE

#Only sum for most
exportShouldWork[,c("NULL", "Yearly","Monthly", "Daily"), c("nObs", "nVis", "nSpp", "nYears", "nDays"), c("median", "mean")]<-FALSE

#Only median for SLL
exportShouldWork[,,"avgSll", c("sum")]<-FALSE

exportShouldWork[,c("NULL", "Yearly","Monthly", "Daily"),"avgSll", c("mean")]<-FALSE

#Only sum for Month and nYears
exportShouldWork[,"Month", "nYears", c("median", "mean")]<-FALSE

test_that("Test valid export variables", {

  for(s in 1:length(dimVar)){
    for(t in 1:length(timeResVar)){
      for(v in 1:length(varVar)){
        for(m in 1:length(methVar)){
          if(exportShouldWork[s,t,v,m]){
            if(timeResVar[t]=="NULL"){
              expect_error(exportBirds(SB, !!dimVar[s], NULL, !!varVar[v], !!methVar[m]), NA)
            }else{
              expect_error(exportBirds(SB, !!dimVar[s], !!timeResVar[t], !!varVar[v], !!methVar[m]), NA)
            }
          }else{
            if(timeResVar[t]=="NULL"){
              expect_error(exportBirds(SB, !!dimVar[s], NULL, !!varVar[v], !!methVar[m]))
            }else{
              expect_error(exportBirds(SB, !!dimVar[s], !!timeResVar[t], !!varVar[v], !!methVar[m]))
            }
          }

        }
      }
    }
  }

})




test_that("Test correct export object classes", {
skip_on_cran()
  for(s in 1:length(dimVar)){
    for(t in 1:length(timeResVar)){
      for(v in 1:length(varVar)){
        for(m in 1:length(methVar)){
          if(exportShouldWork[s,t,v,m]){
            if(s==1){
              if(timeResVar[t]=="NULL"){
                expect_is(exportBirds(SB,!!dimVar[s],NULL,!!varVar[v],!!methVar[m]),"SpatialPolygonsDataFrame")
              }else{
                expect_is(exportBirds(SB,!!dimVar[s],!!timeResVar[t],!!varVar[v],!!methVar[m]),"SpatialPolygonsDataFrame")
              }
            }else if (s==1){
              if(timeResVar[t]=="NULL"){
                expect_is(exportBirds(SB,!!dimVar[s],NULL,!!varVar[v],!!methVar[m]),"numeric")
              }else{
                expect_is(exportBirds(SB,!!dimVar[s],!!timeResVar[t],!!varVar[v],!!methVar[m]),"numeric")
              }
            }
          }
        }
      }
    }
  }

})

# test_that("Test correct export results",{
#
#   expect_equal(exportBirds(SB, "Spatial", NULL, "nObs", "sum")$nObs[!!sampleGridCell], SB$spatial$nObs[!!sampleGridCell])
#   expect_equal(exportBirds(SB, "Spatial", NULL, "nVis", "sum")$nVis[!!sampleGridCell], SB$spatial$nVis[!!sampleGridCell])
#   expect_equal(exportBirds(SB, "Spatial", NULL, "nSpp", "sum")$nSpp[!!sampleGridCell], SB$spatial$nSpp[!!sampleGridCell])
#   expect_equal(exportBirds(SB, "Spatial", NULL, "avgSll", "median")$avgSll[!!sampleGridCell], SB$spatial$avgSll[!!sampleGridCell])
#   expect_equal(exportBirds(SB, "Spatial", NULL, "nYears", "sum")$nYears[!!sampleGridCell], SB$spatial$nYears[!!sampleGridCell])
#   expect_equal(exportBirds(SB, "Spatial", NULL, "nDays", "sum")$nDays[!!sampleGridCell],
#                length(unique(apply(SB$overlaid[[!!sampleGridCell]], 1, function(x) paste0(x[2],"-",x[3],"-",x[4])))))
#
#   expect_equal(exportBirds(SB, "Spatial", "Yearly", "nObs", "sum")@data[!!sampleGridCell, as.character(lubridate::year(!!sampleDate))],
#                SB$spatioTemporal[!!sampleGridCell, as.character(lubridate::year(!!sampleDate)),13, "nObs"])
#   expect_equal(exportBirds(SB, "Spatial", "Yearly", "nVis", "sum")@data[!!sampleGridCell, as.character(lubridate::year(!!sampleDate))],
#                SB$spatioTemporal[!!sampleGridCell, as.character(lubridate::year(!!sampleDate)),13, "nVis"])
#   expect_equal(exportBirds(SB, "Spatial", "Yearly", "nSpp", "sum")@data[!!sampleGridCell, as.character(lubridate::year(!!sampleDate))],
#                SB$spatioTemporal[!!sampleGridCell, as.character(lubridate::year(!!sampleDate)),13, "nSpp"])
#   expect_equal(exportBirds(SB, "Spatial", "Yearly", "avgSll", "median")@data[!!sampleGridCell, as.character(lubridate::year(!!sampleDate))],
#                SB$spatioTemporal[!!sampleGridCell, as.character(lubridate::year(!!sampleDate)),13, "avgSll"])
#   expect_equal(exportBirds(SB, "Spatial", "Yearly", "nDays", "sum")@data[!!sampleGridCell, as.character(lubridate::year(!!sampleDate))],
#                length(unique(apply(SB$overlaid[[!!sampleGridCell]][SB$overlaid[[!!sampleGridCell]][,"year"]==lubridate::year(!!sampleDate),], 1, function(x) paste0(x[2],"-",x[3],"-",x[4])))))
#
#   expect_equal(exportBirds(SB, "Spatial", "Monthly", "nObs", "sum")@data[!!sampleGridCell, paste0(as.character(lubridate::year(!!sampleDate)),"-",as.character(lubridate::month(!!sampleDate)))],
#                SB$spatioTemporal[!!sampleGridCell, as.character(lubridate::year(!!sampleDate)),lubridate::month(!!sampleDate), "nObs"])
#   expect_equal(exportBirds(SB, "Spatial", "Monthly", "nVis", "sum")@data[!!sampleGridCell, paste0(as.character(lubridate::year(!!sampleDate)),"-",as.character(lubridate::month(!!sampleDate)))],
#                SB$spatioTemporal[!!sampleGridCell, as.character(lubridate::year(!!sampleDate)),lubridate::month(!!sampleDate), "nVis"])
#   expect_equal(exportBirds(SB, "Spatial", "Monthly", "nSpp", "sum")@data[!!sampleGridCell, paste0(as.character(lubridate::year(!!sampleDate)),"-",as.character(lubridate::month(!!sampleDate)))],
#                SB$spatioTemporal[!!sampleGridCell, as.character(lubridate::year(!!sampleDate)),lubridate::month(!!sampleDate), "nSpp"])
#   expect_equal(exportBirds(SB, "Spatial", "Monthly", "avgSll", "median")@data[!!sampleGridCell, paste0(as.character(lubridate::year(!!sampleDate)),"-",as.character(lubridate::month(!!sampleDate)))],
#                SB$spatioTemporal[!!sampleGridCell, as.character(lubridate::year(!!sampleDate)),lubridate::month(!!sampleDate), "avgSll"])
#   expect_equal(exportBirds(SB, "Spatial", "Monthly", "nDays", "sum")@data[!!sampleGridCell, paste0(as.character(lubridate::year(!!sampleDate)),"-",as.character(lubridate::month(!!sampleDate)))],
#                length(unique(apply(SB$overlaid[[!!sampleGridCell]][SB$overlaid[[!!sampleGridCell]][,"year"]==lubridate::year(!!sampleDate) & SB$overlaid[[!!sampleGridCell]][,"month"]==lubridate::month(!!sampleDate),], 1, function(x) paste0(x[2],"-",x[3],"-",x[4])))))
#   expect_equal(exportBirds(SB, "Spatial", "Month", "nObs", "sum")@data[!!sampleGridCell, lubridate::month(!!sampleDate)],
#                sum(SB$spatioTemporal[!!sampleGridCell,,lubridate::month(!!sampleDate),"nObs"]))
#   expect_equal(exportBirds(SB, "Spatial", "Month", "nObs", "median")@data[!!sampleGridCell, lubridate::month(!!sampleDate)],
#                median(SB$spatioTemporal[!!sampleGridCell,,lubridate::month(!!sampleDate),"nObs"]))
#   expect_equal(exportBirds(SB, "Spatial", "Month", "nObs", "mean")@data[!!sampleGridCell, lubridate::month(!!sampleDate)],
#                mean(SB$spatioTemporal[!!sampleGridCell,,lubridate::month(!!sampleDate),"nObs"]))
#   expect_equal(exportBirds(SB, "Spatial", "Month", "nVis", "sum")@data[!!sampleGridCell, lubridate::month(!!sampleDate)],
#                sum(SB$spatioTemporal[!!sampleGridCell,,lubridate::month(!!sampleDate),"nVis"]))
#   expect_equal(exportBirds(SB, "Spatial", "Month", "nVis", "median")@data[!!sampleGridCell, lubridate::month(!!sampleDate)],
#                median(SB$spatioTemporal[!!sampleGridCell,,lubridate::month(!!sampleDate),"nVis"]))
#   expect_equal(exportBirds(SB, "Spatial", "Month", "nVis", "mean")@data[!!sampleGridCell, lubridate::month(!!sampleDate)],
#                mean(SB$spatioTemporal[!!sampleGridCell,,lubridate::month(!!sampleDate),"nVis"]))
#   expect_equal(exportBirds(SB, "Spatial", "Month", "nSpp", "sum")@data[!!sampleGridCell, lubridate::month(!!sampleDate)],
#                sum(SB$spatioTemporal[!!sampleGridCell,,lubridate::month(!!sampleDate),"nSpp"]))
#   expect_equal(exportBirds(SB, "Spatial", "Month", "nSpp", "median")@data[!!sampleGridCell, lubridate::month(!!sampleDate)],
#                median(SB$spatioTemporal[!!sampleGridCell,,lubridate::month(!!sampleDate),"nSpp"]))
#   expect_equal(exportBirds(SB, "Spatial", "Month", "nSpp", "mean")@data[!!sampleGridCell, lubridate::month(!!sampleDate)],
#                mean(SB$spatioTemporal[!!sampleGridCell,,lubridate::month(!!sampleDate),"nSpp"]))
#   expect_equal(exportBirds(SB, "Spatial", "Month", "avgSll", "median")@data[!!sampleGridCell, lubridate::month(!!sampleDate)],
#                median(SB$spatioTemporal[!!sampleGridCell,,lubridate::month(!!sampleDate),"avgSll"]))
#   expect_equal(exportBirds(SB, "Spatial", "Month", "avgSll", "mean")@data[!!sampleGridCell, lubridate::month(!!sampleDate)],
#                mean(SB$spatioTemporal[!!sampleGridCell,,lubridate::month(!!sampleDate),"avgSll"]))
#   #bug
#   # expect_equal(exportBirds(SB, "Spatial", "Month", "nYears", "sum")@data[!!sampleGridCell, lubridate::month(!!sampleDate)],
#   #              length(unique(SB$overlaid[[!!sampleGridCell]][,"year"])))
#   expect_equal(exportBirds(SB, "Spatial", "Month", "nYears", "sum")@data[!!sampleGridCell, lubridate::month(sampleDate)],
#                length(unique(SB$overlaid[[!!sampleGridCell]][SB$overlaid[[sampleGridCell]]$month==lubridate::month(sampleDate),"year"])))
#
#   test<-SB$overlaid[[!!sampleGridCell]]
#   test$ymd<-apply(test, 1, function(x) paste0(as.numeric(x[2]), "-", as.numeric(x[3]), "-", as.numeric(x[4])))
#   test<-dplyr::group_by(test, year, month)
#   test<-dplyr::summarise(test, res=n_distinct(ymd))
#   test<-dplyr::summarise(dplyr::group_by(test, month), sum=sum(res), mean=mean(res), median=median(res))
#
#   # expect_equal(exportBirds(SB, "Spatial", "Month", "nDays", "sum")@data[!!sampleGridCell, lubridate::month(!!sampleDate)],
#   #              test[test[,"month"]==lubridate::month(!!sampleDate),]$sum)
#   # #bug in the test
#   # expect_equal(exportBirds(SB, "Spatial", "Month", "nDays", "sum")@data[!!sampleGridCell, lubridate::month(sampleDate)],
#   #              test[test[,"month"]==lubridate::month(sampleDate),]$sum)
#   # #bug in the test
#   # expect_equal(exportBirds(SB, "Spatial", "Month", "nDays", "median")@data[!!sampleGridCell, lubridate::month(!!sampleDate)],
#   #              test[test[,"month"]==lubridate::month(!!sampleDate),]$median)
#   # #bug in the test
#   # expect_equal(exportBirds(SB, "Spatial", "Month", "nDays", "mean")@data[!!sampleGridCell, lubridate::month(!!sampleDate)],
#   #              test[test[,"month"]==lubridate::month(!!sampleDate),]$mean)
#
#   testO<-deconstructOverlay(SB$overlaid, attr(SB, "visitCol"))
#   testM<-test[which(testO[,"month"]==lubridate::month(sampleDate)),]
#   test<-test[which(testO[,"year"]==lubridate::year(sampleDate)),]
#   avgSll<-median(dplyr::summarise(dplyr::group_by(test, !!visitUID),sll=n_distinct(!!scientificName))$sll)
#
#
#   expect_equivalent(exportBirds(SB, "Temporal", "Yearly", "nObs", "sum")[as.character(lubridate::year(!!sampleDate))],
#                sum(SB$temporal[lubridate::year(SB$temporal)==lubridate::year(!!sampleDate),"nObs"]))
#   expect_equivalent(exportBirds(SB, "Temporal", "Yearly", "nVis", "sum")[as.character(lubridate::year(!!sampleDate))],
#                     sum(SB$temporal[lubridate::year(SB$temporal)==lubridate::year(!!sampleDate),"nVis"]))
#   expect_equivalent(exportBirds(SB, "Temporal", "Yearly", "nSpp", "sum")[as.character(lubridate::year(!!sampleDate))],
#                     sum(SB$temporal[lubridate::year(SB$temporal)==lubridate::year(!!sampleDate),"nSpp"]))
#   expect_equivalent(exportBirds(SB, "Temporal", "Yearly", "avgSll", "median")[as.character(lubridate::year(!!sampleDate))],
#                     avgSll)
#   expect_equivalent(exportBirds(SB, "Temporal", "Yearly", "nDays", "sum")[as.character(lubridate::year(!!sampleDate))],
#                     nrow(SB$temporal[lubridate::year(SB$temporal)==lubridate::year(!!sampleDate),]))
# #  bugs in test
# #   expect_equivalent(exportBirds(SB, "Temporal", "Monthly", "nObs", "sum")[paste0(as.character(lubridate::year(!!sampleDate)),"-",as.character(lubridate::month(!!sampleDate)))],
# #                     nrow(test[test[,"month"]==lubridate::month(!!sampleDate),]))
# # bugs in test
# #   expect_equivalent(exportBirds(SB, "Temporal", "Monthly", "nVis", "sum")[paste0(as.character(lubridate::year(!!sampleDate)),"-",as.character(lubridate::month(!!sampleDate)))],
# #                     length(unique(test[test[,"month"]==lubridate::month(!!sampleDate),!!visitUID])))
# #   expect_equivalent(exportBirds(SB, "Temporal", "Monthly", "nSpp", "sum")[paste0(as.character(lubridate::year(!!sampleDate)),"-",as.character(lubridate::month(!!sampleDate)))],
# #                     length(unique(test[test[,"month"]==lubridate::month(!!sampleDate),!!scientificName])))
# #   expect_equivalent(exportBirds(SB, "Temporal", "Monthly", "avgSll", "median")[paste0(as.character(lubridate::year(!!sampleDate)),"-",as.character(lubridate::month(!!sampleDate)))],
# #                     median(dplyr::summarise(dplyr::group_by(test[test[,"month"]==lubridate::month(!!sampleDate),],!!visitUID), sll=n_distinct(!!scientificName))$sll))
# #   expect_equivalent(exportBirds(SB, "Temporal", "Monthly", "nDays", "sum")[paste0(as.character(lubridate::year(!!sampleDate)),"-",as.character(lubridate::month(!!sampleDate)))],
# #                     length(unique(test[test[,"month"]==lubridate::month(!!sampleDate),"day"])))
#
#   ## bug in test
#   # test<-test[test[,"month"]==lubridate::month(sampleDate) & test[,"day"]==lubridate::day(sampleDate),]
#   #
#   # expect_equivalent(exportBirds(SB, "Temporal", "Daily", "nObs", "sum")[as.character(!!sampleDate)],
#   #                   SB$temporal[!!sampleDate, "nObs"])
#   # expect_equivalent(exportBirds(SB, "Temporal", "Daily", "nVis", "sum")[as.character(!!sampleDate)],
#   #                   SB$temporal[!!sampleDate, "nVis"])
#   # expect_equivalent(exportBirds(SB, "Temporal", "Daily", "nSpp", "sum")[as.character(!!sampleDate)],
#   #                   SB$temporal[!!sampleDate, "nSpp"])
#   # expect_equivalent(exportBirds(SB, "Temporal", "Daily", "avgSll", "median")[as.character(!!sampleDate)],
#   #                   median(dplyr::summarise(dplyr::group_by(test, visitUID), sll=n_distinct(scientificName))$sll))
#
#   # testM<-dplyr::group_by(testM, year)
#   #
#   # nyears<-max(testO[,"year"])-min(testO[,"year"])+1
#   #
#   # yearlynObs<-dplyr::summarise(testM, res=n())
#   # nObs<-c(yearlynObs$res, rep(0, nyears-length(yearlynObs$res)))
#   #
#   # yearlynVis<-dplyr::summarise(testM, res=n_distinct(visitUID))
#   # nVis<-c(yearlynObs$res, rep(0, nyears-length(yearlynVis$res)))
#   #
#   # yearlynSpp<-dplyr::summarise(testM, res=n_distinct(scientificName))
#   # nSpp<-c(yearlynObs$res, rep(0, nyears-length(yearlynSpp$res)))
#   #
#   # sll<-dplyr::summarise(dplyr::group_by(testO, year, month, visitUID), sll=n_distinct(scientificName))
#   #
#   # avgSll<-dplyr::summarise(dplyr::group_by(sll, year, month), avgSll=median(sll))
#   #
#   # avgSll<-avgSll[avgSll[,"month"]==lubridate::month(!!sampleDate),]$avgSll
#   #
#   # avgSll<-c(avgSll, rep(0, nyears-length(avgSll$res)))
#   #
#   # yearlynDay<-dplyr::summarise(testM, res=n_distinct(year, month, day))
#   #
#   # nDay<-c(yearlynDay$res, rep(0, nyears-length(yearlynDay$res)))
#   #
#   # expect_equivalent(exportBirds(SB, "Temporal", "Month", "nObs", "sum")[lubridate::month(!!sampleDate)],
#   #                   nrow(testM[,]))
#   # expect_equivalent(exportBirds(SB, "Temporal", "Month", "nObs", "median")[lubridate::month(!!sampleDate)],
#   #                   median(nObs))
#   # expect_equivalent(exportBirds(SB, "Temporal", "Month", "nObs", "mean"),
#   #                   mean(nObs))
#   # expect_equivalent(exportBirds(SB, "Temporal", "Month", "nVis", "sum"),
#   #                   sum(nVis))
#   # expect_equivalent(exportBirds(SB, "Temporal", "Month", "nVis", "median"),
#   #                   median(nVis))
#   # expect_equivalent(exportBirds(SB, "Temporal", "Month", "nVis", "mean"),
#   #                   mean(nVis))
#   # expect_equivalent(exportBirds(SB, "Temporal", "Month", "nSpp", "sum"),
#   #                   sum(nSpp))
#   # expect_equivalent(exportBirds(SB, "Temporal", "Month", "nSpp", "median"),
#   #                   median(nSpp))
#   # expect_equivalent(exportBirds(SB, "Temporal", "Month", "nSpp", "mean"),
#   #                   mean(nSpp))
#   # expect_equivalent(exportBirds(SB, "Temporal", "Month", "avgSll", "median"),
#   #                   median(avgSll))
#   # expect_equivalent(exportBirds(SB, "Temporal", "Month", "avgSll", "mean"),
#   #                   mean(avgSll))
#   # expect_equivalent(exportBirds(SB, "Temporal", "Month", "nYears", "sum")[as.character(lubridate::year(!!sampleDate))],
#   #                   length(unique(lubridate::year(SB$temporal[lubridate::month(sb$temporal)==lubridate::month(!!sampleDate),]))))
#   # expect_equivalent(exportBirds(SB, "Temporal", "Month", "nDays", "sum")[as.character(lubridate::year(!!sampleDate))],
#   #                   length(unique(lubridate::date(SB$temporal[lubridate::month(sb$temporal)==lubridate::month(!!sampleDate),]))))
#   # expect_equivalent(exportBirds(SB, "Temporal", "Month", "nDays", "median")[as.character(lubridate::year(!!sampleDate))],
#   #                   median(nDay))
#   # expect_equivalent(exportBirds(SB, "Temporal", "Month", "nDays", "mean")[as.character(lubridate::year(!!sampleDate))],
#   #                   mean(nDay))
#
# })



