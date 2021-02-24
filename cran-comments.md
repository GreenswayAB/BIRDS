## Test environment
* local Windows 10,  R version 4.0.2 (2020-06-22)
* local Ubuntu 18.04.3 LTS, R version 4.0.2 (2020-06-22)
* remote win-builder.r-project.org (/M6NUwM96x3e4/)
* remote Github Actions Ubuntu 18.04.5 LTS, R version 4.0.2 (2020-06-22)

## R CMF check results
There where no ERRORs, WARNINGs on local or remote buidls RMD check, 
There was 1 NOTEs on local buidls RMD check: 

We did the following changes based on the package since last: 
###Note: 
During the coming releases compatibility issues with rgdal and sp may occur. We advice to update those packages and the libraries GDAL and PROJ.  

###Bug fixes:  
* removeObs() got stuck in infinite loops under certain circumstances. #10  
* zoo::index() Sometimes when reloading summarise birds it trows the following error when I run focalSpReport #7  
* includeUniqueSpillover() would crash if the grid polygon had an associated data.frame (spatialPolygonDataFrame). #8  
* exploreVisits() error when timeInVisits != "day" #11  
* CRS issue in organizeBirds(x=data, grid=grid) #9 This issue was related to #17 for which we have made a great deal of changes to prepare for the imminent release of the new versions of rgdal and sp packages.  
* simplifySpp() would return "Falco" for cases like "Falco peregrinus, 1771". We now implemented the package   taxsize to improve this function. #16  

###New functions:
* we now offer more options to prepare community matices with functions communityMatrix(), CommunityMatrixGrid() and RecbySpec() to we used in community analyses and completeness functions from other packages. #24
* the function exploreVisits() gains the output variable "nclusters" and nos uses the function DBSCAN::DBSCAN() to calculate clusters and outliers within each visit. #19 #18
* focalSpp() reports are now more efficient and fast. #25
* sppreports() now gained the argument overlay to add overlay layers in the reports

## Downstream dependencies
There are currently no downstream dependencies for this package
