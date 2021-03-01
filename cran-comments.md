## Test environment
* local Windows 10,  R version 4.0.2
* local Ubuntu 18.04.3 LTS, R version 4.0.2
* remote win-builder.r-project.org (/M6NUwM96x3e4/)
* remote Github Actions Windows, Mac, and Ubuntu 20.04 LTS, R version 4.0.2 and R devel

## R CMF check results
There where no ERRORs, WARNINGs or NOTES on local or remote builds RMD check, 

We did the following changes based on the package since last: 
###Note: 
We addressed and fixed all notes.

Unknown, possibly mis-spelled, fields in DESCRIPTION:
  ‘Remotes’

Suggests or Enhances not in mainstream repositories:
  dggridR

Found the following (possibly) invalid URLs:
  URL: https://www.tidyverse.org/lifecycle/#stable (moved to https://lifecycle.r-lib.org/articles/stages.html)
    From: README.md
    Status: 200
    Message: OK

Found the following URLs which should use \doi (with the DOI name only):
  File ‘bombusObs.Rd’:
    https://doi.org/10.15468/dl.jhthmb
  File ‘bryophytaObs.Rd’:
    https://doi.org/10.15468/dl.ijr8gw
    
    Package suggested but not available for checking: ‘dggridR’
    
    Running examples in ‘BIRDS-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: getUTMproj
> ### Title: A wrapper around getUTMzone and produce a proj4 string
> ### Aliases: getUTMproj
> 
> ### ** Examples
> 
> OB <- organizeBirds(bombusObs)
> getUTMproj(OB)
Error in spTransform(utmZones, slot(points, "proj4string")) : 
  package rgdal is required for spTransform methods
Calls: getUTMproj -> getUTMzone
Execution halted

## Downstream dependencies
There are currently no downstream dependencies for this package in CRAN
