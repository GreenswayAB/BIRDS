## Test environment
* local Windows 10, R version 4.1.0
* local Ubuntu 18.04.3 LTS, R version 4.1.0
* remote win-builder.r-project.org (/M6NUwM96x3e4/)
* remote Github Actions Windows, Mac, and Ubuntu 20.04 LTS, R version 4.1.2 and R devel

## R CMF check results
There where no ERRORs, WARNINGs or NOTES on local or remote builds RMD check, 

We did the following changes based on the package since last: 
###Note: 
We translated all spatial functions and data from sp to sf. Functions still accept sp objects for backwards compatibility.
We fixed issue #27 in function summarize

## Downstream dependencies
There are currently no downstream dependencies for this package in CRAN
