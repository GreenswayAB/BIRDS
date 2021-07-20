## Test environment
* local Windows 10, R version 4.1.0
* remote r-hub check_for_cran()
* remote Github Actions Windows, Mac, and Ubuntu 20.04 LTS, R version 4.1.2 and R devel

## R CMD check results
There where no ERRORs or WARNINGs on local builds after R CMD check
There was one NOTE on remote builds "Note: found 18 marked UTF-8 strings". 
This note corresponds to 'sf' objects provided as data for the examples and vignettes to run. After being in contact with the sf package developers, I found no way to avoid this note as it is not clear where in the object these characters are. I kindly ask for an exception on this note. 

Regarding the archival reason "X-CRAN-Comment: Archived on 2021-07-13 for policy violation." I have tried to contact CRAN without reply.

We did the following changes based on the package since last: 
## Note: 
We translated all spatial functions and data from package sp to sf. Functions still accept sp objects for backwards compatibility.
We fixed github issue #27 that introduced errors in function summarizeBirds()
We fixed github issue #33 that returned an error when making grid for negative coordinates
The function exploreVisits() can now be run in parallel


## Downstream dependencies
There are currently no downstream dependencies for this package in CRAN
