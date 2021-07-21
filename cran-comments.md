## Test environment
* local Windows 10, R version 4.1.0
* remote r-hub check_for_cran()
* remote Github Actions Windows, Mac, and Ubuntu 20.04 LTS, R version 4.1.2 and R devel

## R CMD check results
There where no ERRORs or WARNINGs on local builds after R CMD check
There was one NOTE on remote builds "Note: found 18 marked UTF-8 strings". 
This note corresponds to 'sf' objects provided as data for the examples and vignettes to run. After being in contact with the sf package developers, I found no way to avoid this note as it is not clear where in the object these characters are. I kindly ask for an exception on this note. 

Regarding the archival reason "X-CRAN-Comment: Archived on 2021-07-13 for policy violation." I have tried to contact CRAN without reply.

## Note: 
We did the following changes based on the reviewer comments since last submission: 
Some of the functions with missing Rd-tags were not meant to have .Rd files (we removed the .Rd files).
Else we added the tags as suggested.

We now use find.package() as suggested instead of installed.packages()

The function that can be run in parallel has an argument 'parallel' set to FALSE as default to prevent using more than 1 core in examples, vignettes, etc.

## Downstream dependencies
There are currently no downstream dependencies for this package in CRAN
