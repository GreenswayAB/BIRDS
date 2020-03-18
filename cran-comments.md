## Test environment
* local Windows 10,  R version 3.6.1 (2019-07-05)
* local Ubuntu 18.04.3 LTS, R version 3.6.1 (2019-07-05)
* remote win-builder.r-project.org (/wfjJw9QWLsmf/)

## R CMF check results
There where no ERRORs, WARNINGs or NOTEs on local RMD check

Only during r-devel build I get a Note regarding my email, but it is the one I use.
  CRAN incoming feasibility ... NOTE
  Maintainer: ‘Alejandro Ruete <aleruete@gmail.com>’

We did the following changes based on the package reviewers recommendations: 
* We use '' only for package names but not for function names

* we changed the beginning of the description so that it does not start with "This package".

* package dggridR was moved to Suggested in Description as it is no longer available on CRAN, and its source is now linked in the description 

* we now are careful to reset user option on the examples using 
  oldpar <- par(no.readonly = TRUE)
  ...
  par(oldpar)

and in functions that so require via 
  oldpar <- par(no.readonly = TRUE)       # code line i
  on.exit(par(oldpar))                    # code line i + 1

* we now avoid \dontrun{} (replaced with \donttest{}) as it is not adequate on our case, and  removed both \dontrun{} and \donttest{} when we use instead if(interactive())

## Downstream dependencies
There are currently no downstream dependencies for this package
