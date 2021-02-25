.onLoad <- function(libname, pkgname) {
  if(!("dggridR" %in% rownames(installed.packages()))){
    packageStartupMessage("The package 'dggrid' was not found among your installed packages. This package is required if you plan to use the function 'makeDggrid()'.
            As it handles about a package that may not be currently on CRAN, please consider installing it with 'remotes::install_github('r-barnes/dggridR')'")
  }
}
