# BIRDS <img src="https://github.com/Greensway/BIRDS/raw/master/man/figures/logo.png" align="right" alt="" width="120" />

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# BIRDS 0.2.1

### Bug Fixes
* fixed github issue #27 that introduced errors in function summarizeBirds().  
* fixed github issue #33 that returned an error when making grid for negative coordinates (thanks to Ricardo Correira).  

### Additions
* The function exploreVisits() can now be run in parallel.  
* We translated all spatial functions and data from package sp to sf. Functions still accept sp objects for backwards compatibility.  


# BIRDS 0.1.27

### Breaking changes
* sadly we had to remove the function makeDggrid() because of persistent dependency issues. You can check the commented function from the repository.  

# BIRDS 0.1

### Breaking changes
* changed parameters the argument 'spillover' on summaryBirds() and overlayBirds()
* changed parameters the argument 'shape' on OB2Polygon()
* as of 2020-03-03 dggridR is no longer available in CRAN. To use the function makeDggrid() you need to get the packages from here https://github.com/r-barnes/dggridR/.  

### Additions
* removeObs() a function to remove observations from an OrganisedBirds dataset based on the visits effort (measured with exploreVisits())
* obsIndex() a function retrieving the Relative Reporting Frequency Index (obsIndex). This retrieves both spatial and temporal observation patterns for a focal species compared to a group of species.
* [BREAKING CHANGE] the argument 'spillOver' in summariseBirds() and overlayBirds() now accepts NULL, 'unique' and 'duplicate'. Before, TRUE would duplicate the observations belonging to a visit overlaying two or more gridcells. That is now 'duplicate'. The new optio is 'unique' where observations are assigned a single gridcell (the one with most observations). FALSE is now replaced by NULL.
* makeDggrid() a function making  Discrete Global Grids using function in the dggridR package: 
https://cran.r-project.org/package=dggridR **UPDATE v0.1** as of 2020-03-03 dggridR is no longer available in CRAN. You need to get the packages from here https://github.com/r-barnes/dggridR/. 
* makeCircle() now uses the Skyums algorithm implemented in the shotGroup package to make the minimum encompassing circle.
* getUTMproj() a function to find the UTM zone that best represents the observations and returns its corresponding proj4 string. This is useful when making buffers and circles that should be geometrically correct (e.g. OB2Polygon(, shape="minCircle") or makeCircle()).


# BIRDS 0.0.3

### Additions
* organizeBirds() now is more tolerant to casing in the column names and has better warning messages.
* organizeBirds() now has an argument that accepts a column names for the variable stating the presence status if there is such (useful for FocalSp* and Observation index)
* createVisits() and organizeBirds() can now make visits strictly spatially based on a grid and has the option to ignore time as a variable for making unique visits IDs.
* spatialVisits() we added this function to convert the visit statitics into a spatial object
* focalSpeciesReport() now shows all months and makes plots nicer and customizable.
* exportBirds() added number of cells (nCells) as variable to temporal exports
* obsIndexTemporal() we added a function to produce observation indices over time. Spatial is comming soon.
* we added a vignette about plotting XTS objects 

# BIRDS 0.0.2

### Bug Fixes

* fix and simplify structure of exportBirds() function

### Additions
* function exportBirds() - add number of spatial units with observations (grid cells nCells) as variable to temporal exports
* new function spatialVisits() to plot the spatial representaion of the visits effort

# BIRDS 0.0.1

* This is the first release of BIRDS.
