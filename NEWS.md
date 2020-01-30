# BIRDS <img src="https://github.com/Greensway/BIRDS/raw/master/man/figures/logo.png" align="right" alt="" width="120" />

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# BIRDS 0.1

## Breaking changes
* in this version have we thouth...

## Additions
*removeObs() a function to remove observations from OrganisedBirds dataset based on the visits effort (measured with exploreVisits())
*Relative Reporting Frequency Index (obsIndex) add a function for focal species time trends: overlaid-->get the species-->convert back to Organized (attention to spillover)-->yearly summary-->compare to the whole group Jag har döpat om funcitonen to obsIndexTemporal för att: det handlar om ett Observation Index, och jag har diskuterad med Debora att det kan också finnas ett spatial function (ingen time resolution, bara ett enda karta), och en funktion som samlar båda (som för export).  Också: vi kan sätta som ett parameter nObs eller nVis
*SpillOver option not repeating visits but moving them to the grid cells that have most observations . Function deconstructOverlay (in export.r) if there is spill over and duplicates have been created they have to be removed from this result
*integrate the makeGrid function with dggridR package: https://cran.r-project.org/web/packages/dggridR/vignettes/dggridR.html
*makeGrid.r changed the makeCircle function to use the Skyums algorithm as implemented in the shotGroup package
*getUTMzones and getUTMproj: we added functions to find a projected UTM zone and its corresponding proj4 string for your data. This is useful when making buffers and circles that should be geometrically correct.

## Bug Fixes  

# BIRDS 0.0.3

## Additions
* organizeBirds() now is more tolerant to casing in the column names and has better warning messages.
* organizeBirds() now has an argument that accepts a column names for the variable stating the presence status if there is such (useful for FocalSp* and Observation index)
* createVisits() and organizeBirds() can now make visits strictly spatially based on a grid and has the option to ignore time as a variable for making unique visits IDs.
* spatialVisits() we added this function to convert the visit statitics into a spatial object
* focalSpeciesReport() now shows all months and makes plots nicer and customizable.
* exportBirds() added number of cells (nCells) as variable to temporal exports
* obsIndexTemporal() we added a function to produce observation indices over time. Spatial is comming soon.
* we added a vignette about plotting XTS objects 

# BIRDS 0.0.2

## Bug Fixes

* fix and simplify structure of exportBirds() function

## Additions
* function exportBirds() - add number of spatial units with observations (grid cells nCells) as variable to temporal exports
* new function spatialVisits() to plot the spatial representaion of the visits effort

# BIRDS 0.0.1

* This is the first release of BIRDS.
