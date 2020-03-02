## Test environment
* local Windows 10,  R version 3.6.1 (2019-07-05)
* local Ubuntu 18.04.3 LTS, R version 3.6.1 (2019-07-05)
* remote win-builder.r-project.org (/wfjJw9QWLsmf/)

## R CMF check results
There where no ERRORs, WARNINGs or NOTEs

Only during r-devel build I get a Note regarding my email, but it is the one I use.
  CRAN incoming feasibility ... NOTE
  Maintainer: ‘Alejandro Ruete <aleruete@gmail.com>’

We did the following changes based on the package reviewrs recommendations:   
* We Fixed Rd markup for DOIs and canonical URLs for packages.

* We removed the redundant 'A Set of Tools or' from your title and
  description.
  
* We now only use single, undirected quotes to mark packages
  
* We now only use double quotes to mark paper titles
  
* We now explain the all acronyms in the description text. e.g. GBIF
  
* We cite the references when needed following the recommended stantards. Although they cant be added as a list in the Description file.
  
* We now removed "+ file LICENSE" and add the LICENSE file to .rbuildignore as there is no modification to it.
  
* We changed all print()/cat() messages to the appropriate message()/warning()
  
* We either uncommented or removed commented lines in the examples.
  
* We now explain how to instead of intalling packages for the user.
  
* we now reset the par() setting at the end of your examples if otherwise changed
  

## Downstream dependencies
There are currently no downstream dependencies for this package
