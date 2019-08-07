# BIRDS <img src="https://github.com/Greensway/BIRDS/raw/master/man/figures/logo.png" align="right" alt="" width="120" />

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)


### A set of tools for Biodiversity Informatics in R 
This is the Biodiversity Information Review and Decision Support package for R!  
**NB**: BIRDS is an acronym. This packages is not limited to birds' data (i.e. Aves) :smiley:  

This repository hosts an R package that is being developed for systematizing biodiversity
data review in order to evaluate whether a set of species observation data is 
fit-for-use and help take decisions upon its use of for further analysis.  

The `BIRDS` package provides a set of tools to review biodiversity data in order 
to understand data quality in terms of completeness, and the data generation 
process (i.e. the observers’ sampling behavior). The `BIRDS` package provides 
a systematic approach to evaluate biodiversity data – to enhance reproducibility
and facilitate the review of data. The `BIRDS` package intends to provide the data
user with knowledge about sampling effort (amount of effort expended during an event) 
and data completeness (data gaps) to help judge whether the data is representative, 
valid and fit for the purpose of its intended use – and hence support for making 
decisions upon the use and further analysis of biodiversity data.  

The `BIRDS` package is most useful for heterogeneous data sets with variation in 
the sampling process, i.e. where data have been collected and reported in variable
ways, not conforming to the same sampling protocol and therefore varying in 
sampling effort, leading to variation in data completeness (i.e. how well the 
reported observations describe the “true” state). Primary biodiversity data (PBD) 
combining data from different data sets, like e.g. GBIF mediated data, commonly 
vary in the ways data has been generated - containing opportunistically collected 
presence-only data (no sampling protocol, no or inconsistent information about 
absences, high sampling variability between observers), and data sets that have been 
collected using different sampling protocols. The set of tools provided by the `BIRDS` 
package is aimed at illuminating and understanding the process that generated the 
data (i.e. observing, recording and reporting species into databases). It does this 
by a systematic approach, and providing summaries that inform about sampling effort
and data completeness (or data gaps). 


The `BIRDS` package is **not** concerned with data accuracy, which can be evaluated 
and improved using other existing packages (as outlined in the 
[technical details](https://greensway.github.io/BIRDS/articles/technical_details.html) 
vignette), before processing the data using `BIRDS`.

The concepts and methods, and examples are described after a short description on 
how to install this package into R.


### How to install `BIRDS`

This package is still not published in CRAN. Therefore the easiest option to 
install it is directly from GitHub using the package `devtools`. 

Install devtools if you do not already have installed it:
```r
install.packages('devtools')
library(devtools)
```
`devtools` may have some other dependencies. Please visits the following webpages 
to be sure you are on the right track:

* [devtools](https://www.r-project.org/nosvn/pandoc/devtools.html)  
* [Rtools](https://cran.r-project.org/bin/windows/Rtools/) 

Now you should be ready to go with the `BIRDS` install:

```r
devtools::install_github('Greensway/BIRDS')
library(BIRDS)
```


### Concepts and methods

#### Systematic approach – a workflow for primary biodiversity data

In order to systematize and enhance reproducibility of the review process for PBD
the `BIRDS` package takes a systematic approach. With this package the date are 
systematically organised and reviewed. This systematic approach actually starts 
before using `BIRDS` as we suggest steps and tools for optionally cleaning the 
data before processing by `BIRDS`. Hence, before using biodiversity data for 
the intended analysis start by optionally cleaning the data, then use `BIRDS` to 
organize, summarize and review the data:

<img src=https://github.com/Greensway/BIRDS/raw/master/man/figures/BIRDs.png />

Then, use your review to evaluate sampling effort and data gaps, and to inform
decisions about whether the data are fit-for-purpose and how to further analyse
the data.


##### Field visit

A central concept used by the `BIRDS` package is the “visit” – defining the sampling
unit as a sampling event by a unique observer (or group of observers), at a unique
unit of space and time (commonly a day). Visits can help us to summarize the amount
of effort expended in the field. During a visit, the observer commonly samples 
(i.e. observes and records) species by similar methods. The sampling effort can 
vary among visits, with the amount of effort expended being greater when spending
more time, and reporting more of the observed species. The same number of observations 
(records of species) at a unique unit of time and space could be made by either 
few observers reporting many species (greater effort by each observer) or many 
observers reporting few species (small effort by each observer). Using visits as
sampling units allows separation of sampling effort into the effort that can be 
expressed through the number of visits by different observers and the effort per
visit (e.g. species list length, or when available the time spent during a visit). 
Hence, the quality (completeness) of the data can be judged by using information 
for each visit and information from a collection of visits. 

You can examine this in the 
[technical details](https://greensway.github.io/BIRDS/articles/technical-details.html)
vignette.

##### Spatial grid and spillover

Defined by a unique observer (or group of observers), at a unique unit of space and time visits can be identified by a unique combination of variables: observer id, location, time. Often location is a named unit of space that has been visited during the same sampling event. For example a botanist visiting and reporting species for a meadow, or a bird watcher visiting and reporting species for a lake. 

Sometimes locations can be more accurate positions for individuals of species that have been observed and reported during the same field visit. The botanist may have visited the meadow but reported species from a number of different sampling points in that meadow. Or the bird watcher reported species for different parts of the lake. In that case there is no common spatial identifier for the visit. 

If there is no common spatial identifier to define the visit extent, and the observer id is not enough to constrain observations spatially (e.g. group of observers from organisation where observer id = organisation name), then visits can be created *when* overlaying the observation data with the spatial grid. A visit is then defined as all the observations falling into the same grid cell. It is important to keep in mind to choose a grid with a cell size that corresponds to (or at least is not smaller than) the average spatial extent known (or assumed) to be typical for field visits for the reference species group (see below). This process can be repeated with a set of grids with different offset to explore the sensitivity of the results to the size of the grid cells. 

You can examine this in the [technical details](https://greensway.github.io/BIRDS/articles/technical-details.html) vignette.


##### Reference species group

Because visits result from the sampling process they can only be defined for a reference species group, i.e. a group of species observed and recorded by similar methods. The rationale for a reference species group is based on the assumption that species groups share similar bias: we assume that, despite varying field skills and accuracy, observers reporting observations for species of a reference species group share similar observer behavior and methods and, hence, generate data with similar sampling bias (Phillips et al. 2009). From this we can assume that the larger the number of visits (or observations) reporting species from the reference group at a specific unit of space and time, the more likely it is that the lack of visits for (or observations of) a particular species reflects the absence of (or failure to detect) a focal species rather than a lack of visits and reports made.

It is important to keep in mind that, to keep the sampling bias consistent, the reference species group should only include species that are assumed to be sampled with the same methodology (Ponder et al. 2001). For example, a reference group should not include all species in the Order Lepidoptera because butterflies *sensu stricto* (superfamily Papilionoidea) are sampled in very different ways than most other species of Lepidoptera (mainly moths).


##### Species list length (SLL)

The SLL per visit (i.e. the number of species observed and recorded per visit) is a well known proxy for the time spent in the field and willingness to report all species seen of a reference taxonomic group, Szabo et al. 2010). The `BIRDS` package therefore uses SLL as a proxy for sampling effort.


### What does the package do?

With the `BIRDS`’ package set of tools PBD can be reviewed based on the information contained in the visits. Use `BIRDS` to organize the data, summarize and review the data as shown above. The `BIRDS` package organizes the data into a spatially gridded visit-based format, from which summaries are retrieved for a number of variables describing the visits across both spatial and temporal dimension. Those variables are the number of visits, number of species, number of observations, average species list length per visit, number of units of space and time with visits. The variables can be used to collectively describe the sampling effort and data completeness (data gaps), and can be examined spatially (e.g. viewed on maps) and temporally (e.g. plotted as time series). 


#### What does the package help us with?

Using the detailed information on sampling effort and data completeness provided by the `BIRDS`’ package summaries allows better inference on what the reported species observations mean. As a much of the PBD is presence-only data the provided information helps us judging to what degree a lack of observations may be (1) due to the species not being observed (absent, or failed to detect) or (2) due to a lack of reports (lack of visits, or lack of reports for observed species) (little sampling effort). We can be more confident about the first when there is good sampling effort and data completeness, while evidence is shaky, i.e. high probability to have missed species, when there is little sampling effort and data completeness.  In this way the user can judge whether the data is fit-for-purpose for the intended use. Using this information about how the data has been collected the user can also decide about how to analyse the data.

It helps you getting :world_map: :bar_chart: :chart_with_upwards_trend: :chart_with_downwards_trend:
:page_facing_up: :bulb: about  

:dog2: :cat2: :pig2: :mouse2: :cat: :sheep: :cow2: :rat: :rabbit: :goat: :baby_chick: 
:rooster: :wolf: :frog: :tiger: :koala: :bear: :boar: :monkey: :horse: :camel:
:elephant: :panda_face: :snake: :bird: :penguin: :turtle: :bug: :honeybee: :ant:
:beetle: :snail: :octopus: :tropical_fish: :blowfish: :fish: :shell: :whale: 
:dolphin: :water_buffalo: :tiger2: :leopard: :ox: :crocodile: :dromedary_camel:

and 
 
:bouquet: :cherry_blossom: :tulip: :four_leaf_clover: :rose: :sunflower: :hibiscus:
:maple_leaf: :leaves: :fallen_leaf: :herb: :mushroom: :cactus: :palm_tree: :evergreen_tree:
:deciduous_tree: :chestnut: :seedling: :blossom: :ear_of_rice:  

but, maybe not :dragon_face: :dragon: :christmas_tree:

##### References:  
Phillips et al. 2009 Sample selection bias and presence‐only distribution models: implications for background and pseudo‐absence data, Ecol Appl 19:181-197.  
Ponder et al. 2001 Evaluation of Museum Collection Data for Use in Biodiversity Assessment, Cons Biol 15:648-657.  
Szabo et al. 2010 Regional avian species declines estimated from volunteer‐collected long‐term data using List Length Analysis, Ecol Appl 20:2157-2169.  


### Overview of main components

You can find an overview of the `BIRDS` main components and functions, organised as an [overview workflow here](https://github.com/Greensway/BIRDS/raw/master/man/figures/BIRDs.png) and a [workflow highlighting the decisions to be taking when using BIRDS here](https://github.com/Greensway/BIRDS/raw/master/man/figures/BIRDsDecision.png). 

### Example

The [Intro to BIRDS](https://greensway.github.io/BIRDS/articles/BIRDS.html) vignette provides a useful
walk through the package tools using an example data set.

A short introductory [video can be found here](https://www.dropbox.com/s/fxg1t9vl4ainipy/BirdsLR.mp4?dl=0).


### In the TODO LIST
Check [here](https://github.com/Greensway/BIRDS/projects/1) for a list of future features to be added, and don't hesitate sending your suggestions by [e-mail](mailto:alejandro@greensway.se) 


