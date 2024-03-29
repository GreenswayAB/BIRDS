% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exportBirds.R
\name{exportBirds}
\alias{exportBirds}
\title{Export single variables from SummarisedBirds objects}
\usage{
exportBirds(x, dimension, timeRes, variable, method = "sum")
}
\arguments{
\item{x}{a SummarizedBirds-object}

\item{dimension}{a character string indicating if the export should be
\code{"spatial"} or \code{"temporal"}}

\item{timeRes}{A character string indicating what temporal resolution should
be used. For Spatial export the function accepts \code{NULL, "yearly", "monthly"}
or \code{"month"}. For temporal export the function accepts \code{"yearly",
"monthly", "daily"} or \code{"month"}.}

\item{variable}{a character string indicating which variable should be
exported, \code{"nObs", "nVis", "nSpp", "nDays", "nCells"} or \code{"avgSll" }.
For \code{timeRes = c(NULL, "month")} the function also accepts \code{"nYears"}.}

\item{method}{Only applicable to \code{timeRes = "month"}. A variable specifying which
statistical method should be applied. The function accepts \code{"sum", "median", "mean"}.}
}
\value{
an xts time series (if dimension = "temporal"), a named vector (if 
dimension = "temporal" and timeRes = "month"), or a sf (if dimension = "spatial")
}
\description{
Export single variables from SummarisedBirds objects
}
\note{
the difference between \code{timeRes = "monthly"} and \code{timeRes = "month"}
is that the former returns "n.years x 12" values, while month summarize over
years and returns only 12 values aplying the method among years.
For more details over the possible combinations of dimensions and variables
please refer to the vignette "Technical details".
}
\examples{
\donttest{
grid <- makeGrid(searchPolygon, gridSize = 10)
SB <- summariseBirds(organizeBirds(bombusObsShort), grid=grid)
EB <- exportBirds(SB, "spatial", "month", "nDays", "median")
}
}
