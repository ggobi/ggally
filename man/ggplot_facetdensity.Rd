\name{ggplot_facetdensity}
\alias{ggplot_facetdensity}
\title{Plots the density plots by Faceting...}
\usage{ggplot_facetdensity(data, ...)}
\description{Plots the density plots by Faceting
Make density plots by displaying subsets of the data in different panels}
\author{Barret Schloerke \email{bigbear@iastate.edu}, Haesung Kim \email{hae0510@iastate.edu}, Di Cook \email{dicook@iastate.edu} and Heike Hofmann \email{hofmann@iastate.edu}}
\keyword{hplot}
\arguments{\item{data}{data set using}
\item{...}{}}
\examples{ggplot_facetdensity(iris[,4:5])
ggplot_facetdensity(iris[,5:4], colour = "blue")}
