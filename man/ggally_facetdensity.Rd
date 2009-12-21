\name{ggally_facetdensity}
\alias{ggally_facetdensity}
\title{Plots the density plots by faceting...}
\usage{ggally_facetdensity(data, ...)}
\description{Plots the density plots by faceting
Make density plots by displaying subsets of the data in different panels}
\author{Barret Schloerke \email{bigbear@iastate.edu}, Haesung Kim \email{hae0510@iastate.edu}, Di Cook \email{dicook@iastate.edu} and Heike Hofmann \email{hofmann@iastate.edu}}
\keyword{hplot}
\arguments{\item{data}{data set using}
\item{...}{other arguments being sent to stat_density's by aes_string(...)}}
\examples{ggally_facetdensity(iris, x = "Petal.Width", y = "Species")
ggally_facetdensity(iris, x = "Species", y = "Petal.Width", colour = "Species")}
