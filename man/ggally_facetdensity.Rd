\name{ggally_facetdensity}
\alias{ggally_facetdensity}
\title{Plots the density plots by faceting...}
\usage{ggally_facetdensity(data, mapping, ...)}
\description{Plots the density plots by faceting
Make density plots by displaying subsets of the data in different panels}
\author{Barret Schloerke \email{schloerke@gmail.com}}
\keyword{hplot}
\arguments{\item{data}{data set using}
\item{mapping}{aesthetics being used}
\item{...}{other arguments being sent to stat_density}}
\examples{ggally_facetdensity(iris, aes(x = Petal.Width, y = Species))
ggally_facetdensity(iris, aes_string(x = "Species", y = "Petal.Width", color = "Species"))}
