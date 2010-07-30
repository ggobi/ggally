\name{ggally_facetdensitystrip}
\alias{ggally_facetdensitystrip}
\title{Plots a density plot with facets or a tile plot with facets...}
\usage{ggally_facetdensitystrip(data, mapping, ..., den_strip=FALSE)}
\description{Plots a density plot with facets or a tile plot with facets
Make Tile Plot as densely as possible}
\author{Barret Schloerke \email{schloerke@gmail.com}}
\keyword{hplot}
\arguments{\item{data}{data set using}
\item{mapping}{aesthetics being used}
\item{...}{other arguments being sent to either stat_bin or stat_density}
\item{den_strip}{boolean to deceide whether or not to plot a density strip(TRUE) or a facet density(FALSE) plot.}}
\examples{example(ggally_facetdensity)
example(ggally_denstrip)}
