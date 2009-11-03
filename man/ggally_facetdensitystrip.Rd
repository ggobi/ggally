\name{ggally_facetdensitystrip}
\alias{ggally_facetdensitystrip}
\title{Plots a density plot with facets or a tile plot with facets...}
\usage{ggally_facetdensitystrip(data, ..., den_strip=FALSE)}

\description{
  Plots a density plot with facets or a tile plot with facets Make Tile
  Plot as densely as possible
}
\author{Barret Schloerke \email{bigbear@iastate.edu}, Haesung Kim \email{hae0510@iastate.edu}, Di Cook \email{dicook@iastate.edu} and Heike Hofmann \email{hofmann@iastate.edu}}
\keyword{hplot}
\arguments{
  \item{data}{data set using}
  \item{...}{other arguments being sent to stat_bin's by aes_string(...)}
  \item{den_strip}{boolean to deceide whether or not to plot a density strip(TRUE) or a facet density(FALSE) plot.}
}
\examples{example(ggally_facetdensity)
example(ggally_denstrip)}
