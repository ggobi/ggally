\name{ggally_facetdensity}
\alias{ggally_facetdensity}
\title{Plots the density plots by faceting
Make density plots by displaying subsets of the data in different panels}
\usage{
ggally_facetdensity(data, mapping, ...)
}
\arguments{
  \item{data}{data set using}

  \item{mapping}{aesthetics being used}

  \item{...}{other arguments being sent to stat_density}
}
\description{
  Plots the density plots by faceting Make density plots by
  displaying subsets of the data in different panels
}
\examples{
ggally_facetdensity(iris, mapping = ggplot2::aes(x = Petal.Width, 
    y = Species))
ggally_facetdensity(iris, mapping = ggplot2::aes_string(x = "Species", 
    y = "Petal.Width", color = "Species"))
}
\author{
  Barret Schloerke \email{schloerke@gmail.com}
}
\keyword{hplot}
