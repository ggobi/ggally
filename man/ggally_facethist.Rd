\name{ggally_facethist}
\alias{ggally_facethist}
\title{Plots the Histograms by Faceting
Make histograms by displaying subsets of the data in different panels}
\usage{
ggally_facethist(data, mapping, ...)
}
\arguments{
  \item{data}{data set using}

  \item{mapping}{aesthetics being used}

  \item{...}{parameters sent to stat_bin()}
}
\description{
  Plots the Histograms by Faceting Make histograms by
  displaying subsets of the data in different panels
}
\examples{
ggally_facethist(iris, mapping = ggplot2::aes(x = Petal.Width, 
    y = Species))
ggally_facethist(iris, mapping = ggplot2::aes_string(x = "Species", 
    y = "Petal.Width"), binwidth = 0.1)
}
\author{
  Barret Schloerke \email{schloerke@gmail.com}
}
\keyword{hplot}
