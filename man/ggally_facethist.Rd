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
data(tips, package="reshape")
 ggally_facethist(tips, mapping = ggplot2::aes(x = tip, y = sex))
 ggally_facethist(tips, mapping = ggplot2::aes_string(x = "tip", y = "sex"), binwidth = 0.1)
}
\author{
  Barret Schloerke \email{schloerke@gmail.com}
}
\keyword{hplot}

