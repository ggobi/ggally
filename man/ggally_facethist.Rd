\name{ggally_facethist}
\alias{ggally_facethist}
\title{Plots the Histograms by Faceting...}
\usage{ggally_facethist(data, ...)}

\description{
  Plots the Histograms by Faceting Make histograms by displaying
  subsets of the data in different panels
}
\author{Barret Schloerke \email{bigbear@iastate.edu}, Haesung Kim \email{hae0510@iastate.edu}, Di Cook \email{dicook@iastate.edu} and Heike Hofmann \email{hofmann@iastate.edu}}
\keyword{hplot}
\arguments{
  \item{data}{data set using}
  \item{...}{parameters sent to aes_string(...).  It will effect stat_bin()}
}
\examples{ggally_facethist(iris, x = "Petal.Width", y = "Species")
ggally_facethist(iris, x = "Species", y = "Petal.Width", binwidth = "0.1")}
