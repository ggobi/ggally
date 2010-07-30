\name{ggally_facethist}
\alias{ggally_facethist}
\title{Plots the Histograms by Faceting...}
\usage{ggally_facethist(data, mapping, ...)}
\description{Plots the Histograms by Faceting
Make histograms by displaying subsets of the data in different panels}
\author{Barret Schloerke \email{schloerke@gmail.com}}
\keyword{hplot}
\arguments{\item{data}{data set using}
\item{mapping}{aesthetics being used}
\item{...}{parameters sent to stat_bin()}}
\examples{ggally_facethist(iris, aes(x = Petal.Width, y = Species))
ggally_facethist(iris, aes_string(x = "Species", y = "Petal.Width"), binwidth = 0.1)}
