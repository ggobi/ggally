\name{ggplot_facethist}
\alias{ggplot_facethist}
\title{Plots the Histograms by Faceting...}
\usage{ggplot_facethist(data, ...)}
\description{Plots the Histograms by Faceting
Make histograms by displaying subsets of the data in different panels}
\author{Barret Schloerke \email{bigbear@iastate.edu}, Haesung Kim \email{hae0510@iastate.edu}, Di Cook \email{dicook@iastate.edu} and Heike Hofmann \email{hofmann@iastate.edu}}
\keyword{hplot}
\arguments{\item{data}{data set using}
\item{...}{}}
\examples{ggplot_facethist(iris[,4:5])
ggplot_facethist(iris[,5:4], binwidth = .1)}
