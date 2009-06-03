\name{ggplot_denstrip}
\alias{ggplot_denstrip}
\title{Plots the Tile Plot...}
\usage{ggplot_denstrip(data, ...)}
\description{Plots the Tile Plot
Make Tile Plot as densely as possible}
\author{Barret Schloerke \email{bigbear@iastate.edu}, Haesung Kim \email{hae0510@iastate.edu}, Di Cook \email{dicook@iastate.edu} and Heike Hofmann \email{hofmann@iastate.edu}}
\keyword{hplot}
\arguments{\item{data}{data set using}
\item{...}{}}
\examples{ggplot_denstrip(iris[,4:5])
ggplot_denstrip(iris[,5:4], binwidth = .2) + scale_fill_gradient(low = "grey80", high = "black")}
