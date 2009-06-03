\name{ggplot_box}
\alias{ggplot_box}
\title{Plots the Box Plot...}
\usage{ggplot_box(data, ...)}
\description{Plots the Box Plot
Make a box plot with a given data set}
\author{Barret Schloerke \email{bigbear@iastate.edu}, Haesung Kim \email{hae0510@iastate.edu}, Di Cook \email{dicook@iastate.edu} and Heike Hofmann \email{hofmann@iastate.edu}}
\keyword{hplot}
\arguments{\item{data}{data set using}
\item{...}{}}
\examples{ggplot_box(iris[,4:5])
ggplot_box(iris[,5:4], outlier.colour = "red", outlier.shape = 13, outlier.size = 8)}
