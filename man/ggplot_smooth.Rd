\name{ggplot_smooth}
\alias{ggplot_smooth}
\title{Plots the Scatter Plot with Smoothing...}
\usage{ggplot_smooth(data, ...)}
\description{Plots the Scatter Plot with Smoothing
Add a smoothed condition mean with a given scatter plot}
\author{Barret Schloerke \email{bigbear@iastate.edu}, Haesung Kim \email{hae0510@iastate.edu}, Di Cook \email{dicook@iastate.edu} and Heike Hofmann \email{hofmann@iastate.edu}}
\keyword{hplot}
\arguments{\item{data}{data set using}
\item{...}{other arguments to add to geom_point}}
\examples{ggplot_smooth(iris[, 1:2])
ggplot_smooth(iris[, c(1, 3)], colour = as.factor(iris[, "Species"]))}
