\name{ggplot_density}
\alias{ggplot_density}
\title{Plots the Scatter Density Plot...}
\usage{ggplot_density(data, filled=FALSE)}
\description{Plots the Scatter Density Plot
Make a scatter density plot from a given data}
\author{Barret Schloerke \email{bigbear@iastate.edu}, Haesung Kim \email{hae0510@iastate.edu}, Di Cook \email{dicook@iastate.edu} and Heike Hofmann \email{hofmann@iastate.edu}}
\keyword{hplot}
\arguments{\item{data}{data set using}
\item{filled}{Boolean asking if the contours should be filled.}}
\examples{ggplot_density(iris[,c(1,3)])
ggplot_density(iris[,c(1,3)], filled = TRUE)
ggplot_density(iris[,3:4],TRUE) + scale_fill_gradient(breaks = c(0.05, 0.1,0.15,0.2))}
