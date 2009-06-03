\name{ggplot_densityI}
\alias{ggplot_densityI}
\title{Plots the Density Plots by Using Identity...}
\usage{ggplot_densityI(data, ...)}
\description{Plots the Density Plots by Using Identity
Plots the density plots by using identity}
\author{Barret Schloerke \email{bigbear@iastate.edu}, Haesung Kim \email{hae0510@iastate.edu}, Di Cook \email{dicook@iastate.edu} and Heike Hofmann \email{hofmann@iastate.edu}}
\keyword{hplot}
\arguments{\item{data}{data set using}
\item{...}{}}
\examples{ggplot_densityI(movies[,c("rating","rating")])
ggplot_densityI(movies[,c("rating","rating")], colour = "blue")}
