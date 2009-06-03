\name{ggplot_bar}
\alias{ggplot_bar}
\title{Plots the Bar Plots by Using Identity...}
\usage{ggplot_bar(data, ...)}
\description{Plots the Bar Plots by Using Identity
Plots the bar plots by using identity}
\author{Barret Schloerke \email{bigbear@iastate.edu}, Haesung Kim \email{hae0510@iastate.edu}, Di Cook \email{dicook@iastate.edu} and Heike Hofmann \email{hofmann@iastate.edu}}
\keyword{hplot}
\arguments{\item{data}{data set using}
\item{...}{}}
\examples{ggplot_bar(movies[,c("mpaa","mpaa")])
ggplot_bar(movies[,c("rating","rating")], binwidth = .1)}
