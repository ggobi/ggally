\name{ggplot_dot}
\alias{ggplot_dot}
\title{Plots the Box Plot with Dot...}
\usage{ggplot_dot(data, ...)}
\description{Plots the Box Plot with Dot
Add jittering with the box plot}
\author{Barret Schloerke \email{bigbear@iastate.edu}, Haesung Kim \email{hae0510@iastate.edu}, Di Cook \email{dicook@iastate.edu} and Heike Hofmann \email{hofmann@iastate.edu}}
\keyword{hplot}
\arguments{\item{data}{data set using}
\item{...}{}}
\examples{ggplot_dot(iris[,4:5])
ggplot_dot(iris[,5:4],colour = as.factor(iris[,"Species"]))}
