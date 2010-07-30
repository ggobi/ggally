\name{getPlot}
\alias{getPlot}
\title{getPlot...}
\usage{getPlot(plotMatrix, rowFromTop, columnFromLeft)}
\description{getPlot
Retrieves the ggplot object at the desired location}
\keyword{hplot}
\author{Barret Schloerke \email{schloerke@gmail.com}}
\arguments{\item{plotMatrix}{ggpair object to select from}
\item{rowFromTop}{row from the top}
\item{columnFromLeft}{column from the left}}
\examples{plotMatrix2 <- ggpairs(iris[,5:4], upper = list(combo = "denstrip"))
getPlot(plotMatrix2, 1, 2)}
