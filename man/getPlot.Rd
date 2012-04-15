\name{getPlot}
\alias{getPlot}
\title{getPlot
Retrieves the ggplot object at the desired location}
\usage{
getPlot(plotMatrix, rowFromTop, columnFromLeft)
}
\arguments{
  \item{plotMatrix}{ggpair object to select from}

  \item{rowFromTop}{row from the top}

  \item{columnFromLeft}{column from the left}
}
\description{
  getPlot Retrieves the ggplot object at the desired
  location
}
\examples{
plotMatrix2 <- ggpairs(tips[, 3:2], upper = list(combo = "denstrip"))
getPlot(plotMatrix2, 1, 2)
}
\author{
  Barret Schloerke \email{schloerke@gmail.com}
}
\keyword{hplot}
