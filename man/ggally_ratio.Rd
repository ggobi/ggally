\name{ggally_ratio}
\alias{ggally_ratio}
\title{Plots a mosaic plots}
\usage{
ggally_ratio(data)
}
\arguments{
  \item{data}{data set using}
}
\description{
Plots the mosaic plot by using fluctuation.
}
\details{
Must send only two discrete columns in the data set.
}
\examples{
data(movies, package = "ggplot2")
ggally_ratio(movies[,c("mpaa","Action")])
ggally_ratio(movies[,c("mpaa","Action")]) + coord_equal()
nummpaa <- length(levels(movies[,"mpaa"]))
numAction <- length(levels(as.factor(movies[,"Action"])))
ggally_ratio(
  movies[,c("Action","mpaa")]
) + theme(
  aspect.ratio = nummpaa / numAction
)
}
\author{
Barret Schloerke \email{schloerke@gmail.com}
}
\keyword{hplot}

