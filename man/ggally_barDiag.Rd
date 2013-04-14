\name{ggally_barDiag}
\alias{ggally_barDiag}
\title{Plots the Bar Plots by Using Diagonal}
\usage{
  ggally_barDiag(data, mapping, ...)
}
\arguments{
  \item{data}{data set using}

  \item{mapping}{aesthetics being used}

  \item{...}{other arguements are sent to geom_bar}
}
\description{
  Plots the bar plots by using Diagonal.
}
\examples{
library(ggplot2)
data(movies)
ggally_barDiag(movies, mapping = ggplot2::aes(x = mpaa))
# ggally_barDiag(movies, mapping = ggplot2::aes_string(x = "mpaa"))
# ggally_barDiag(movies, mapping = ggplot2::aes_string(x ="rating", binwidth = ".1"))
}
\author{
  Barret Schloerke \email{schloerke@gmail.com}
}
\keyword{hplot}

