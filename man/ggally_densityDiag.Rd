\name{ggally_densityDiag}
\alias{ggally_densityDiag}
\title{Plots the Density Plots by Using Diagonal
Plots the density plots by using Diagonal}
\usage{
  ggally_densityDiag(data, mapping, ...)
}
\arguments{
  \item{data}{data set using}

  \item{mapping}{aesthetics being used.}

  \item{...}{other arguments sent to stat_density}
}
\description{
  Plots the Density Plots by Using Diagonal Plots the
  density plots by using Diagonal
}
\examples{
ggally_densityDiag(tips, mapping = ggplot2::aes(x = total_bill))
#library(ggplot2)
#data(movies)
#ggally_densityDiag(movies, mapping = ggplot2::aes_string(x="rating"))
#ggally_densityDiag(movies, mapping = ggplot2::aes_string(x="rating", color = "mpaa"))
}
\author{
  Barret Schloerke \email{schloerke@gmail.com}
}
\keyword{hplot}

