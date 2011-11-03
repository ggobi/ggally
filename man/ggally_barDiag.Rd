\name{ggally_barDiag}
\alias{ggally_barDiag}
\title{Plots the Bar Plots by Using Diagonal
Plots the bar plots by using Diagonal}
\usage{
  ggally_barDiag(data, mapping, ...)
}
\arguments{
  \item{data}{data set using}

  \item{mapping}{aesthetics being used}

  \item{...}{other arguements are sent to geom_bar}
}
\description{
  Plots the Bar Plots by Using Diagonal Plots the bar plots
  by using Diagonal
}
\examples{
ggally_barDiag(movies, aes(x = mpaa))
ggally_barDiag(movies, aes_string(x = "mpaa"))
ggally_barDiag(movies, aes_string(x ="rating", binwidth = ".1"))
}
\author{
  Barret Schloerke \email{schloerke@gmail.com}
}
\keyword{hplot}

