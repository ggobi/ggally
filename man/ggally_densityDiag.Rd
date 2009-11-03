\name{ggally_densityDiag}
\alias{ggally_densityDiag}
\title{Plots the Density Plots by Using Diagonal...}
\usage{ggally_densityDiag(data, ...)}

\description{
  Plots the Density Plots by Using Diagonal Plots the density plots by
  using Diagonal
}
\author{Barret Schloerke \email{bigbear@iastate.edu}, Haesung Kim \email{hae0510@iastate.edu}, Di Cook \email{dicook@iastate.edu} and Heike Hofmann \email{hofmann@iastate.edu}}
\keyword{hplot}
\arguments{
  \item{data}{data set using}
  \item{...}{}
}
\examples{ggally_densityDiag(iris,x = "Petal.Width")
ggally_densityDiag(movies, x="rating")
ggally_densityDiag(movies, x="rating", colour = "mpaa")}
