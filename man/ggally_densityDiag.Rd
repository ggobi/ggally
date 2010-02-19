\name{ggally_densityDiag}
\alias{ggally_densityDiag}
\title{Plots the Density Plots by Using Diagonal...}
\usage{ggally_densityDiag(data, mapping, ...)}
\description{Plots the Density Plots by Using Diagonal
Plots the density plots by using Diagonal}
\author{Barret Schloerke \email{bigbear@iastate.edu}}
\keyword{hplot}
\arguments{\item{data}{data set using}
\item{mapping}{aesthetics being used.}
\item{...}{other arguments sent to stat_density}}
\examples{ggally_densityDiag(iris, aes(x = Petal.Width))
ggally_densityDiag(movies, aes_string(x="rating"))
ggally_densityDiag(movies, aes_string(x="rating", colour = "mpaa"))}
