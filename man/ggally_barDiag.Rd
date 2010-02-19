\name{ggally_barDiag}
\alias{ggally_barDiag}
\title{Plots the Bar Plots by Using Diagonal...}
\usage{ggally_barDiag(data, mapping, ...)}
\description{Plots the Bar Plots by Using Diagonal
Plots the bar plots by using Diagonal}
\author{Barret Schloerke \email{bigbear@iastate.edu}}
\keyword{hplot}
\arguments{\item{data}{data set using}
\item{mapping}{aesthetics being used}
\item{...}{other arguements are sent to geom_bar}}
\examples{ggally_barDiag(movies, aes(x = mpaa))
ggally_barDiag(movies, aes_string(x = "mpaa"))
ggally_barDiag(movies, aes_string(x ="rating", binwidth = ".1"))}
