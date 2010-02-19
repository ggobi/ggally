\name{ggally_text}
\alias{ggally_text}
\title{GGplot Text...}
\usage{ggally_text(label, mapping=aes(colour = "black"), xP=0.5, yP=0.5,
    xrange=c(0, 1), yrange=c(0, 1), ...)}
\description{GGplot Text
Plot text for a plot}
\author{Barret Schloerke \email{bigbear@iastate.edu}}
\keyword{hplot}
\arguments{\item{label}{text that you want to appear}
\item{mapping}{aesthetics that don't relate to position (such as colour)}
\item{xP}{horizontal position percentage}
\item{yP}{vertical position percentage}
\item{xrange}{range of the data around it.  Only nice to have if plotting in a matrix}
\item{yrange}{range of the data around it.  Only nice to have if plotting in a matrix}
\item{...}{other arguments for geom_text}}
\examples{ggally_text("Example 1")
ggally_text("Example\nTwo", mapping = aes_string(size = 15, colour = "red"))}
