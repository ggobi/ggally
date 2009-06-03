\name{ggplot_text}
\alias{ggplot_text}
\title{GGplot Text...}
\usage{ggplot_text(label, xP=0.5, yP=0.5,
    xrange=c(0, 1), yrange=c(0,
    1), ...)}
\description{GGplot Text
Plot text for a plot}
\author{Barret Schloerke \email{bigbear@iastate.edu}, Haesung Kim \email{hae0510@iastate.edu}, Di Cook \email{dicook@iastate.edu} and Heike Hofmann \email{hofmann@iastate.edu}}
\keyword{hplot}
\arguments{\item{xP}{horizontal position percentage}
\item{yP}{vertical position percentage}
\item{label}{text that you want to appear}
\item{xrange}{range of the data around it.  Only nice to have if plotting in a matrix}
\item{yrange}{range of the data around it.  Only nice to have if plotting in a matrix}
\item{...}{other arguments for geom_text}}
\examples{ggplot_text("Example 1")
ggplot_text("Example\nTwo",size = 15, colour = "red")}
