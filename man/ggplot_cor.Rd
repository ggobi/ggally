\name{ggplot_cor}
\alias{ggplot_cor}
\title{Correlation from the Scatter Plot...}
\usage{ggplot_cor(data, size=8,
    colour=black, ...)}
\description{Correlation from the Scatter Plot
estimate correlation from the given data}
\author{Barret Schloerke \email{bigbear@iastate.edu}, Haesung Kim \email{hae0510@iastate.edu}, Di Cook \email{dicook@iastate.edu} and Heike Hofmann \email{hofmann@iastate.edu}}
\keyword{hplot}
\arguments{\item{data}{data set using}
\item{size}{setting size}
\item{colour}{setting color}
\item{...}{other arguments being supplied to geom_text}}
\examples{ggplot_cor(iris[,c(1,3)])
ggplot_cor(iris[,c(1,3)], size = 15, colour = "red")}
