\name{ggplot_rata}
\alias{ggplot_rata}
\title{Plots the Mosaic Plots...}
\usage{ggplot_rata(data)}
\description{Plots the Mosaic Plots
Plots the mosaic plot by using fluctuation}
\author{Barret Schloerke \email{bigbear@iastate.edu}, Haesung Kim \email{hae0510@iastate.edu}, Di Cook \email{dicook@iastate.edu} and Heike Hofmann \email{hofmann@iastate.edu}}
\keyword{hplot}
\arguments{\item{data}{data set using}}
\examples{ggplot_rata(movies[,c("mpaa","Action")])
ggplot_rata(movies[,c("mpaa","Action")]) + coord_equal()
ggplot_rata(movies[,c("Action","mpaa")]) + opts(aspect.ratio = (length(levels(movies[,"mpaa"])) ) / (length(levels(as.factor(movies[,"Action"]))) ) )}
