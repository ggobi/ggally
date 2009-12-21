\name{ggally_ratio}
\alias{ggally_ratio}
\title{Plots a mosaic plots...}
\usage{ggally_ratio(data)}
\description{Plots a mosaic plots
Plots the mosaic plot by using fluctuation}
\author{Barret Schloerke \email{bigbear@iastate.edu}, Haesung Kim \email{hae0510@iastate.edu}, Di Cook \email{dicook@iastate.edu} and Heike Hofmann \email{hofmann@iastate.edu}}
\keyword{hplot}
\arguments{\item{data}{data set using}}
\examples{ggally_ratio(movies[,c("mpaa","Action")])
ggally_ratio(movies[,c("mpaa","Action")]) + coord_equal()
ggally_ratio(movies[,c("Action","mpaa")]) + opts(aspect.ratio = (length(levels(movies[,"mpaa"])) ) / (length(levels(as.factor(movies[,"Action"]))) ) )}
