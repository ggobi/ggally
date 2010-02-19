\name{ggally_ratio}
\alias{ggally_ratio}
\title{Plots a mosaic plots...}
\usage{ggally_ratio(data)}
\description{Plots a mosaic plots
Plots the mosaic plot by using fluctuation}
\details{Must send only two discrete columns in the data set.}
\author{Barret Schloerke \email{bigbear@iastate.edu}}
\keyword{hplot}
\arguments{\item{data}{data set using}}
\examples{ggally_ratio(movies[,c("mpaa","Action")])
ggally_ratio(movies[,c("mpaa","Action")]) + coord_equal()
ggally_ratio(movies[,c("Action","mpaa")]) + opts(aspect.ratio = (length(levels(movies[,"mpaa"])) ) / (length(levels(as.factor(movies[,"Action"]))) ) )}
