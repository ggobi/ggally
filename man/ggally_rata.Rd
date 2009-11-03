\name{ggally_rata}
\alias{ggally_rata}
\title{Plots a mosaic plots...}
\usage{ggally_rata(data)}

\description{
  Plots a mosaic plots Plots the mosaic plot by using fluctuation
}
\author{Barret Schloerke \email{bigbear@iastate.edu}, Haesung Kim \email{hae0510@iastate.edu}, Di Cook \email{dicook@iastate.edu} and Heike Hofmann \email{hofmann@iastate.edu}}
\keyword{hplot}
\arguments{
  \item{data}{data set using}
}
\examples{ggally_rata(movies[,c("mpaa","Action")])
ggally_rata(movies[,c("mpaa","Action")]) + coord_equal()
ggally_rata(movies[,c("Action","mpaa")]) + opts(aspect.ratio = (length(levels(movies[,"mpaa"])) ) / (length(levels(as.factor(movies[,"Action"]))) ) )}
