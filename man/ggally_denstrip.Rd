\name{ggally_denstrip}
\alias{ggally_denstrip}
\title{Plots a tile plot with facets...}
\usage{ggally_denstrip(data, ...)}
\description{Plots a tile plot with facets
Make Tile Plot as densely as possible}
\author{Barret Schloerke \email{bigbear@iastate.edu}, Haesung Kim \email{hae0510@iastate.edu}, Di Cook \email{dicook@iastate.edu} and Heike Hofmann \email{hofmann@iastate.edu}}
\keyword{hplot}
\arguments{\item{data}{data set using}
\item{...}{other arguments being sent to stat_bin's by aes_string(...)}}
\examples{ggally_denstrip(iris, x = "Petal.Width", y = "Species")
ggally_denstrip(iris, x = "Species", y = "Petal.Width", binwidth = "0.2") + scale_fill_gradient(low = "grey80", high = "black")}
