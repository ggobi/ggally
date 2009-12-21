\name{ggally_smooth}
\alias{ggally_smooth}
\title{Plots the Scatter Plot with Smoothing...}
\usage{ggally_smooth(data, ...)}
\description{Plots the Scatter Plot with Smoothing
Add a smoothed condition mean with a given scatter plot}
\author{Barret Schloerke \email{bigbear@iastate.edu}, Haesung Kim \email{hae0510@iastate.edu}, Di Cook \email{dicook@iastate.edu} and Heike Hofmann \email{hofmann@iastate.edu}}
\keyword{hplot}
\arguments{\item{data}{data set using}
\item{...}{other arguments to add to geom_point by aes_string(...)}}
\examples{ggally_smooth(iris, x = "Sepal.Length", y = "Sepal.Width")
ggally_smooth(iris, x = "Sepal.Length", y = "Petal.Length", colour = "Species")}
