\name{ggally_smooth}
\alias{ggally_smooth}
\title{Plots the Scatter Plot with Smoothing...}
\usage{ggally_smooth(data, mapping, ...)}
\description{Plots the Scatter Plot with Smoothing
Add a smoothed condition mean with a given scatter plot}
\author{Barret Schloerke \email{schloerke@gmail.com}}
\keyword{hplot}
\arguments{\item{data}{data set using}
\item{mapping}{aesthetics being used}
\item{...}{other arguments to add to geom_point}}
\examples{ggally_smooth(iris, aes(x = Sepal.Length, y = Sepal.Width))
ggally_smooth(iris, aes_string(x = "Sepal.Length", y = "Sepal.Width"))
ggally_smooth(iris, aes_string(x = "Sepal.Length", y = "Petal.Length", color = "Species"))}
