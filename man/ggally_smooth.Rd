\name{ggally_smooth}
\alias{ggally_smooth}
\title{Plots the Scatter Plot with Smoothing
Add a smoothed condition mean with a given scatter plot}
\usage{
  ggally_smooth(data, mapping, ...)
}
\arguments{
  \item{data}{data set using}

  \item{mapping}{aesthetics being used}

  \item{...}{other arguments to add to geom_point}
}
\description{
  Plots the Scatter Plot with Smoothing Add a smoothed
  condition mean with a given scatter plot
}
\examples{
ggally_smooth(iris, aes(x = Sepal.Length, y = Sepal.Width))
ggally_smooth(iris, aes_string(x = "Sepal.Length", y = "Sepal.Width"))
ggally_smooth(iris, aes_string(x = "Sepal.Length", y = "Petal.Length", color = "Species"))
}
\author{
  Barret Schloerke \email{schloerke@gmail.com}
}
\keyword{hplot}

