\name{ggally_density}
\alias{ggally_density}
\title{Plots the Scatter Density Plot
Make a scatter density plot from a given data}
\usage{
ggally_density(data, mapping, ...)
}
\arguments{
  \item{data}{data set using}

  \item{mapping}{aesthetics being used}

  \item{...}{parameters sent to either stat_density2d or
  geom_density2d}
}
\description{
  The aesthetic "fill" determines whether or not
  stat_density2d (filled) or geom_density2d (lines) is
  used.
}
\examples{
ggally_density(iris, mapping = ggplot2::aes(x = Sepal.Length, 
    y = Petal.Length))
ggally_density(iris, mapping = ggplot2::aes_string(x = "Sepal.Length", 
    y = "Petal.Length"))
ggally_density(iris, mapping = ggplot2::aes_string(x = "Sepal.Length", 
    y = "Petal.Length", fill = "..level.."))
ggally_density(iris, mapping = ggplot2::aes_string(x = "Petal.Length", 
    y = "Petal.Width", fill = "..level..")) + scale_fill_gradient(breaks = c(0.05, 
    0.1, 0.15, 0.2))
}
\author{
  Barret Schloerke \email{schloerke@gmail.com}
}
\keyword{hplot}
