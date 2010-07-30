\name{ggally_density}
\alias{ggally_density}
\title{Plots the Scatter Density Plot...}
\usage{ggally_density(data, mapping, ...)}
\description{Plots the Scatter Density Plot
Make a scatter density plot from a given data}
\details{The aesthetic "fill" determines whether or not stat_density2d (filled) or geom_density2d (lines) is used.}
\author{Barret Schloerke \email{schloerke@gmail.com}}
\keyword{hplot}
\arguments{\item{data}{data set using}
\item{mapping}{aesthetics being used}
\item{...}{parameters sent to either stat_density2d or geom_density2d}}
\examples{ggally_density(iris, aes(x = Sepal.Length, y = Petal.Length))
ggally_density(iris, aes_string(x = "Sepal.Length", y = "Petal.Length"))
ggally_density(iris, aes_string(x = "Sepal.Length", y = "Petal.Length", fill = "..level.."))
ggally_density(iris, aes_string(x = "Petal.Length", y = "Petal.Width",fill = "..level..")) + scale_fill_gradient(breaks = c(0.05, 0.1,0.15,0.2))}
