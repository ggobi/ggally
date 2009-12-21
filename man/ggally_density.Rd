\name{ggally_density}
\alias{ggally_density}
\title{Plots the Scatter Density Plot...}
\usage{ggally_density(data, ...)}
\description{Plots the Scatter Density Plot
Make a scatter density plot from a given data}
\author{Barret Schloerke \email{bigbear@iastate.edu}, Haesung Kim \email{hae0510@iastate.edu}, Di Cook \email{dicook@iastate.edu} and Heike Hofmann \email{hofmann@iastate.edu}}
\keyword{hplot}
\arguments{\item{data}{data set using}
\item{...}{parameters sent to aes_string(...).  Will effect either stat_density2d or geom_density2d}}
\examples{ggally_density(iris, x = "Sepal.Length", y = "Petal.Length")
ggally_density(iris, x = "Sepal.Length", y = "Petal.Length", filled = "TRUE")
ggally_density(iris, x = "Petal.Length", y = "Petal.Width",filled = "TRUE") + scale_fill_gradient(breaks = c(0.05, 0.1,0.15,0.2))}
