\name{ggally_dot}
\alias{ggally_dot}
\title{Plots the Box Plot with Dot...}
\usage{ggally_dot(data, mapping, ...)}
\description{Plots the Box Plot with Dot
Add jittering with the box plot}
\author{Barret Schloerke \email{schloerke@gmail.com}}
\keyword{hplot}
\arguments{\item{data}{data set using}
\item{mapping}{aesthetics being used}
\item{...}{other arguments being supplied to geom_jitter}}
\examples{ggally_dot(iris, aes(x = Petal.Width, y = Species))
ggally_dot(iris, aes_string(x = "Petal.Width", y = "Species"))
ggally_dot(iris, aes_string(x = "Species", y = "Petal.Width", color = "Species"))
ggally_dot(iris, aes_string(x = "Species", y = "Petal.Width", color = "Species", shape = "Species")) + scale_shape(solid=FALSE)}
