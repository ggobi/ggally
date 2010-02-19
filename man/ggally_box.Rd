\name{ggally_box}
\alias{ggally_box}
\title{Plots the Box Plot...}
\usage{ggally_box(data, mapping, ...)}
\description{Plots the Box Plot
Make a box plot with a given data set}
\author{Barret Schloerke \email{bigbear@iastate.edu}}
\keyword{hplot}
\arguments{\item{data}{data set using}
\item{mapping}{aesthetics being used}
\item{...}{other arguments being supplied to geom_boxplot}}
\examples{ggally_box(iris, aes(x = Petal.Width, y = Species))
ggally_box(iris, aes_string(x = "Petal.Width", y = "Species"))
ggally_box(iris, aes_string(y = "Petal.Width", x = "Species", colour = "Species", outlier.colour = "red", outlier.shape = 13, outlier.size = 18))}
