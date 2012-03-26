\name{ggally_box}
\alias{ggally_box}
\title{Plots the Box Plot
Make a box plot with a given data set}
\usage{
ggally_box(data, mapping, ...)
}
\arguments{
  \item{data}{data set using}

  \item{mapping}{aesthetics being used}

  \item{...}{other arguments being supplied to
  geom_boxplot}
}
\description{
  Plots the Box Plot Make a box plot with a given data set
}
\examples{
ggally_box(iris, mapping = ggplot2::aes(x = Petal.Width, 
    y = Species))
ggally_box(iris, mapping = ggplot2::aes_string(x = "Petal.Width", 
    y = "Species"))
ggally_box(iris, mapping = ggplot2::aes_string(y = "Petal.Width", 
    x = "Species", color = "Species"), outlier.colour = "red", 
    outlier.shape = 13, outlier.size = 18)
}
\author{
  Barret Schloerke \email{schloerke@gmail.com}
}
\keyword{hplot}
