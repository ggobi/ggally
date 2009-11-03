\name{ggally_box}
\alias{ggally_box}
\title{Plots the Box Plot...}
\usage{ggally_box(data, ...)}

\description{
  Plots the Box Plot Make a box plot with a given data set
}
\author{Barret Schloerke \email{bigbear@iastate.edu}, Haesung Kim \email{hae0510@iastate.edu}, Di Cook \email{dicook@iastate.edu} and Heike Hofmann \email{hofmann@iastate.edu}}
\keyword{hplot}
\arguments{
  \item{data}{data set using}
  \item{...}{other arguments being supplied to geom_boxplot by aes_string(...)}
}
\examples{ggally_box(iris, x = "Petal.Width", y = "Species")
ggally_box(iris, y = "Petal.Width", x = "Species", colour = "Species", outlier.colour = "red", outlier.shape = 13, outlier.size = 18)}
