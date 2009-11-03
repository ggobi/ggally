\name{ggally_points}
\alias{ggally_points}
\title{Plots the Scatter Plot...}
\usage{ggally_points(data, ...)}

\description{
  Plots the Scatter Plot Make a scatter plot with a given data set
}
\author{Barret Schloerke \email{bigbear@iastate.edu}, Haesung Kim \email{hae0510@iastate.edu}, Di Cook \email{dicook@iastate.edu} and Heike Hofmann \email{hofmann@iastate.edu}}
\keyword{hplot}
\arguments{
  \item{data}{data set using}
  \item{...}{other arguments to add to geom_point by aes_string(...)}
}
\examples{ggally_points(mtcars, x = "disp", y = "hp")
ggally_points(mtcars, x = "disp", y = "hp", colour = "as.factor(cyl)", size = "gear")}
