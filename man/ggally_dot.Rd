\name{ggally_dot}
\alias{ggally_dot}
\title{Plots the Box Plot with Dot...}
\usage{ggally_dot(data, ...)}
\description{Plots the Box Plot with Dot
Add jittering with the box plot}
\author{Barret Schloerke \email{bigbear@iastate.edu}, Haesung Kim \email{hae0510@iastate.edu}, Di Cook \email{dicook@iastate.edu} and Heike Hofmann \email{hofmann@iastate.edu}}
\keyword{hplot}
\arguments{\item{data}{data set using}
\item{...}{other arguments being supplied to geom_jitter by aes_string(...)}}
\examples{ggally_dot(iris, x = "Petal.Width", y = "Species")
ggally_dot(iris, x = "Species", y = "Petal.Width", colour = "Species")}
