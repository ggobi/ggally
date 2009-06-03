\name{ggplot_points}
\alias{ggplot_points}
\title{Plots the Scatter Plot...}
\usage{ggplot_points(data, ...)}
\description{Plots the Scatter Plot
Make a scatter plot with a given data set}
\author{Barret Schloerke \email{bigbear@iastate.edu}, Haesung Kim \email{hae0510@iastate.edu}, Di Cook \email{dicook@iastate.edu} and Heike Hofmann \email{hofmann@iastate.edu}}
\keyword{hplot}
\arguments{\item{data}{data set using}
\item{...}{other arguments to add to geom_point}}
\examples{ggplot_points(mtcars[,3:4])
ggplot_points(mtcars[,3:4],colour = as.factor(mtcars[,"cyl"]), size = 1:nrow(mtcars)/nrow(mtcars) * 6)}
