\name{ggally_cor}
\alias{ggally_cor}
\title{Correlation from the Scatter Plot...}
\usage{ggally_cor(data, ...)}
\description{Correlation from the Scatter Plot
estimate correlation from the given data}
\author{Barret Schloerke \email{bigbear@iastate.edu}, Haesung Kim \email{hae0510@iastate.edu}, Di Cook \email{dicook@iastate.edu} and Heike Hofmann \email{hofmann@iastate.edu}}
\keyword{hplot}
\arguments{\item{data}{data set using}
\item{...}{other arguments being supplied to geom_text by aes_string(...)}}
\examples{ggally_cor(iris, x = "Sepal.Length", y = "Petal.Length")
ggally_cor(iris, x = "Sepal.Length", y = "Petal.Length", size = 15, colour = "red")}
