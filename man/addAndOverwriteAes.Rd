\name{addAndOverwriteAes}
\alias{addAndOverwriteAes}
\title{Add new aes
Add new aesthetics to a previous aes}
\usage{
  addAndOverwriteAes(current, new)
}
\value{
  aes_string output
}
\description{
  Add new aes Add new aesthetics to a previous aes
}
\examples{
diamondMatrix <- ggpairs(
diamonds,
columns = 1:3,
upper = list(points = "scatterplot", aes_string = aes_string(color = "clarity")),
lower = list(points = "scatterplot", aes_string = aes_string(color = "cut")),
diag = "blank",
color = "color",
title = "Diamonds"
)
}
\author{
  Barret Schloerke \email{schloerke@gmail.com}
}
\keyword{internal}

