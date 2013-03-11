\name{addAndOverwriteAes}
\alias{addAndOverwriteAes}
\title{Add new aes}
\usage{
  addAndOverwriteAes(current, new)
}
\value{
  aes_string output
}
\description{
  Add new aesthetics to a previous aes.
}
\examples{
data(diamonds, package="ggplot2")
 diamonds.samp <- diamonds[sample(1:dim(diamonds)[1],100),]
 ggpairs(diamonds.samp, columns = 5:7,
  upper = list(continuous = "cor", aes_string = aes_string(color = "clarity")),
  lower = list(continuous = "cor", aes_string = aes_string(color = "cut")),
  color = "color",
  title = "Diamonds Sample")
}
\author{
  Barret Schloerke \email{schloerke@gmail.com}
}
\keyword{internal}

