\name{print.ggpairs}
\alias{print.ggpairs}
\title{Print ggpair object}
\usage{
  \method{print}{ggpairs} (x, ...)
}
\arguments{
  \item{x}{ggpair object to be plotted}

  \item{...}{not used}
}
\description{
  Specialized method to print the ggpair object-
}
\examples{
data(tips, package="reshape")
 ggpairs(tips[,1:3])
}
\author{
  Barret Schloerke \email{schloerke@gmail.com}
}
\keyword{internal}

