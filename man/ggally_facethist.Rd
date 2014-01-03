\name{ggally_facethist}
\alias{ggally_facethist}
\title{Plots the Histograms by Faceting}
\usage{
  ggally_facethist(data, mapping, ...)
}
\arguments{
  \item{data}{data set using}

  \item{mapping}{aesthetics being used}

  \item{...}{parameters sent to stat_bin()}
}
\description{
  Make histograms by displaying subsets of the data in
  different panels.
}
\examples{
data(tips, package="reshape")
 ggally_facethist(tips, mapping = aes(x = tip, y = sex))
 ggally_facethist(tips, mapping = aes_string(x = "tip", y = "sex"), binwidth = 0.1)
}
\author{
  Barret Schloerke \email{schloerke@gmail.com}
}
\keyword{hplot}

