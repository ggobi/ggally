\name{ggally_denstrip}
\alias{ggally_denstrip}
\title{Plots a tile plot with facets
Make Tile Plot as densely as possible}
\usage{
  ggally_denstrip(data, mapping, ...)
}
\arguments{
  \item{data}{data set using}

  \item{mapping}{aesthetics being used}

  \item{...}{other arguments being sent to stat_bin}
}
\description{
  Plots a tile plot with facets Make Tile Plot as densely
  as possible
}
\examples{
ggally_denstrip(tips, mapping = ggplot2::aes(x = total_bill, y = sex))
 ggally_denstrip(tips, mapping = ggplot2::aes_string(x = "total_bill", y = "sex"))
 ggally_denstrip(tips, mapping = ggplot2::aes_string(x = "sex", y = "tip", binwidth = "0.2")) + scale_fill_gradient(low = "grey80", high = "black")
}
\author{
  Barret Schloerke \email{schloerke@gmail.com}
}
\keyword{hplot}

