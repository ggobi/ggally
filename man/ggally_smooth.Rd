\name{ggally_smooth}
\alias{ggally_smooth}
\title{Plots the Scatter Plot with Smoothing
Add a smoothed condition mean with a given scatter plot}
\usage{
ggally_smooth(data, mapping, ...)
}
\arguments{
  \item{data}{data set using}

  \item{mapping}{aesthetics being used}

  \item{...}{other arguments to add to geom_point}
}
\description{
  Plots the Scatter Plot with Smoothing Add a smoothed
  condition mean with a given scatter plot
}
\examples{
ggally_smooth(tips, mapping = ggplot2::aes(x = total_bill, y = tip))
ggally_smooth(tips, mapping = ggplot2::aes_string(x = "total_bill", 
    y = "tip"))
ggally_smooth(tips, mapping = ggplot2::aes_string(x = "total_bill", 
    y = "tip", color = "sex"))
}
\author{
  Barret Schloerke \email{schloerke@gmail.com}
}
\keyword{hplot}
