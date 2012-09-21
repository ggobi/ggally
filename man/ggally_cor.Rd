\name{ggally_cor}
\alias{ggally_cor}
\title{Correlation from the Scatter Plot
estimate correlation from the given data}
\usage{
  ggally_cor(data, mapping, corAlignPercent = 0.6,
    corSize = 3, corPValues = FALSE, ...)
}
\arguments{
  \item{data}{data set using}

  \item{mapping}{aesthetics being used}

  \item{corAlignPercent}{right align position of numbers.
  Default is 60 percent across the horizontal}

  \item{corSize}{size of text}

  \item{...}{other arguments being supplied to geom_text}
}
\description{
  Correlation from the Scatter Plot estimate correlation
  from the given data
}
\examples{
ggally_cor(tips, mapping = ggplot2::aes_string(x = "total_bill", y = "tip"))
ggally_cor(tips, mapping = ggplot2::aes_string(x = "total_bill", y = "tip", size = 15, colour = "red"))
ggally_cor(tips, mapping = ggplot2::aes_string(x = "total_bill", y = "tip", color = "sex"), corSize = 5, corPValues = TRUE)
}
\author{
  Barret Schloerke \email{schloerke@gmail.com}
}
\keyword{hplot}

