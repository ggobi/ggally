\name{ggally_diagAxis}
\alias{ggally_diagAxis}
\title{Internal Axis Labeling Plot for ggpairs
; is used when axisLabels == "internal"}
\usage{
ggally_diagAxis(data, mapping, ...)
}
\arguments{
  \item{data}{dataset being plotted}

  \item{mapping}{aesthetics being used (x is the variable
  the plot will be made for)}

  \item{...}{other arguments for geom_text}
}
\description{
  Internal Axis Labeling Plot for ggpairs ; is used when
  axisLabels == "internal"
}
\examples{
ggally_diagAxis(iris, aes(x = Petal.Width))
ggally_diagAxis(iris, aes(x = Species))
}
\author{
  Jason Crowley \email{crowley.jason.s@gmail.com}
}
