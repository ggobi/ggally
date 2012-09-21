\name{ggally_diagAxis}
\alias{ggally_diagAxis}
\title{Internal Axis Labeling Plot for ggpairs
; is used when axisLabels == "internal"}
\usage{
  ggally_diagAxis(data, mapping, labelSize = 5,
    labelXPercent = 0.5, labelYPercent = 0.55,
    labelHJust = 0.5, labelVJust = 0.5, gridLabelSize = 4,
    ...)
}
\arguments{
  \item{data}{dataset being plotted}

  \item{mapping}{aesthetics being used (x is the variable
  the plot will be made for)}

  \item{labelSize}{size of variable label}

  \item{labelXPercent}{percent of horizontal range}

  \item{labelYPercent}{percent of vertical range}

  \item{labelHJust}{hjust supplied to label}

  \item{labelVJust}{vjust supplied to label}

  \item{gridLabelSize}{size of grid labels}

  \item{...}{other arguments for geom_text}
}
\description{
  Internal Axis Labeling Plot for ggpairs ; is used when
  axisLabels == "internal"
}
\examples{
ggally_diagAxis(tips, aes(x=tip))
ggally_diagAxis(tips,aes(x=sex))
}
\author{
  Jason Crowley \email{crowley.jason.s@gmail.com} and
  Barret Schloerke
}

