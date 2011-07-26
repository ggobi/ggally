\name{ggally_diagAxis}
\alias{ggally_diagAxis}
\title{Internal Axis Labeling Plot for ggpairs...}
\usage{ggally_diagAxis(data, mapping, ...)}
\description{Internal Axis Labeling Plot for ggpairs
; is used when axisLabels == "internal"}
\author{Jason Crowley \email{crowley.jason.s@gmail.com}}
\arguments{\item{data}{dataset being plotted}
\item{mapping}{aesthetics being used (x is the variable the plot will be made for)}
\item{...}{other arguments for geom_text}}
\examples{ggally_diagAxis(iris,aes(x=Petal.Width))
ggally_diagAxis(iris,aes(x=Species))}
