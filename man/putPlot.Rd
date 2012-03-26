\name{putPlot}
\alias{putPlot}
\title{Put Plot
Function to place your own plot in the layout}
\usage{
putPlot(plotMatrix, plotObj, rowFromTop, columnFromLeft)
}
\arguments{
  \item{plotMatrix}{ggally object to be altered}

  \item{plotObj}{ggplot object to be placed}

  \item{rowFromTop}{row from the top}

  \item{columnFromLeft}{column from the left}
}
\description{
  Put Plot Function to place your own plot in the layout
}
\examples{
plotMatrix <- ggpairs(mtcars[, c("mpg", "wt", 
    "cyl")], upper = "blank", title = "Custom Example")
# ggplot example taken from example(geom_text)
plot <- ggplot(mtcars, aes(x = wt, y = mpg, label = rownames(mtcars)))
plot <- plot + geom_text(aes(colour = factor(cyl)), 
    size = 3) + scale_colour_discrete(l = 40)
plotMatrix <- putPlot(plotMatrix, plot, 1, 2)
plotMatrix <- putPlot(plotMatrix, ggally_text("ggpairs allows you\\nto put in your\\nown plot.\\nLike that one.\\n <---"), 
    1, 3)
plotMatrix
}
\author{
  Barret Schloerke \email{schloerke@gmail.com}
}
\keyword{hplot}
