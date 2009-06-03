\name{ggpairs}
\alias{ggpairs}
\title{GGpairs - GGplot2 Matrix...}
\usage{ggpairs(data, colour=black,
    upper=list(scatter = "cor",
    conditional = "facethist",
    mosaic = "rata"),
    lower=list(scatter = "points",
    conditional = "box", mosaic =
    "rata"), identity=list(scatter
    = "density", conditional =
    "bar"), title, ...)}
\description{GGpairs - GGplot2 Matrix
Make a matrix of plots with a given data set}
\keyword{hplot}
\author{Barret Schloerke \email{bigbear@iastate.edu}}
\arguments{\item{data}{data set using.  Can have both numerical and categorical data.}
\item{colour}{colour of the points}
\item{title}{title for the graph}
\item{upper}{see Details}
\item{lower}{see Details}
\item{identity}{see Details}
\item{...}{other parameters being supplied to geoms, such as binwidth}}
\examples{ggpairs(iris)
ggpairs(iris[,3:5])
ggpairs(
iris[,3:5], 
upper = list(scatter = "density", conditional = "box"), 
lower = list(scatter = "points", conditional = "dot"), 
iden = list(scatter = "bar", conditional = "bar")
)
ggpairs(
iris[,3:5],
upper = list(scatter = "density", conditional = "facetdensity"),
lower = "blank",
identity = list(scatter = "bar", conditional = "blank")
)

ggpairs(
iris[,3:4],
upper = list(scatter = "wrongType", conditional = "wrongType"),
lower = list(scatter = "IDK", conditional = "5:1", mosaic = "mosaic"),
identity = list(scatter = "points", conditional = "box")
)

ggpairs(mtcars[,c("mpg","wt","cyl")], upper = "blank")
plot <- ggplot(mtcars, aes(x=wt, y=mpg, label=rownames(mtcars))) + geom_text(aes(colour=factor(cyl)), size = 3) + scale_colour_discrete(l=40)
putPlot(1,2,plot)
putPlot(1,2,plot,axes = FALSE)
putPlot(1,3,ggplot_text("ggpairs allows you\nto put in your\nown plot.\nLike that one.\n <---"),axes = FALSE)

ggpairs(iris[,5:4], upper = list(conditional = "denstrip"))
putPlot(1,1,ggplot_text("ggpairs allows you\nto retrieve a plot.\nWe will grab this one,\n-->\nwith the legend!"),axes = FALSE)
getPlot(1,2) -> plot
plot
plotNew <- plot + scale_fill_gradient(low = "grey80", high = "black")
plotNew
ggpairs(iris[,5:4], upper = list(conditional = "denstrip"))
putPlot(1,2,plotNew,axes = FALSE)}
