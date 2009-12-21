\name{ggpairs}
\alias{ggpairs}
\title{ggpairs - A GGplot2 Matrix...}
\usage{ggpairs(data, colour=black, upper=list(scatter = "cor", conditional =
    "facethist", mosaic = "ratio"), lower=list(scatter = "points",
    conditional = "box", mosaic = "ratio"), diag=list(scatter =
    "density", conditional = "bar"), title, ...)}
\description{ggpairs - A GGplot2 Matrix
Make a matrix of plots with a given data set}
\details{upper and lower are lists that may contain the variables 'continuous', 
'combo' and 'discrete'. Each element of the list is a string implementing
the following options: continuous = exactly one of ('points', 'smooth', 
'density', 'cor', 'blank'); 'combo' = exactly one of ('box', 'dot', 
'facethist', 'facetdensity', 'denstrip', 'blank'); mosaic = exactly one 
of ('ratio', 'blank').

diag is a list that may only contain the variables 'continuous' and 'discrete'. 
Each element of the diag list is a string implmenting the following options: 
continuous = exactly one of ('density', 'bar', 'blank'); discrete = exactly one
of ('bar', 'blank').

If a list option it will be set to the function default.  If 'blank' is ever 
chosen as an option, then ggpairs will produce a blank plot, as if nothing was 
printed there.}
\keyword{hplot}
\author{Barret Schloerke \email{bigbear@iastate.edu}}
\value{ggpair object that if called, will reprint}
\arguments{\item{data}{data set using.  Can have both numerical and categorical data.}
\item{colour}{colour of the points}
\item{title}{title for the graph}
\item{upper}{see Details}
\item{lower}{see Details}
\item{diag}{see Details}
\item{...}{other parameters being supplied to geom's aes, such as color}}
\examples{ggpairs(iris)
ggpairs(iris[,3:5])
ggpairs(
iris[,3:5], 
upper = list(scatter = "density", conditional = "box"), 
lower = list(scatter = "points", conditional = "dot"), 
diag = list(scatter = "bar", conditional = "bar")
)

ggpairs(
iris[,3:5],
upper = list(scatter = "density", conditional = "facetdensity"),
lower = "blank",
diag = list(scatter = "bar", conditional = "blank")
)


diamondMatrix <- ggpairs(	
diamonds[,1:3], 	
upper = list(scatter = "density", conditional = "box"), 	
lower = list(scatter = "points", conditional = "dot"), 	
diag = list(scatter = "bar", conditional = "bar"), 
color = "cut", 
filled = TRUE,
title = "Diamonds"
)
diamondMatrix


#will plot four "Incorrect Plots"	
ggpairs(
iris[,3:4],
upper = list(scatter = "wrongType1", conditional = "wrongType2"),
lower = list(scatter = "IDK1", conditional = "IDK2", mosaic = "mosaic"),
diag = list(scatter = "points", conditional = "box")
)


#Custom Examples
plotMatrix <- ggpairs(mtcars[,c("mpg","wt","cyl")], upper = "blank", title = "Custom Example")
# ggplot example taken from example(geom_text)
plot <- ggplot(mtcars, aes(x=wt, y=mpg, label=rownames(mtcars)))
plot <- plot + geom_text(aes(colour=factor(cyl)), size = 3) + scale_colour_discrete(l=40)
plotMatrix <- putPlot(plotMatrix, plot, 1, 2)
plotMatrix <- putPlot(plotMatrix, ggally_text("ggpairs allows you\nto put in your\nown plot.\nLike that one.\n <---"), 1, 3)
plotMatrix


# 
plotMatrix2 <- ggpairs(iris[,5:4], upper = list(conditional = "denstrip"))
plotMatrix2 <- putPlot(plotMatrix2, ggally_text("ggpairs allows you\nto retrieve a plot.\nWe will grab this one,\n-->\nwith the legend\nand axis labels!"), 1, 1)
plotMatrix2
getPlot(plotMatrix2, 1, 2) -> plot
plot
plotNew <- plot + scale_fill_gradient(low = "grey80", high = "black")
plotNew
plotMatrix2 <- putPlot(plotMatrix2, plotNew, 1, 2)
plotMatrix2


plotWithBigLabels <- getPlot(diamondMatrix, 2, 1) 
plotWithBigLabels <- plotWithBigLabels + 
opts(axis.text.y = 
theme_text(
angle = 90, 
vjust = 0, 
colour = "grey50",
size = 4
)
)
diamondMatrix <- putPlot(
diamondMatrix,
plotWithBigLabels,
2,
1
)
plotWithBigLabels2 <- getPlot(diamondMatrix, 3, 2) 
plotWithBigLabels2 <- plotWithBigLabels2 + 
opts(axis.text.x = 
theme_text(
vjust = 1, 
hjust = 0, 
colour = "grey50",
size = 4
)
)
diamondMatrix <- putPlot(
diamondMatrix,
plotWithBigLabels2,
3,
2
)
diamondMatrix # now with much smaller labels}
