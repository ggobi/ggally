\name{ggpairs}
\alias{ggpairs}
\title{ggpairs - A GGplot2 Matrix...}
\usage{ggpairs(data, columns=1:ncol(data), upper=list(continuous = "cor",
    combo = "facethist", discrete = "ratio", aes_string = NULL),
    lower=list(continuous = "points", combo = "box", discrete =
    "ratio", aes_string = NULL), diag=list(continuous = "density",
    discrete = "bar", aes_string = NULL), title="", ..., .verbose=TRUE)}
\description{ggpairs - A GGplot2 Matrix
Make a matrix of plots with a given data set}
\details{upper and lower are lists that may contain the variables 'continuous', 
'combo' and 'discrete'. Each element of the list is a string implementing
the following options: continuous = exactly one of ('points', 'smooth', 
'density', 'cor', 'blank'); combo = exactly one of ('box', 'dot', 
'facethist', 'facetdensity', 'denstrip', 'blank'); discrete = exactly one 
of ('ratio', 'blank').

diag is a list that may only contain the variables 'continuous' and 'discrete'. 
Each element of the diag list is a string implmenting the following options: 
continuous = exactly one of ('density', 'bar', 'blank'); discrete = exactly one
of ('bar', 'blank').

If a list option it will be set to the function default.  If 'blank' is ever 
chosen as an option, then ggpairs will produce a blank plot, as if nothing was 
printed there.}
\keyword{hplot}
\author{Barret Schloerke \email{bigbear@iastate.edu}, Di Cook \email{dicook@iastate.edu}, Heike Hofmann \email{hofmann@iastate.edu}, Hadley Wickham \email{h.wickham@gmail.com}}
\value{ggpair object that if called, will reprint}
\arguments{\item{data}{data set using.  Can have both numerical and categorical data.}
\item{columns}{which columns are used to make plots.  Defaults to all columns.}
\item{title}{title for the graph}
\item{upper}{see Details}
\item{lower}{see Details}
\item{diag}{see Details}
\item{...}{other parameters being supplied to geom's aes, such as color}
\item{.verbose}{boolean to determine the printing of "Plot #1, Plot #2...."}}
\examples{# plotting is reduced to the first couple of examples.  
# Feel free to print the ggpair objects created in the examples

#ggpairs(iris)
ggpairs(iris[,3:5])

# Custom Example
ggpairs(
iris[,3:5], 
upper = list(continuous = "density", combo = "box"), 
lower = list(continuous = "points", combo = "dot"), 
diag = list(continuous = "bar", discrete = "bar")
)

# Custom Example
#ggpairs(
#	iris[,3:5],
#	upper = list(continuous = "density", combo = "facetdensity"),
#	lower = "blank",
#	diag = list(continuous = "bar", discrete = "blank")
#)


# Custom Example
diamondMatrix <- ggpairs(	
diamonds[,1:3], 	
upper = list(continuous = "density", combo = "box"), 	
lower = list(continuous = "points", combo = "dot"), 	
diag = list(continuous = "bar", discrete = "bar"), 
color = "cut", 
title = "Diamonds"
)
#diamondMatrix


# Will plot four "Incorrect Plots"	
bad_plots <- ggpairs(
iris[,3:4],
upper = list(continuous = "wrongType1", combo = "wrongType2"),
lower = list(continuous = "IDK1", combo = "IDK2", discrete = "mosaic"),
diag = list(continuous = "points", discrete = "box")
)


# Custom Examples
plotMatrix <- ggpairs(mtcars[,c("mpg","wt","cyl")], upper = "blank", title = "Custom Example")
# ggplot example taken from example(geom_text)
plot <- ggplot(mtcars, aes(x=wt, y=mpg, label=rownames(mtcars)))
plot <- plot + geom_text(aes(colour=factor(cyl)), size = 3) + scale_colour_discrete(l=40)
plotMatrix <- putPlot(plotMatrix, plot, 1, 2)
plotMatrix <- putPlot(plotMatrix, ggally_text("ggpairs allows you\nto put in your\nown plot.\nLike that one.\n <---"), 1, 3)
#plotMatrix


# Custom plot with different scale fill gradient
plotMatrix2 <- ggpairs(iris[,5:4], upper = list(combo = "denstrip"))
plotMatrix2 <- putPlot(
plotMatrix2, 
ggally_text("ggpairs allows you\nto retrieve a plot.\nWe will grab this one,\n-->\nwith the legend\nand axis labels!"),
1, 1)
#plotMatrix2
getPlot(plotMatrix2, 1, 2) -> plot
#plot
plotNew <- plot + scale_fill_gradient(low = "grey80", high = "black")
#plotNew
plotMatrix2 <- putPlot(plotMatrix2, plotNew, 1, 2)
#plotMatrix2


#Sequence to show how to change label size
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
#diamondMatrix # now with much smaller labels



customDiamond <- ggpairs(
diamonds,
columns = 8:10, 
upper = list(continuous = "points",aes_string = aes_string(color = "clarity")), 
lower = list(continuous = "points",aes_string = aes_string(color = "cut")), 
diag = "blank", 
title = "Diamonds",
.printInfo=FALSE
)
#customDiamond



## prints
#   data = mtcars
#   columns = c(1,3,4) # (mpg, disp, hp)
#   upper = contains scatter plots with the shape defined by the cyl and size constant at 5
#   lower = contains scatter plots with the size defined by the cyl
#   diag = contains 'blank' plots
#   color = defined by cyl and is applied to botht he upper and lower plots.  It would be applied to diag if it existed
#   title = "Custom Cars"
#   .verbose = FALSE makes the "Plot #1, Plot #2...." not print
carsMatrix <- ggpairs(
mtcars,
columns = c(1,3,4), 
upper = list(continuous = "points",aes_string = aes_string(shape = "cyl", size = 5)), 
lower = list(continuous = "points",aes_string = aes_string(size = "cyl")), 
diag = "blank", 
color = "cyl", 
title = "Custom Cars",
.verbose = FALSE
)
#carsMatrix}
