\name{ggpairs}
\alias{ggpairs}
\title{ggpairs - A GGplot2 Matrix...}
\usage{ggpairs(data, columns=1:ncol(data), title="", upper=list(),
    lower=list(), diag=list(), params = NULL, ..., verbose=FALSE,
    axisLabels="internal", removeTicks=TRUE)}
\description{ggpairs - A GGplot2 Matrix
Make a matrix of plots with a given data set}
\details{upper and lower are lists that may contain the variables 'continuous', 
'combo' and 'discrete'. Each element of the list is a string implementing
the following options: continuous = exactly one of ('points', 'smooth', 
'density', 'cor', 'blank'); combo = exactly one of ('box', 'dot', 
'facethist', 'facetdensity', 'denstrip', 'blank'); discrete = exactly one 
of ('facetbar','ratio', 'blank').

diag is a list that may only contain the variables 'continuous' and 'discrete'. 
Each element of the diag list is a string implmenting the following options: 
continuous = exactly one of ('density', 'bar', 'blank'); discrete = exactly one
of ('bar', 'blank').

If a list option it will be set to the function default.  If 'blank' is ever 
chosen as an option, then ggpairs will produce a blank plot, as if nothing was 
printed there.}
\keyword{hplot}
\author{Barret Schloerke \email{schloerke@gmail.com}, Jason Crowley \email{crowley.jason.s@gmail.com}, Di Cook \email{dicook@iastate.edu}, Heike Hofmann \email{hofmann@iastate.edu}, Hadley Wickham \email{h.wickham@gmail.com}}
\value{ggpair object that if called, will print}
\arguments{\item{data}{data set using.  Can have both numerical and categorical data.}
\item{columns}{which columns are used to make plots.  Defaults to all columns.}
\item{title}{title for the graph}
\item{upper}{see Details}
\item{lower}{see Details}
\item{diag}{see Details}
\item{params}{vector of parameters to be applied to geoms.  Each value must have a corresponding name, such as \code{c(binwidth = 0.1)}.}
\item{...}{other parameters being supplied to geom's aes, such as color}
\item{verbose}{boolean to determine the printing of "Plot #1, Plot #2...."}
\item{axisLabels}{either "internal" for labels in the diagonal plots, or "none" for no axis labels}
\item{removeTicks}{boolean to determine if the ticks/labels are removed from the border areas}}
\examples{# plotting is reduced to the first couple of examples.  
# Feel free to print the ggpair objects created in the examples

#ggpairs(iris)
#ggpairs(iris, upper = "blank")
ggpairs(iris[,3:5])

# Custom Example
ggpairs(
iris[,3:5], 
upper = list(continuous = "density", combo = "box"), 
lower = list(continuous = "points", combo = "dot"), 
diag = list(continuous = "bar", discrete = "bar")
)


# Use sample of the diamonds data
diamonds.samp <- diamonds[sample(1:dim(diamonds)[1],200),]

# Custom Example
diamondMatrix <- ggpairs(	
diamonds.samp[,1:3], 	
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
#bad_plots


# Custom Examples
custom_car <- ggpairs(mtcars[,c("mpg","wt","cyl")], upper = "blank", title = "Custom Example")
# ggplot example taken from example(geom_text)
plot <- ggplot(mtcars, aes(x=wt, y=mpg, label=rownames(mtcars)))
plot <- plot + geom_text(aes(colour=factor(cyl)), size = 3) + scale_colour_discrete(l=40)
custom_car <- putPlot(custom_car, plot, 1, 2)
custom_car <- putPlot(custom_car, ggally_text("ggpairs allows you\nto put in your\nown plot.\nLike that one.\n <---"), 1, 3)
#custom_car


# Custom plot with different scale fill gradient
custom_fill <- ggpairs(iris[,5:4], upper = list(combo = "denstrip"))
custom_fill <- putPlot(
custom_fill, 
ggally_text("ggpairs allows you\nto retrieve a plot.\nWe will grab this one,\n-->\nwith the legend\nand axis labels!"),
1, 1)
#custom_fill
plot <- getPlot(custom_fill, 1, 2)
#plot
plotNew <- plot + scale_fill_gradient(low = "grey80", high = "black")
#plotNew
custom_fill <- putPlot(custom_fill, plotNew, 1, 2)
#custom_fill


special_diamond <- ggpairs(
diamonds.samp,
columns = 8:10, 
upper = list(continuous = "points",aes_string = aes_string(color = "clarity")), 
lower = list(continuous = "points",aes_string = aes_string(color = "cut")), 
diag = "blank", 
title = "Diamonds"
)
#special_diamond


## prints
#   data =    mtcars
#   columns = c(1,3,4) # (mpg, disp, hp)
#   upper =   contains scatter plots with the shape defined by the cyl and size constant at 5
#   lower =   contains scatter plots with the size defined by the cyl
#   diag =    contains 'blank' plots
#   color =   defined by cyl and is applied to botht he upper and lower plots.  It would be applied to diag if it existed
#   title =   "Custom Cars"
#   verbose = FALSE makes the "Plot #1, Plot #2...." not print
carsMatrix <- ggpairs(
mtcars,
columns = c(1,3,4), 
upper = list(continuous = "points",aes_string = aes_string(shape = "cyl", size = 5)), 
lower = list(continuous = "points",aes_string = aes_string(size = "cyl")), 
diag = "blank", 
color = "cyl", 
title = "Custom Cars",
verbose = FALSE
)
#carsMatrix


## Each list is custom to it's own area
#  params are the parameters applied to the geoms.  Parameters can not be considered aesthetics.
iris_with_params <- ggpairs(
iris, 
upper = list(continuous = "density", combo = "dot", aes_string = aes_string(color = "Species")), 
lower = list(continuous = "smooth", combo = "denstrip", aes_string = aes_string(color = "Species", fill = "Species"), params = c(binwidth=0.1)), 
diag = list(continuous = "bar", discrete = "bar", aes_string = aes_string(fill = "Species"), params = c(binwidth = 0.25)),
title = "Complex Iris Data"
)
#iris_with_params

## The same plot matrix with no axis labels
iris_with_params2 <- ggpairs(
iris, 
upper = list(continuous = "density", combo = "dot", aes_string = aes_string(color = "Species")), 
lower = list(continuous = "smooth", combo = "denstrip", aes_string = aes_string(color = "Species", fill = "Species"), params = c(binwidth=0.1)), 
diag = list(continuous = "bar", discrete = "bar", aes_string = aes_string(fill = "Species"), params = c(binwidth = 0.25)),
title = "Complex Iris Data",
axisLabels = "none"
)
#iris_with_params2}
