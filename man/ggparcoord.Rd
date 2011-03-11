\name{ggparcoord}
\alias{ggparcoord}
\title{ggparcoord - A ggplot2 Parallel Coordinate Plot...}
\usage{ggparcoord(data, columns, groupColumn, scale="std", scaleSummary="mean", centerObsID=1,
    missing="exclude", order=columns, showPoints=FALSE, alphaLines=1, boxplot=FALSE,
    shadeBox, mapping, title="")}
\description{ggparcoord - A ggplot2 Parallel Coordinate Plot
; A function for plotting static parallel coordinate plots, utilizing
the \code{ggplot2} graphics package.}
\details{\code{scale} is a character string that denotes how to scale the variables
in the parallel coordinate plot. Options:
\itemize{
\item{\code{std}}{: univariately, subtract mean and divide by standard deviation}
\item{\code{robust}}{: univariately, subtract median and divide by median absolute deviation}
\item{\code{uniminmax}}{: univariately, scale so the minimum of the variable is zero, and the maximum is one}
\item{\code{globalminmax}}{: scale so the global minimum is zero, and the global maximum is one}
\item{\code{center}}{: use \code{uniminmax} to standardize vertical height, then
center each variable at a value specified by the \code{scaleSummary} param}
\item{\code{centerObs}}{: use \code{uniminmax} to standardize vertical height, then
center each variable at the value of the observation specified by the \code{centerObsID} param}
}

\code{missing} is a character string that denotes how to handle missing
missing values. Options:
\itemize{
\item{\code{exclude}}{: remove all cases with missing values}
\item{\code{mean}}{: set missing values to the mean of the variable}
\item{\code{median}}{: set missing values to the median of the variable}
\item{\code{min10}}{: set missing values to 10\% below the minimum of the variable}
\item{\code{random}}{: set missing values to value of randomly chosen observation
on that variable}
}

\code{order} is either a vector of indices or a character string that denotes how to 
order the axes (variables) of the parallel coordinate plot. Options:
\itemize{
\item{\code{(default)}}{: order by the vector denoted by \code{columns}}
\item{\code{(given vector)}}{: order by the vector specified}
\item{\code{Outlying}}{: order by the scagnostic measure, Outlying, as calculated
by the package \code{scagnostics}. Other scagnostic measures available to order
by are \code{Skewed, Clumpy, Sparse, Striated, Convex, Skinny, Stringy,} and
\code{Monotonic}.}
}}
\author{Jason Crowley \email{crowley.jason.s@gmail.com}, Barret Schloerke \email{schloerke@gmail.com}, Di Cook \email{dicook@iastate.edu}, Heike Hofmann \email{hofmann@iastate.edu}, Hadley Wickham \email{h.wickham@gmail.com}}
\value{ggplot object that if called, will print}
\arguments{\item{data}{the dataset to plot}
\item{columns}{a vector of variables (either names or indices) to be axes in the plot}
\item{groupColumn}{a single variable to group (color) by}
\item{scale}{method used to scale the variables (see Details)}
\item{scaleSummary}{if scale=="center", summary statistic to univariately
center each variable by}
\item{centerObsID}{if scale=="centerObs", row number of case plot should
univariately be centered on}
\item{missing}{method used to handle missing values (see Details)}
\item{order}{method used to order the axes (see Details)}
\item{showPoints}{logical operator indicating whether points should be
plotted or not}
\item{alphaLines}{value of alpha scaler for the lines of the parcoord plot}
\item{boxplot}{logical operator indicating whether or not boxplots should
underlay the distribution of each variable}
\item{shadeBox}{color of underlaying box which extends from the min to the
max for each variable (no box is plotted if shadeBox == NULL)}
\item{mapping}{aes string to pass to ggplot object}
\item{title}{character string denoting the title of the plot}}
\examples{# basic parallel coordinate plot, using defaults
ggparcoord(iris,1:4,5)

# underlay univariate boxplots, add title, use uniminmax scaling
ggparcoord(iris,1:4,5,scale="uniminmax",boxplot=TRUE,title="Parallel Coord. Plot of Iris Data")

# utilize ggplot2 aes to switch to dashed lines
ggparcoord(iris,1:4,5,boxplot=TRUE,title="Parallel Coord. Plot of Iris Data",
mapping=aes_string(lty=2))}
