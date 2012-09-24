\name{ggparcoord}
\alias{ggparcoord}
\title{ggparcoord - A ggplot2 Parallel Coordinate Plot
; A function for plotting static parallel coordinate plots, utilizing
the \code{ggplot2} graphics package.}
\usage{
  ggparcoord(data, columns, groupColumn = NULL,
    scale = "std", scaleSummary = "mean", centerObsID = 1,
    missing = "exclude", order = columns,
    showPoints = FALSE, alphaLines = 1, boxplot = FALSE,
    shadeBox = NULL, mapping = NULL, title = "")
}
\arguments{
  \item{data}{the dataset to plot}

  \item{columns}{a vector of variables (either names or
  indices) to be axes in the plot}

  \item{groupColumn}{a single variable to group (color) by}

  \item{scale}{method used to scale the variables (see
  Details)}

  \item{scaleSummary}{if scale=="center", summary statistic
  to univariately center each variable by}

  \item{centerObsID}{if scale=="centerObs", row number of
  case plot should univariately be centered on}

  \item{missing}{method used to handle missing values (see
  Details)}

  \item{order}{method used to order the axes (see Details)}

  \item{showPoints}{logical operator indicating whether
  points should be plotted or not}

  \item{alphaLines}{value of alpha scaler for the lines of
  the parcoord plot}

  \item{boxplot}{logical operator indicating whether or not
  boxplots should underlay the distribution of each
  variable}

  \item{shadeBox}{color of underlaying box which extends
  from the min to the max for each variable (no box is
  plotted if shadeBox == NULL)}

  \item{mapping}{aes string to pass to ggplot object}

  \item{title}{character string denoting the title of the
  plot}
}
\value{
  ggplot object that if called, will print
}
\description{
  \code{scale} is a character string that denotes how to
  scale the variables in the parallel coordinate plot.
  Options: \itemize{ \item{\code{std}}{: univariately,
  subtract mean and divide by standard deviation}
  \item{\code{robust}}{: univariately, subtract median and
  divide by median absolute deviation}
  \item{\code{uniminmax}}{: univariately, scale so the
  minimum of the variable is zero, and the maximum is one}
  \item{\code{globalminmax}}{: no scaling is done; the
  range of the graphs is defined by the global minimum and
  the global maximum} \item{\code{center}}{: use
  \code{uniminmax} to standardize vertical height, then
  center each variable at a value specified by the
  \code{scaleSummary} param} \item{\code{centerObs}}{: use
  \code{uniminmax} to standardize vertical height, then
  center each variable at the value of the observation
  specified by the \code{centerObsID} param} }
}
\details{
  \code{missing} is a character string that denotes how to
  handle missing missing values. Options: \itemize{
  \item{\code{exclude}}{: remove all cases with missing
  values} \item{\code{mean}}{: set missing values to the
  mean of the variable} \item{\code{median}}{: set missing
  values to the median of the variable}
  \item{\code{min10}}{: set missing values to 10\% below
  the minimum of the variable} \item{\code{random}}{: set
  missing values to value of randomly chosen observation on
  that variable} }

  \code{order} is either a vector of indices or a character
  string that denotes how to order the axes (variables) of
  the parallel coordinate plot. Options: \itemize{
  \item{\code{(default)}}{: order by the vector denoted by
  \code{columns}} \item{\code{(given vector)}}{: order by
  the vector specified} \item{\code{anyClass}}{: order
  variables by their separation between any one class and
  the rest (as opposed to their overall variation between
  classes). This is accomplished by calculating the
  F-statistic for each class vs. the rest, for each axis
  variable.  The axis variables are then ordered
  (decreasing) by their maximum of k F-statistics, where k
  is the number of classes.} \item{\code{allClass}}{: order
  variables by their overall F statistic (decreasing) from
  an ANOVA with \code{groupColumn} as the explanatory
  variable (note: it is required to specify a
  \code{groupColumn} with this ordering method). Basically,
  this method orders the variables by their variation
  between classes (most to least).}
  \item{\code{skewness}}{: order variables by their sample
  skewness (most skewed to least skewed)}
  \item{\code{Outlying}}{: order by the scagnostic measure,
  Outlying, as calculated by the package
  \code{scagnostics}. Other scagnostic measures available
  to order by are \code{Skewed, Clumpy, Sparse, Striated,
  Convex, Skinny, Stringy,} and \code{Monotonic}. Note: To
  use these methods of ordering, you must have the
  \code{scagnostics} package loaded.} }
}
\examples{
# use sample of the diamonds data for illustrative purposes
data(diamonds, package="ggplot2")
diamonds.samp <- diamonds[sample(1:dim(diamonds)[1],100),]

# basic parallel coordinate plot, using default settings
ggparcoord(data = diamonds.samp,columns = c(1,5:10))

# this time, color by diamond cut
ggparcoord(data = diamonds.samp,columns = c(1,5:10),groupColumn = 2)

# underlay univariate boxplots, add title, use uniminmax scaling
ggparcoord(data = diamonds.samp,columns = c(1,5:10),groupColumn = 2,
  scale = "uniminmax",boxplot = TRUE,title = "Parallel Coord. Plot of Diamonds Data")

# utilize ggplot2 aes to switch to thicker lines
ggparcoord(data = diamonds.samp,columns = c(1,5:10),groupColumn = 2,
  title="Parallel Coord. Plot of Diamonds Data",mapping = aes(size = 1))

# basic parallel coord plot of the msleep data, using 'random' imputation and
# coloring by diet (can also use variable names in the columns and groupColumn
# arguments)
ggparcoord(data = msleep, columns = 6:11, groupColumn = "vore", missing =
  "random", scale = "uniminmax")

# center each variable by its median, using the default missing value handler,
# 'exclude'
ggparcoord(data = msleep, columns = 6:11, groupColumn = "vore", scale =
  "center", scaleSummary = "median")

# with the iris data, order the axes by overall class (Species) separation using
# the anyClass option
ggparcoord(data = iris, columns = 1:4, groupColumn = 5, order = "anyClass")

# add points to the plot, add a title, and use an alpha scalar to make the lines
# transparent
ggparcoord(data = iris, columns = 1:4, groupColumn = 5, order = "anyClass",
  showPoints = TRUE, title = "Parallel Coordinate Plot for the Iris Data",
  alphaLines = 0.3)
}
\author{
  Jason Crowley \email{crowley.jason.s@gmail.com}, Barret
  Schloerke \email{schloerke@gmail.com}, Di Cook
  \email{dicook@iastate.edu}, Heike Hofmann
  \email{hofmann@iastate.edu}, Hadley Wickham
  \email{h.wickham@gmail.com}
}

