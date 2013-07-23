\name{ggtable}
\alias{ggtable}
\title{ggtable - quick bar plots with ggplot2}
\usage{
  ggtable(x, y = NULL, percent = FALSE, name = "",
    order.x = TRUE, order.y = FALSE, order = order.x,
    label = FALSE, hjust = 2, color = "grey90",
    append = "", verbose = FALSE, palette = NULL,
    horizontal = TRUE, position = "stack",
    legend.position = "right", ...)
}
\arguments{
  \item{x,}{y vectors to plot. If a vector \code{y} is
  provided, the function returns the stacked bars
  corresponding to \code{table(x, y)}.}

  \item{percent}{whether to use percentages instead of raw
  counts. Defaults to \code{FALSE}.}

  \item{name}{when a vector \code{y} is provided, the title
  of its colour legend.}

  \item{order}{whether to sort the levels of vector
  \code{x} by decreasing order of frequency. Defaults to
  \code{TRUE}.}

  \item{order.x}{the order in which to plot the levels of
  vector \code{x}. Overrules \code{order}.}

  \item{order.y}{the order in which to plot the levels of
  vector \code{y}.}

  \item{label}{whether to add text labels to the bars.
  Defaults to \code{FALSE}.}

  \item{hjust}{horizontal adjustment for the bar labels.
  Defaults to \code{2}.}

  \item{color}{color for the bar labels. Defaults to
  \code{grey90} (quasi-white).}

  \item{append}{a single character string to append to the
  bar labels.}

  \item{verbose}{whether to return messages on the plot
  construction. Defaults to \code{FALSE}.}

  \item{palette}{when a vector \code{y} is provided, the
  name of a ColorBrewer palette to use to color the bars.}

  \item{position}{when a vector \code{y} is provided, the
  position of the bars split by \code{y}. Defaults to
  \code{"stack"}.}

  \item{legend.position}{location of the legend for bar
  colors and weights. Accepts all positions supported by
  ggplot2 themes. Defaults to \code{"right"}.}

  \item{...}{other arguments supplied to \code{geom_text}
  for the bar labels. Arguments pertaining to the title or
  other items can be achieved through ggplot2 methods, at
  the exception of facet methods.}
}
\description{
  Function to plot fequencies and crosstabulations as
  horizontal bars or stacked bars, using raw counts or
  percentages. Please visit
  \link{http://github.com/briatte/ggtable} for the latest
  development and descriptions about ggtable.
}
\examples{
## Examples using the questionr package.
require(devtools)
install_github("questionr", "briatte")
require(questionr)
data(hdv2003)
## One-way tables
freq(x <- recode.na(hdv2003$relig, "NSP ou NVPR|Rejet"))
ggtable(x)
ggtable(x, order = FALSE, label = TRUE)
ggtable(x, percent = TRUE, label = TRUE, append = " \%")
## Two-way tables
freq(y <- hdv2003$sexe)
ggtable(x, y)
ggtable(x, y, percent = TRUE)
ggtable(y, x, order.y = c("Homme", "Femme"))
## Using options.
ggtable(y, x, order = c("Homme", "Femme"), horizontal = FALSE,
  percent = TRUE, position = "dodge", legend.position = "bottom")
Cleaner theme.
ggtable(hdv2003$nivetud, x, percent = TRUE, palette = "RdGy") +
  theme_bw(16) +
  theme(legend.position = "top", axis.ticks = element_blank())
}
\author{
  François Briatte \email{f.briatte@ed.ac.uk}
}
\seealso{
  \code{\link[likert]{plot.likert}} in the
  \link[likert]{likert} package
}

