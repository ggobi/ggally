\name{ggcorr}
\alias{ggcorr}
\title{ggcorr - Plot a correlation matrix with ggplot2}
\usage{
  ggcorr(data, method = "pairwise", palette = "RdYlGn",
    ...)
}
\arguments{
  \item{data}{a data matrix. Should contain numerical
  (continuous) data.}

  \item{method}{a character string giving a method for
  computing covariances in the presence of missing values.
  This must be (an abbreviation of) one of the strings
  \code{"everything"}, \code{"all.obs"},
  \code{"complete.obs"}, \code{"na.or.complete"}, or
  \code{"pairwise.complete.obs"}. Defaults to
  \code{"pairwise"}.}

  \item{palette}{a ColorBrewer palette to be used for
  correlation coefficients. Defaults to \code{"RdYlGn"}.}

  \item{...}{other arguments supplied to geom_text for the
  diagonal labels.  Arguments pertaining to the title or
  other items can be achieved through ggplot2 methods.}
}
\description{
  Function for making a correlation plot starting from a
  data matrix, using ggplot2. The function is directly
  inspired by Tian Zheng and Yu-Sung Su's arm:corrplot
  function.
}
\examples{
# Basketball statistics provided by Nathan Yau at Flowing Data.
nba <- read.csv("http://datasets.flowingdata.com/ppg2008.csv")
ggcorr(nba[-1])
ggcorr(
  nba[-1],
  size = 3,
  hjust = 0.75,
  angle = -45,
  palette = "PuOr" # colorblind safe, photocopy-able
) + labs(title = "Points Per Game")
}
\author{
  Francois Briatte \email{f.briatte@ed.ac.uk}
}
\seealso{
  \code{\link{cor}} and \code{\link[arm]{corrplot}}
}

