\name{ggram}
\alias{ggram}
\title{Plot n-gram frequencies}
\usage{
  ggram(phrases, ignore_case = FALSE, geom = "line",
    ignore.case = ignore_case, geom_options = list(),
    google_theme = FALSE, ...)
}
\arguments{
  \item{phrases}{vector of phrases}

  \item{ignore_case}{if \code{TRUE} then the frequencies
  are case insensitive.  Default is \code{FALSE}.}

  \item{ignore.case}{alternative syntax for
  \code{ignore_case}. Only used if \code{ignore_case} is
  not specified.}

  \item{google_theme}{use a Google Ngram-style plot theme.}

  \item{geom}{the ggplot2 geom used to plot the data;
  defaults to "line"}

  \item{geom_options}{list of additional parameters passed
  to the ggplot2 geom.}

  \item{...}{additional parameters passed to \code{ngram}}
}
\description{
  \code{ggram} downloads data from the Google Ngram Viewer
  website and plots it in \code{ggplot2} style.
}
\details{
  Google generated two datasets drawn from digitised books
  in the Google books collection. One was generated in July
  2009, the second in July 2012.  Google will update these
  datasets as book scanning continues.
}
\examples{
ggram(c("hacker", "programmer"), year_start = 1950)

# Changing the geom.
ggram(c("cancer", "fumer", "cigarette"),
      year_start = 1900,
      corpus = "fre_2012",
      geom = "step")

# Passing more options.
require(ggplot2)
ggram(c("cancer", "smoking", "tobacco"),
      year_start = 1900,
      corpus = "eng_fiction_2012",
      geom = "point",
      geom_options = list(alpha = .5)) +
  stat_smooth(method="loess", se = FALSE)

# Setting the layers manually.
ggram(c("cancer", "smoking", "tobacco"),
      year_start = 1900,
      corpus = "eng_fiction_2012",
      geom = NULL) +
  stat_smooth(method="loess", se=FALSE, span = 0.3)

# Setting the legend placement on a long query.
# Example posted by Ben Zimmer at Language Log.
x = c("(The United States is + The United States has) / The United States",
      "(The United States are + The United States have) / The United States")
ggram(x, year_start = 1800) +
  theme(legend.direction = "vertical", legend.position = "bottom")
}

