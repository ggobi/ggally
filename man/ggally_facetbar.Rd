\name{ggally_facetbar}
\alias{ggally_facetbar}
\title{Plots the Bar Plots Faceted by Conditional Variable...}
\usage{ggally_facetbar(data, mapping, ...)}
\description{Plots the Bar Plots Faceted by Conditional Variable}
\details{X variables are plotted using \code{geom_bar} and faceted by the Y variable.}
\author{Barret Schloerke \email{schloerke@gmail.com}}
\keyword{hplot}
\arguments{\item{data}{data set using}
\item{mapping}{aesthetics being used}
\item{...}{other arguements are sent to geom_bar}}
\examples{ggally_facetbar(tips, aes(x = sex, y = smoker, fill = time))
ggally_facetbar(tips, aes(x = smoker, y = sex, fill = time))}
