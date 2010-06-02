\name{.addAndOverwriteAes}
\alias{.addAndOverwriteAes}
\title{Add new aes...}
\usage{.addAndOverwriteAes(current, new)}
\description{Add new aes
Add new aesthetics to a previous aes}
\keyword{internal}
\author{Barret Schloerke \email{bigbear@iastate.edu}}
\value{aes_string output}
\examples{diamondMatrix <- ggpairs(	
diamonds,
columns = 1:3, 	
upper = list(points = "scatterplot", aes_string = aes_string(color = "clarity")), 	
lower = list(points = "scatterplot", aes_string = aes_string(color = "cut")), 	
diag = "blank", 
color = "color", 
title = "Diamonds"
)}
