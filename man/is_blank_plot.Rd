\name{is_blank_plot}
\alias{is_blank_plot}
\title{Is Blank Plot?
Find out if the plot equals a blank plot}
\usage{
is_blank_plot(p)
}
\description{
  Is Blank Plot? Find out if the plot equals a blank plot
}
\examples{
GGally:::is_blank_plot(ggally_blank())
GGally:::is_blank_plot(ggally_points(mtcars, aes_string(x = "disp", 
    y = "hp")))
}
\keyword{internal}
