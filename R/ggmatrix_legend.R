#' Grab the legend and print it as a plot
#'
#' @param p ggplot2 plot object
#' @param x legend object that has been grabbed from a ggplot2 object
#' @param ... ignored
#' @param plotNew boolean to determine if the `grid.newpage()` command and a new blank rectangle should be printed
#' @import ggplot2
#' @export
#' @examples
#' library(ggplot2)
#' histPlot <- qplot(
#'   x = Sepal.Length,
#'   data = iris,
#'   fill = Species,
#'   geom = "histogram",
#'   binwidth = 1/4
#' )
#' (right <- histPlot)
#' (bottom <- histPlot + theme(legend.position = "bottom"))
#' (top <- histPlot + theme(legend.position = "top"))
#' (left <- histPlot + theme(legend.position = "left"))
#'
#' grab_legend(right)
#' grab_legend(bottom)
#' grab_legend(top)
#' grab_legend(left)
grab_legend <- function(p) {
  builtP <- ggplot_build(p)
  pTable <- ggplot_gtable(builtP)

  ret <- get_legend_from_gtable(pTable)
  return(ret)
}

get_legend_from_gtable <- function(pTable) {
  ret <- ggplot2::zeroGrob()
  if (inherits(pTable, "gtable")) {
    if ("guide-box" %in% pTable$layout$name) {
      ret <- gtable_filter(pTable, "guide-box")
    }
  }
  class(ret) <- c("legend_guide_box", class(ret))
  ret
}

#' @importFrom grid grid.newpage grid.draw gpar
#' @importFrom gtable gtable_filter
#' @rdname grab_legend
#' @export
print.legend_guide_box <- function(x, ..., plotNew = FALSE) {
  if (identical(plotNew, TRUE)) {
    grid.newpage()
  }

  grid::grid.rect(gp = grid::gpar(fill = "white", col = "white"))
  grid.draw(x)
}


#' Plot only legend of plot function
#'
#' @param fn this value is passed directly to an empty \code{\link{wrap}} call.  Please see \code{?\link{wrap}} for more details.
#' @return a function that when called with arguments will produce the legend of the plotting function supplied.
#' @export
#' @examples
#' # display regular plot
#' ggally_points(iris, ggplot2::aes(Sepal.Length, Sepal.Width, color = Species))
#'
#' # Make a function that will only print the legend
#' points_legend <- gglegend(ggally_points)
#' points_legend(iris, ggplot2::aes(Sepal.Length, Sepal.Width, color = Species))
#'
#' # produce the sample legend plot, but supply a string that 'wrap' understands
#' same_points_legend <- gglegend("points")
#' identical(
#'   attr(attr(points_legend, "fn"), "original_fn"),
#'   attr(attr(same_points_legend, "fn"), "original_fn")
#' )
#'
#' # Complicated examples
#' custom_legend <- wrap(gglegend("points"), size = 6)
#' custom_legend(iris, ggplot2::aes(Sepal.Length, Sepal.Width, color = Species))
#'
#' # Use within ggpairs
#' pm <- ggpairs(
#'   iris, 1:2,
#'   mapping = ggplot2::aes(color = Species),
#'   upper = list(continuous = gglegend("points"))
#' )
#' # pm
#'
#' # Place a legend in a specific location
#' pm <- ggpairs(iris, 1:2, mapping = ggplot2::aes(color = Species))
#' # Make the legend
#' pm[1,2] <- points_legend(iris, ggplot2::aes(Sepal.Width, Sepal.Length, color = Species))
#' pm
gglegend <- function(fn) {

  # allows users to supply a character just like in ggpairs
  fn <- wrapp(fn, list())
  fn <- attr(fn, "fn")

  ret <- function(...) {
    p <- fn(...)
    grab_legend(p)
  }

  # attach function so people can see what it is
  attr(ret, "fn") <- fn
  attr(ret, "name") <- "gglegend"
  ret
}
