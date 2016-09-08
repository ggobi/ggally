
#' ggpairs - A ggplot2 Matrix
#'
#' Make a generic matrix of ggplot2 plots
#' @param plots list of plots to be put into matrix
#' @param nrow,ncol number of rows and columns
#' @param xAxisLabels,yAxisLabels strip titles for the x and y axis respectively. Set to \code{NULL} to not be displayed
#' @param title,xlab,ylab title, x label, and y label for the graph. Set to \code{NULL} to not be displayed
#' @param showStrips boolean to determine if each plot's strips should be displayed. \code{NULL} will default to the top and right side plots only. \code{TRUE} or \code{FALSE} will turn all strips on or off respectively.
#' @param showAxisPlotLabels,showXAxisPlotLabels,showYAxisPlotLabels booleans that determine if the plots axis labels are printed on the X (bottom) or Y (left) part of the plot matrix. If \code{showAxisPlotLabels} is set, both \code{showXAxisPlotLabels} and \code{showYAxisPlotLabels} will be set to the given value.
#' @param byrow boolean that determines whether the plots should be ordered by row or by column
#' @param data data set using. This is the data to be used in place of 'ggally_data' if the plot is a string to be evaluated at print time
#' @param gg ggplot2 theme objects to be applied to every plot
#' @template ggmatrix-legend-param
#' @param legends deprecated
#' @keywords hplot
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @export
#' @examples
#' # small function to display plots only if it's interactive
#' p_ <- GGally::print_if_interactive
#'
#' plotList <- list()
#' for (i in 1:6) {
#'   plotList[[i]] <- ggally_text(paste("Plot #", i, sep = ""))
#' }
#' pm <- ggmatrix(
#'   plotList,
#'   2, 3,
#'   c("A", "B", "C"),
#'   c("D", "E"),
#'   byrow = TRUE
#' )
#' p_(pm)
#'
#' pm <- ggmatrix(
#'   plotList,
#'   2, 3,
#'   xAxisLabels = c("A", "B", "C"),
#'   yAxisLabels = NULL,
#'   byrow = FALSE,
#'   showXAxisPlotLabels = FALSE
#' )
#' p_(pm)

ggmatrix <- function(
  plots,
  nrow,
  ncol,
  xAxisLabels = NULL,
  yAxisLabels = NULL,
  title = NULL,
  xlab = NULL,
  ylab = NULL,
  byrow = TRUE,
  showStrips = NULL,
  showAxisPlotLabels = TRUE,
  showXAxisPlotLabels = TRUE,
  showYAxisPlotLabels = TRUE,
  data = NULL,
  gg = NULL,
  legend = NULL
) {

  if (!is.list(plots)) {
    stop("'plots' must be a list()")
  }
  check_nrow_ncol(nrow, "nrow")
  check_nrow_ncol(ncol, "ncol")

  if (!missing(showAxisPlotLabels)) {
    showXAxisPlotLabels <- showAxisPlotLabels
    showYAxisPlotLabels <- showAxisPlotLabels
  }

  plotMatrix <- list(
    data = data,
    plots = plots,
    title = title,
    xlab = xlab,
    ylab = ylab,
    showStrips = showStrips,
    xAxisLabels = xAxisLabels,
    yAxisLabels = yAxisLabels,
    showXAxisPlotLabels = showXAxisPlotLabels,
    showYAxisPlotLabels = showYAxisPlotLabels,
    legend = legend,
    gg = gg,
    nrow = nrow,
    ncol = ncol,
    byrow = byrow
  )

  attributes(plotMatrix)$class <- c("gg", "ggmatrix")

  plotMatrix
}


check_nrow_ncol <- function(x, title) {
  if (!is.numeric(x)) {
    stop(paste("'", title, "' must be a numeric value", sep = ""))
  }
  if (length(x) != 1) {
    stop(paste("'", title, "' must be a single numeric value", sep = ""))
  }

}
