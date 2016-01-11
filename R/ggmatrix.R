
#' ggpairs - A ggplot2 Matrix
#'
#' Make a generic matrix of ggplot2 plots
#' @param plots list of plots to be put into matrix
#' @param nrow,ncol number of rows and columns
#' @param xAxisLabels,yAxisLabels,title labels for plot. Set the variable to \code{NULL} to not be displayed
#' @param showStrips boolean to determine if each plot's strips should be displayed. \code{NULL} will default to the top and right side plots only. \code{TRUE} or \code{FALSE} will turn all strips on or off respectively.
#' @param showAxisPlotLabels,showXAxisPlotLabels,showYAxisPlotLabels booleans that determine if the plots axis labels are printed on the X (bottom) or Y (left) part of the plot matrix. If \code{showAxisPlotLabels} is set, both \code{showXAxisPlotLabels} and \code{showYAxisPlotLabels} will be set to the given value.
#' @param byrow boolean that determines whether the plots should be ordered by row or by column
#' @param verbose boolean to determine the printing of "Plot #1, Plot #2...."
#' @param data data set using. This is the data to be used in place of 'ggally_data' if the plot is a string to be evaluated at print time
#' @param gg ggplot2 theme objects to be applied to every plot
#' @param legends boolean to determine the printing of the legend in each plot. Not recommended.
#' @keywords hplot
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @export
#' @examples
#' plotList <- list()
#' for (i in 1:6) {
#'   plotList[[i]] <- ggally_text(paste("Plot #", i, sep = ""))
#' }
#' a <- ggmatrix(
#'   plotList,
#'   2, 3,
#'   c("A", "B", "C"),
#'   c("D", "E"),
#'   byrow = TRUE
#' )
#' #a
#'
#' a <- ggmatrix(
#'   plotList,
#'   2, 3,
#'   xAxisLabels = c("A", "B", "C"),
#'   yAxisLabels = NULL,
#'   byrow = FALSE,
#'   showXAxisPlotLabels = FALSE
#' )
#' #a

ggmatrix <- function(
  plots,
  nrow,
  ncol,
  xAxisLabels = NULL,
  yAxisLabels = NULL,
  title = NULL,
  byrow = TRUE,
  showStrips = NULL,
  showAxisPlotLabels = TRUE,
  showXAxisPlotLabels = TRUE,
  showYAxisPlotLabels = TRUE,
  verbose = FALSE,
  data = NULL,
  gg = NULL,
  legends = FALSE
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
    verbose = verbose,
    printInfo = verbose,
    showStrips = showStrips,
    xAxisLabels = xAxisLabels,
    yAxisLabels = yAxisLabels,
    showXAxisPlotLabels = showXAxisPlotLabels,
    showYAxisPlotLabels = showYAxisPlotLabels,
    legends = legends,
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
