
#' ggpairs - A ggplot2 Matrix
#'
#' Make a matrix of ggplot2 plots
#' @param plots list of plots to be put into matrix
#' @param nrow,ncol number of rows and columns
#' @param xAxisLabels,yAxisLabels,title labels for plot
#' @param axisLabels either "show" to display axisLabels, "internal" for labels in the diagonal plots, or "none" for no axis labels
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
#'   c("A", "B", "C"),
#'   c("D", "E"),
#'   byrow = FALSE
#' )
#' #a

ggmatrix <- function(
  plots,
  nrow,
  ncol,
  xAxisLabels = rep("", length(plots)),
  yAxisLabels = rep("", length(plots)),
  axisLabels = "show",
  title = NULL,
  byrow = TRUE,
  verbose = FALSE,
  data = NULL,
  gg = NULL,
  legends = FALSE
) {

  if(!is.list(plots)) {
    stop("'plots' must be a list()")
  }
  check_nrow_ncol(nrow, "nrow")
  check_nrow_ncol(ncol, "ncol")

  if (! byrow) {
    plots <- plots[c(t(matrix(1:(nrow * ncol), byrow = FALSE, ncol = ncol)))]
  }

  plotMatrix <- list(
    data = data,
    plots = plots,
    title = title,
    verbose = verbose,
    printInfo = verbose,
    axisLabels = axisLabels,
    xAxisLabels = xAxisLabels,
    yAxisLabels = yAxisLabels,
    legends = legends,
    gg = gg,
    nrow = nrow,
    ncol = ncol
  )

  attributes(plotMatrix)$class <- c("gg", "ggmatrix")

  plotMatrix
}


check_nrow_ncol <- function(x, title) {
  if(!is.numeric(x)) {
    stop(paste("'", title, "' must be a numeric value", sep = ""))
  }
  if (length(x) != 1) {
    stop(paste("'", title, "' must be a single numeric value", sep = ""))
  }

}
