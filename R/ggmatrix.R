#' \pkg{ggplot2} plot matrix
#'
#' Make a generic matrix of \pkg{ggplot2} plots.
#'
#' @section Memory usage:
#' Now that the \code{\link{print.ggmatrix}} method uses a large \pkg{gtable} object, rather than print each plot independently, memory usage may be of concern.  From small tests, memory usage flutters around \code{object.size(data) * 0.3 * length(plots)}.  So, for a 80Mb random noise dataset with 100 plots, about 2.4 Gb of memory needed to print. For the 3.46 Mb diamonds dataset with 100 plots, about 100 Mb of memory was needed to print.  The benefits of using the \pkg{ggplot2} format greatly outweigh the price of about 20% increase in memory usage from the prior ad-hoc print method.
#'
#' @param plots list of plots to be put into matrix
#' @param nrow,ncol number of rows and columns
#' @param xAxisLabels,yAxisLabels strip titles for the x and y axis respectively. Set to \code{NULL} to not be displayed
#' @param title,xlab,ylab title, x label, and y label for the graph. Set to \code{NULL} to not be displayed
#' @param byrow boolean that determines whether the plots should be ordered by row or by column
#' @param showStrips boolean to determine if each plot's strips should be displayed. \code{NULL} will default to the top and right side plots only. \code{TRUE} or \code{FALSE} will turn all strips on or off respectively.
#' @param showAxisPlotLabels,showXAxisPlotLabels,showYAxisPlotLabels booleans that determine if the plots axis labels are printed on the X (bottom) or Y (left) part of the plot matrix. If \code{showAxisPlotLabels} is set, both \code{showXAxisPlotLabels} and \code{showYAxisPlotLabels} will be set to the given value.
#' @template ggmatrix-labeller-param
#' @template ggmatrix-switch-param
#' @param xProportions,yProportions Value to change how much area is given for each plot. Either \code{NULL} (default), numeric value matching respective length, or \code{grid::\link[grid]{unit}} object with matching respective length
#' @template ggmatrix-progress
#' @param data data set using. This is the data to be used in place of 'ggally_data' if the plot is a string to be evaluated at print time
#' @param gg \pkg{ggplot2} theme objects to be applied to every plot
#' @template ggmatrix-legend-param
#' @keywords hplot
#' @author Barret Schloerke
#' @importFrom rlang %||% .data
#' @export
#' @examples
#' # Small function to display plots only if it's interactive
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
ggmatrix <- S7::new_class(
  "ggmatrix",
  properties = list(
    data = S7::new_union(S7::class_data.frame, NULL),
    plots = S7::class_list,
    title = S7::class_any,
    xlab = S7::class_any,
    ylab = S7::class_any,
    showStrips = S7::new_union(S7::class_logical, NULL),
    xAxisLabels = S7::class_any,
    yAxisLabels = S7::class_any,
    showXAxisPlotLabels = S7::class_logical,
    showYAxisPlotLabels = S7::class_logical,
    labeller = S7::class_any,
    switch = S7::new_union(S7::class_character, NULL),
    # xProportions = S7::new_union(S7::class_numeric, S7::class_grid_unit, NULL),
    xProportions = S7::class_any,
    yProportions = S7::class_any,
    progress = S7::new_union(
      S7::new_S3_class("progress_bar"),
      S7::class_function,
      S7::class_logical,
      NULL
    ),
    legend = S7::class_any,
    gg = S7::new_union(S7::class_list, NULL),
    nrow = S7::class_numeric,
    ncol = S7::class_numeric,
    byrow = S7::class_logical,
    meta = S7::class_list
  ),
  constructor = function(
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
    labeller = NULL,
    switch = NULL,
    xProportions = NULL,
    yProportions = NULL,
    progress = NULL,
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

    progress <- as_ggmatrix_progress(progress, nrow * ncol)

    ret <-
      S7::new_object(
        S7::S7_object(),
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
        labeller = labeller,
        switch = switch,
        xProportions = xProportions,
        yProportions = yProportions,
        progress = progress,
        legend = legend,
        gg = gg,
        nrow = nrow,
        ncol = ncol,
        byrow = byrow,
        meta = list()
      )
    # Prefix with ggmatrix class
    class(ret) <- c("ggmatrix", class(ret))
    ret
  }
)

check_nrow_ncol <- function(x, title) {
  if (!is.numeric(x)) {
    stop(paste("'", title, "' must be a numeric value", sep = ""))
  }
  if (length(x) != 1) {
    stop(paste("'", title, "' must be a single numeric value", sep = ""))
  }
}


# ------------------------------------------------------

# The following extractors and subassignment operators are for a smooth
# transition and should be deprecated in the release cycle of choice

#' @export
`$.GGally::ggmatrix` <- function(x, i) {
  if (!S7::prop_exists(x, i) && S7::prop_exists(x, "meta")) {
    # This is a trick to bridge a gap between S3 and S7. We're allowing
    # for arbitrary fields by reading/writing to the 'meta' field when the
    # index does not point to an actual property.
    # The proper way to go about this is to implement new fields as properties
    # of a ggplot subclass.
    S7::prop(x, "meta")[[i]]
  } else {
    `[[`(S7::props(x), i)
  }
}

#' @export
`$<-.GGally::ggmatrix` <- function(x, i, value) {
  if (!S7::prop_exists(x, i) && S7::prop_exists(x, "meta")) {
    # See explanation in `$.GGally::ggmatrix`
    S7::prop(x, "meta")[[i]] <- value
  } else {
    S7::props(x) <- `[[<-`(S7::props(x), i, value)
  }
  x
}

#' @include ggpairs_getput.R
#' @export
`[.GGally::ggmatrix` <- `[.ggmatrix`
#' @export
`[<-.GGally::ggmatrix` <- `[<-.ggmatrix`


#' @export
`[[.GGally::ggmatrix` <- `$.GGally::ggmatrix`

#' @export
`[[<-.GGally::ggmatrix` <- `$<-.GGally::ggmatrix`

#' @importFrom S7 convert
S7::method(convert, list(from = ggmatrix, to = S7::class_list)) <-
  function(from, to) {
    vals <- S7::props(from)
    meta <- vals$meta
    # Remove meta from the list of properties
    vals$meta <- NULL
    # Collect the original values and the user added meta data
    c(vals, meta)
  }

local({
  S7::method(as.list, class_ggplot) <- function(x, ...) {
    convert(x, S7::class_list)
  }
})
