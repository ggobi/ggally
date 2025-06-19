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
ggmatrix <- new_class(
  "ggmatrix",
  properties = list(
    data = new_union(class_data.frame, NULL),
    plots = class_list,
    title = class_any,
    xlab = class_any,
    ylab = class_any,
    showStrips = new_union(class_logical, NULL),
    xAxisLabels = class_any,
    yAxisLabels = class_any,
    showXAxisPlotLabels = class_logical,
    showYAxisPlotLabels = class_logical,
    labeller = class_any,
    switch = new_union(class_character, NULL),
    # xProportions = new_union(class_numeric, class_grid_unit, NULL),
    xProportions = class_any,
    yProportions = class_any,
    progress = new_union(
      new_S3_class("progress_bar"),
      class_function,
      class_logical,
      NULL
    ),
    legend = class_any,
    gg = new_union(class_list, NULL),
    nrow = class_numeric,
    ncol = class_numeric,
    byrow = class_logical,
    meta = class_list
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
      new_object(
        S7_object(),
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
  if (!prop_exists(x, i) && prop_exists(x, "meta")) {
    # This is a trick to bridge a gap between S3 and S7. We're allowing
    # for arbitrary fields by reading/writing to the 'meta' field when the
    # index does not point to an actual property.
    # The proper way to go about this is to implement new fields as properties
    # of a ggplot subclass.
    prop(x, "meta")[[i]]
  } else {
    `[[`(props(x), i)
  }
}

#' @export
`$<-.GGally::ggmatrix` <- function(x, i, value) {
  if (!prop_exists(x, i) && prop_exists(x, "meta")) {
    # See explanation in `$.GGally::ggmatrix`
    prop(x, "meta")[[i]] <- value
  } else {
    props(x) <- `[[<-`(props(x), i, value)
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


# https://github.com/RConsortium/S7/issues/529
utils::globalVariables("properties")

method(convert, list(from = ggmatrix, to = class_list)) <-
  function(from, to) {
    vals <- props(from)
    meta <- vals$meta
    # Remove meta from the list of properties
    vals$meta <- NULL
    # Collect the original values and the user added meta data
    c(vals, meta)
  }

local({
  method(as.list, class_ggplot) <- function(x, ...) {
    convert(x, class_list)
  }
})
