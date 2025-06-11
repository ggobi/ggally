#' \code{\link{ggmatrix}} default progress bar
#'
#' @param format,clear,show_after,... parameters supplied directly to \code{progress::\link[progress]{progress_bar}$new()}
#' @return function that accepts a plot matrix as the first argument and \code{...} for future expansion. Internally, the plot matrix is used to determine the total number of plots for the progress bar.
#' @export
#' @examples
#' p_ <- GGally::print_if_interactive
#'
#' pm <- ggpairs(iris, 1:2, progress = ggmatrix_progress())
#' p_(pm)
#'
#' # does not clear after finishing
#' pm <- ggpairs(iris, 1:2, progress = ggmatrix_progress(clear = FALSE))
#' p_(pm)
ggmatrix_progress <- function(
    format = " plot: [:plot_i, :plot_j] [:bar]:percent est::eta ",
    clear = TRUE,
    show_after = 0,
    ...) {
  ret <- function(pm, ...) {
    progress::progress_bar$new(
      format = format,
      clear = clear,
      show_after = show_after,
      total = pm$ncol * pm$nrow,
      ...
    )
  }

  ret
}


as_ggmatrix_progress <- function(x, total, ...) {
  if (isFALSE(x)) {
    return(FALSE)
  }
  if (isTRUE(x)) {
    return(ggmatrix_progress(...))
  }
  if (is.null(x)) {
    shouldDisplay <- interactive() && total > 15
    if (!shouldDisplay) {
      return(FALSE)
    } else {
      return(ggmatrix_progress(...))
    }
  }
  if (is.function(x)) {
    return(x)
  }

  stop(
    "as_ggmatrix_progress only knows how to handle TRUE, FALSE, NULL, or a function.",
    "  If a function, it must return a new progress_bar"
  )
}

isFALSE <- function(x) {
  identical(FALSE, x)
}
