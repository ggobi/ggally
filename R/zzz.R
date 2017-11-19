#' Code Default Options
#'
#' @section Package options:
#'
#' GGally uses the following \code{\link{options}} to configure behaviour:
#'
#' \itemize{
#'   \item \code{GGally.ggmatrix.progress}: default progress behavior for displaying a progress bar while printing a ggmatrix.  \code{NULL} (default) will display a progress bar for an interactive session with more than 15 panels.  \code{TRUE} will always print a progress bar.  \code{FALSE} will always hide the progress bar.
#'
#'   \item \code{GGally.ggmatrix.progress_format}: default progress format.  Defaults to \code{" plot: [:plot_i,:plot_j] [:bar]:percent est::eta "}.  Two extra tokens are added to regular \code{progress::\link[progress]{progress_bar}} tokens for the plot location: \code{'plot_i'} and \code{'plot_j'}.
#'
#'   \item \code{GGally.ggmatrix.progress_clear}: default behavior of progress bar clearing.  Defaults to \code{TRUE}, which removes the most latest progress bar, if displayed.
#'
#' }
#' @name GGally_options
NULL


ggally_default_options <- list(
  GGally.ggmatrix.progress = NULL,
  GGally.ggmatrix.progress_format = " plot: [:plot_i,:plot_j] [:bar]:percent est::eta ",
  GGally.ggmatrix.progress_clear = TRUE
)

.onLoad <- function(libname, pkgname) {
  op <- options()
  toset <- !(names(ggally_default_options) %in% names(op))
  if (any(toset)) {
    options(ggally_default_options[toset])
  }

  invisible()
}
