
#' Modify a ggmatrix object by adding an ggplot2 object to all plots
#'
#' This operator allows you to add ggplot2 objects to a ggmatrix object.
#'
#' If the first object is an object of class \code{ggmatrix}, you can add
#' the following types of objects, and it will return a modified ggplot
#' object.
#'
#' \itemize{
######   \item \code{data.frame}: replace current data.frame
######      (must use \code{\%+\%})
######   \item \code{uneval}: replace current aesthetics
######   \item \code{layer}: add new layer
#'   \item \code{theme}: update plot theme
######   \item \code{scale}: replace current scale
######   \item \code{coord}: override current coordinate system
######   \item \code{facet}: override current coordinate faceting
#' }
#'
#' The \code{+} operator completely replaces elements
#' with elements from e2.
#'
#' @param e1 An object of class \code{ggplot} or \code{theme}
#' @param e2 A component to add to \code{e1}
#'
#' @export
#' @seealso \code{\link[ggplot2]{+.gg}} and \code{\link[ggplot2]{theme}}
#' @method + gg
#' @rdname gg-add
#' @examples
#' data(tips, package = "reshape")
#' pm <- ggpairs(tips[, 2:3])
#' ## change to black and white theme
#' pm + ggplot2::theme_bw()
#' ## change to linedraw theme
#' # pm + ggplot2::theme_linedraw()
#' ## change to custom theme
#' # pm + ggplot2::theme(panel.background = ggplot2::element_rect(fill = "lightblue"))
#'
"+.gg" <- function(e1, e2) {

  if (is.ggmatrix(e1)) {
    if (is.theme(e2)) {
      # Get the name of what was passed in as e2, and pass along so that it
      # can be displayed in error messages
      # e2name <- deparse(substitute(e2))

      if (is.null(e1$gg)) {
        e1$gg <- e2
      } else {
        # calls ggplot2 add method and stores the result in gg
        e1$gg <- e1$gg %+% e2
      }
      e1

    } else {
      stop("'ggmatrix' does not know how to add objects that do not have class 'theme'")
    }

  } else {
    # calls ggplot2 add method
    e1 %+% e2
  }
}


is.ggmatrix <- function(x) {
  inherits(x, "ggmatrix")
}
