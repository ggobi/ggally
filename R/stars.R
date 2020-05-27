#' Significance Stars
#'
#' Calculate significance stars
#'
#' @param x numeric values that will be compared to the \code{point}, \code{one}, \code{two}, and \code{three} values
#' @param three threshold below which to display three stars
#' @param two threshold below which to display two stars
#' @param one threshold below which to display one star
#' @param point threshold below which to display one point (\code{NULL} to deactivate)
#' @return character vector containing the appropriate number of stars for each \code{x} value
#' @author Joseph Larmarange
#' @export
#' @examples
#' x <- c(0.5, 0.1, 0.05, 0.01, 0.001)
#' signif_stars(x)
#' signif_stars(x, one = .15, point = NULL)
signif_stars <- function(x, three = 0.001, two = 0.01, one = 0.05, point = 0.1) {
  res <- rep_len("", length.out = length(x))
  if (!is.null(point)) {
    res[x <= point] <- "."
  }
  if (!is.null(one)) {
    res[x <= one] <- "*"
  }
  if (!is.null(two)) {
    res[x <= two] <- "**"
  }
  if (!is.null(three)) {
    res[x <= three] <- "***"
  }
  res
}
