
#' Require packages
#'
#' Requires packages or yells at user... loudly
#'
#' @param pkgs vector of character values
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
require_pkgs <- function(pkgs) {
  for (pkg in pkgs) {
    if (! require(pkg, character.only = TRUE)) {
      stop(str_c("please install the package '", pkg, "'.  install.packages('", pkg, "') "))
    }
  }
}


str_c <- function (..., sep = "", collapse = NULL) {
  paste(..., sep = sep, collapse = collapse)
}

str_detect <- function(string, pattern, ...) {
  grepl(pattern, string, ...)
}

str_replace <- function(string, pattern, replacement) {
  sub(pattern, replacement, string)
}
