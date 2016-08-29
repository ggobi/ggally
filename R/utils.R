
#' Print if not CRAN
#'
#' Small function to print a plot if the R session is interactive or in a travis build
#'
#' @param p plot to be displayed
#' @export
print_if_interactive <- function(p) {
  if (interactive() || nzchar(Sys.getenv("CAN_PRINT"))) {
    print(p)
  }
}


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

# str_replace <- function(string, pattern, replacement) {
#   sub(pattern, replacement, string)
# }

ifnull <- function(a, b) {
  if (!is.null(a)) {
    a
  } else {
    b
  }
}


hf <- function(field) {
  eval(parse(text = read.dcf(".helper_functions", fields = field)))
}
