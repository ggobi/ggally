
#' Print if not CRAN
#'
#' Small function to print a plot if the R session is interactive or in a CI build
#'
#' @param p plot to be displayed
#' @export
print_if_interactive <- function(p) {
  if (interactive() || nzchar(Sys.getenv("CAN_PRINT")) || on_ci())) {
    print(p)
  }
}
on_ci <- function() {
  isTRUE(as.logical(Sys.getenv("CI")))
}


#' Loads package namespaces
#'
#' Loads package namespaces or yells at user... loudly
#'
#' @param pkgs vector of character values
#' @keywords internal
require_namespaces <- function(pkgs) {
  for (pkg in pkgs) {
    if (! requireNamespace(pkg, quietly = TRUE)) {
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
