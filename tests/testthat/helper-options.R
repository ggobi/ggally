rq <- function(...) {
  suppressPackageStartupMessages(require(..., quietly = TRUE))
}

with_options <- function(opts, expr) {
  old_opts <- options(opts)
  on.exit(options(old_opts))
  force(expr)
}
