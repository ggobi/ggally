ggmatrix_progress <- function(
  format = " plot: [:plot_i,:plot_j] [:bar]:percent est::eta ",
  clear = TRUE,
  show_after = 0,
  ...
) {
  ret <- function(total) {
    progress::progress_bar$new(
      format = format,
      clear = clear,
      show_after = show_after,
      total = total,
      ...
    )
  }

  ret
}


as_ggmatrix_progress = function(x, pm) {
  if (isFALSE(x)) {
    return(FALSE)
  }
  if (isTRUE(x)) {
    return(ggmatrix_progress())
  }
  if (is.null(x)) {
    shouldDisplay <- interactive() && (pm$ncol * pm$nrow) > 15
    if (!shouldDisplay) {
      return(FALSE)
    } else {
      return(ggmatrix_progress())
    }
  }
  if (is.function(x)) {
    return(x)
  }

  stop("as_ggmatrix_progress only knows how to handle TRUE, FALSE, NULL, or a function.  If a function, it must return a new progress_bar")
}

isFALSE <- function(x) {
  identical(FALSE, x)
}
