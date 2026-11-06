#' \code{\link{ggmatrix}} plot locations
#'
#' `r lifecycle::badge("experimental")`
#'
#' Convert many types of location values to a consistent \code{data.frame} of \code{row} and \code{col} values.
#'
#' @param pm \code{\link{ggmatrix}} plot object
#' @param location \describe{
#'   \item{\code{"all"}, \code{TRUE}}{All row and col combinations}
#'   \item{\code{"none"}}{No row and column combinations}
#'   \item{\code{"upper"}}{Locations where the column value is higher than the row value}
#'   \item{\code{"lower"}}{Locations where the row value is higher than the column value}
#'   \item{\code{"diag"}}{Locations where the column value is equal to the row value}
#'   \item{\code{matrix} or \code{data.frame}}{
#'     \code{matrix} values will be converted into \code{data.frame}s.
#'     \itemize{
#'       \item A \code{data.frame} with the exact column names \code{c("row", "col")}
#'       \item A \code{data.frame} with the number of rows and columns matching the plot matrix object provided.  Each cell will be tested for a "truthy" value to determine if the location should be kept.
#'     }
#'   }
#' }
#' @param rows numeric vector of the rows to be used. Will be used with  \code{cols} if \code{location} is \code{NULL}
#' @param cols numeric vector of the cols to be used. Will be used with \code{rows} if \code{location} is \code{NULL}
#' @return Data frame with columns \code{c("row", "col")} containing locations for the plot matrix
#' @export
#' @examples
#' pm <- ggpairs(tips, 1:3)
#'
#' # All locations
#' ggmatrix_location(pm, location = "all")
#' ggmatrix_location(pm, location = TRUE)
#'
#' # No locations
#' ggmatrix_location(pm, location = "none")
#'
#' # "upper" triangle locations
#' ggmatrix_location(pm, location = "upper")
#'
#' # "lower" triangle locations
#' ggmatrix_location(pm, location = "lower")
#'
#' # "diag" locations
#' ggmatrix_location(pm, location = "diag")
#'
#' # specific rows
#' ggmatrix_location(pm, rows = 2)
#'
#' # specific columns
#' ggmatrix_location(pm, cols = 2)
#'
#' # row and column combinations
#' ggmatrix_location(pm, rows = c(1, 2), cols = c(1, 3))
#'
#' # matrix locations
#' mat <- matrix(TRUE, ncol = 3, nrow = 3)
#' mat[1, 1] <- FALSE
#' locs <- ggmatrix_location(pm, location = mat)
#' ## does not contain the 1, 1 cell
#' locs
#'
#' # Use the output of a prior ggmatrix_location
#' ggmatrix_location(pm, location = locs)
ggmatrix_location <- function(
  pm,
  location = NULL,
  rows = NULL,
  cols = NULL
) {
  if (!is_ggmatrix(pm)) {
    cli::cli_abort("{.arg pm} should be a {.fn ggmatrix}.")
  }

  if (!is.null(location)) {
    if (
      is.logical(location) && !(is.matrix(location) || is.data.frame(location))
    ) {
      if (length(location) != 1) {
        cli::cli_abort("{.arg location} logical value must be of length 1")
      }
      location <-
        if (isTRUE(location)) {
          "all"
        } else {
          cli::cli_warn(
            "Not {.code TRUE} logical {.arg location} value. Setting to {.code 'none'}"
          )
          "none"
        }
    }
    if (is.character(location)) {
      location <- match.arg(
        location,
        c("all", "upper", "lower", "diag", "none"),
        several.ok = FALSE
      )
      locs <- expand.grid(row = seq_len(pm$nrow), col = seq_len(pm$ncol))

      location <-
        switch(
          location,
          "all" = locs,
          "none" = subset(locs, FALSE),
          "diag" = subset(locs, row == col),
          "upper" = subset(locs, col > row),
          "lower" = subset(locs, col < row),
          cli::cli_abort("{location} not implemented")
        )
    } else {
      if (is.matrix(location)) {
        location <- as.data.frame(location)
      }
      if (is.data.frame(location)) {
        if (!identical(c("row", "col"), colnames(location))) {
          # using data.frame of locations as truthy vals
          if (ncol(location) != pm$ncol) {
            cli::cli_abort(
              "{.arg location} provided does not have the same size of columns"
            )
          }
          if (nrow(location) != pm$nrow) {
            cli::cli_abort(
              "{.arg location} provided does not have the same size of rows"
            )
          }

          # turn wide matrix into a tall data.frame of row/col combos
          tmp_locs <- data.frame(row = numeric(0), col = numeric(0))
          for (i in seq_len(nrow(location))) {
            for (j in seq_len(ncol(location))) {
              val <- location[i, j]
              if (val) {
                tmp_locs[nrow(tmp_locs) + 1, ] <- list(row = i, col = j)
              }
            }
          }
          location <- tmp_locs
        } # end (location is data.frame)
      } # end (location not character)
    } # end (location not null)
  } else {
    # location is null

    if (is.null(rows)) {
      rows <- seq_len(pm$nrow)
    }
    if (!is.numeric(rows)) {
      cli::cli_abort("{.arg rows} must be numeric")
    }
    if (is.null(cols)) {
      cols <- seq_len(pm$ncol)
    }
    if (!is.numeric(cols)) {
      cli::cli_abort("{.arg cols} must be numeric")
    }
    location <- expand.grid(row = rows, col = cols)
  }

  # location will be a 2d data.frame with colnames of `'row'` and `'col'`
  locs <- as.data.frame(location)
  if (ncol(locs) < 2) {
    utils::str(locs)
    cli::cli_abort("not enough columns to inspect for a location")
  }
  if (!all(c("row", "col") %in% colnames(locs))) {
    cli::cli_abort("invalid location row / col object")
  }

  row <- locs$row
  if (any(row > pm$nrow) || any(row <= 0) || any(is.na(row))) {
    cli::cli_abort(c(
      "{.code row} must be non-NA / positive numeric values {.code <= pm$nrow}",
      "*" = "{.code pm$nrow}: {.val {pm$nrow}}",
      "*" = "row: {.val {row}}"
    ))
  }
  col <- locs$col
  if (any(col > pm$ncol) || any(col <= 0) || any(is.na(col))) {
    cli::cli_abort(c(
      "{.code col} must be non-NA / positive numeric values {.code <= pm$ncol}",
      "*" = "{.code pm$ncol}: {.val {pm$ncol}}",
      "*" = "{.code col}: {.val {col}}"
    ))
  }

  # typical case
  return(
    locs[, c("row", "col")]
  )
}
