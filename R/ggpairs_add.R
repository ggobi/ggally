add_gg_info <- function(p, gg) {
  if (!is.null(gg)) {
    if (!is.null(gg$theme)) {
      p <- p + gg$theme
    }
    if (!is.null(gg$labs)) {
      p <- p + gg$labs
    }
  }
  p
}


add_labels_to_ggmatrix <- function(e1, e2) {
  label_names <- names(e2)

  if ("x" %in% label_names) {
    e1$xlab <- e2$x
  }
  if ("y" %in% label_names) {
    e1$ylab <- e2$y
  }
  if ("title" %in% label_names) {
    e1$title <- e2$title
  }

  non_ggmatrix_labels <- label_names[!label_names %in% c("x", "y", "title")]

  if (length(non_ggmatrix_labels) > 0) {
    if (is.null(e1$gg)) {
      e1$gg <- list()
    }

    if (is.null(e1$gg$labs)) {
      e1$gg$labs <- labs()
    }
    e1$gg$labs[non_ggmatrix_labels] <- e2[non_ggmatrix_labels]
  }

  e1
}

add_theme_to_ggmatrix <- function(e1, e2) {
  if (is.null(e1$gg)) {
    e1$gg <- list()
  }
  # Get the name of what was passed in as e2, and pass along so that it
  # can be displayed in error messages
  # e2name <- deparse(substitute(e2))

  if (is.null(e1$gg$theme)) {
    e1$gg$theme <- e2
  } else {
    # calls ggplot2 add method and stores the result in gg
    e1$gg$theme <- e1$gg$theme + e2
  }
  e1
}

#' Modify a \code{\link{ggmatrix}} object by adding an \pkg{ggplot2} object to all plots
#'
#' This operator allows you to add \pkg{ggplot2} objects to a \code{\link{ggmatrix}} object.
#'
#' If the first object is an object of class \code{\link{ggmatrix}}, you can add
#' the following types of objects, and it will return a modified \pkg{ggplot2}
#' object.
#'
#' \itemize{
######   \item \code{data.frame}: replace current data.frame
######      (must use \code{%+%})
######   \item \code{uneval}: replace current aesthetics
######   \item \code{layer}: add new layer
#'   \item \code{theme}: update plot theme
#'   \item \code{scale}: replace current scale
#'   \item \code{coord}: override current coordinate system
######   \item \code{facet}: override current coordinate faceting
#' }
#'
#' The \code{+} operator completely replaces elements
#' with elements from e2.
#'
#' @param e1 An object of class \code{\link{ggnostic}} or \code{ggplot}
#' @param e2 A component to add to \code{e1}
#' @export
#' @inheritParams ggmatrix_location
#' @details
#' \code{add_to_ggmatrix} gives you more control to modify
#'   only some subplots.  This function may be replaced and/or removed in the future. \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#' @seealso \code{\link{ggmatrix_location}}
#' @examples
#' # small function to display plots only if it's interactive
#' p_ <- GGally::print_if_interactive
#' data(tips)
#'
#' pm <- ggpairs(tips[, 2:4], ggplot2::aes(color = sex))
#' ## change to black and white theme
#' pm + ggplot2::theme_bw()
#' ## change to linedraw theme
#' p_(pm + ggplot2::theme_linedraw())
#' ## change to custom theme
#' p_(pm + ggplot2::theme(panel.background = ggplot2::element_rect(fill = "lightblue")))
#' ## add a list of information
#' extra <- list(ggplot2::theme_bw(), ggplot2::labs(caption = "My caption!"))
#' p_(pm + extra)
#'
#' ## modify scale
#' p_(pm + scale_fill_brewer(type = "qual"))
#' ## only first row
#' p_(add_to_ggmatrix(pm, scale_fill_brewer(type = "qual"), rows = 1:2))
#' ## only second col
#' p_(add_to_ggmatrix(pm, scale_fill_brewer(type = "qual"), cols = 2:3))
#' ## only to upper triangle of plot matrix
#' p_(add_to_ggmatrix(
#'   pm,
#'   scale_fill_brewer(type = "qual"),
#'   location = "upper"
#' ))
add_to_ggmatrix <- function(
  e1,
  e2,
  location = NULL,
  rows = NULL,
  cols = NULL
) {
  if (!is_ggmatrix(e1)) {
    stop("e1 should be a ggmatrix.")
  }
  if (!is_ggproto(e2)) {
    stop("e2 should be a ggproto object.")
  }

  pm <- e1
  gg <- e2

  loc <- ggmatrix_location(pm, location = location, rows = rows, cols = cols)

  row_vals <- loc$row
  col_vals <- loc$col

  for (i in seq_along(row_vals)) {
    row <- row_vals[i]
    col <- col_vals[i]
    # wrap in try to not let one plot fail, but also print the error
    try({
      pm[row, col] <- pm[row, col] + gg
    })
  }

  pm
}

#' \code{\link{ggmatrix}} plot locations
#'
#' \lifecycle{experimental}
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
    stop("pm should be a ggmatrix.")
  }

  if (!is.null(location)) {
    if (
      is.logical(location) && !(is.matrix(location) || is.data.frame(location))
    ) {
      if (length(location) != 1) {
        stop("`location` logical value must be of length 1")
      }
      location <-
        if (isTRUE(location)) {
          "all"
        } else {
          warning("Not `TRUE` logical `location` value. Setting to `'none'`")
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
          stop(location, " not implemented")
        )
    } else {
      if (is.matrix(location)) {
        location <- as.data.frame(location)
      }
      if (is.data.frame(location)) {
        if (!identical(c("row", "col"), colnames(location))) {
          # using data.frame of locations as truthy vals
          if (ncol(location) != pm$ncol) {
            stop("location provided does not have the same size of columns")
          }
          if (nrow(location) != pm$nrow) {
            stop("location provided does not have the same size of rows")
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
      stop("rows must be numeric")
    }
    if (is.null(cols)) {
      cols <- seq_len(pm$ncol)
    }
    if (!is.numeric(cols)) {
      stop("cols must be numeric")
    }
    location <- expand.grid(row = rows, col = cols)
  }

  # location will be a 2d data.frame with colnames of `'row'` and `'col'`
  locs <- as.data.frame(location)
  if (ncol(locs) < 2) {
    utils::str(locs)
    stop("not enough columns to inspect for a location")
  }
  if (!all(c("row", "col") %in% colnames(locs))) {
    stop("invalid location row / col object")
  }

  row <- locs$row
  if (any(row > pm$nrow) || any(row <= 0) || any(is.na(row))) {
    stop(
      "`row` must be non-NA / positive numeric values `<= pm$nrow`",
      "\n",
      "pm$nrow: ",
      dput_val(pm$nrow),
      "\n",
      "row: ",
      dput_val(row)
    )
  }
  col <- locs$col
  if (any(col > pm$ncol) || any(col <= 0) || any(is.na(col))) {
    stop(
      "`col` must be non-NA / positive numeric values `<= pm$ncol`",
      "\n",
      "pm$ncol: ",
      dput_val(pm$ncol),
      "\n",
      "col: ",
      dput_val(col)
    )
  }

  # typical case
  return(
    locs[, c("row", "col")]
  )
}

add_list_to_ggmatrix <- function(e1, e2) {
  for (item in e2) {
    e1 <- e1 + item
  }
  e1
}


#' Check if an object is a ggmatrix
#'
#' @param x An object to check
#' @return Logical value indicating if the object is a `ggmatrix`
#' @export
#' @examples
#' is_ggmatrix(ggpairs(mtcars))
#' is_ggmatrix(ggplot2::ggplot())
is_ggmatrix <- function(x) {
  inherits(x, "ggmatrix")
}


# -------------------------

#' @rawNamespace if (utils::packageVersion("ggplot2") < "3.5.2.9001") S3method("+",ggmatrix)
NULL
#' @exportS3Method NULL
"+.ggmatrix" <- function(e1, e2) {
  if (!is_ggmatrix(e1)) {
    stop("e1 should be a ggmatrix.")
  }

  if (inherits(e2, c("labels", "ggplot2::labels"))) {
    add_labels_to_ggmatrix(e1, e2)
  } else if (is_theme(e2)) {
    add_theme_to_ggmatrix(e1, e2)
  } else if (is.list(e2)) {
    add_list_to_ggmatrix(e1, e2)
  } else if (is_ggproto(e2)) {
    add_to_ggmatrix(e1, e2)
  } else {
    stop(
      "'ggmatrix' does not know how to add objects that do not have class 'theme', 'labels' or 'ggproto'.",
      " Received object with class: '",
      paste(class(e2), collapse = ", "),
      "'"
    )
  }
}


if (utils::packageVersion("ggplot2") >= "3.5.2.001") {
  # ggplot2 3.5.2.9001 and later!

  class_gg <- utils::getFromNamespace("class_gg", "ggplot2")

  class_ggproto <- utils::getFromNamespace("class_ggproto", "ggplot2")
  # class_gtable <- utils::getFromNamespace("class_gtable", "ggplot2")
  # class_scale <- utils::getFromNamespace("class_scale", "ggplot2")
  # class_guides <- utils::getFromNamespace("class_guides", "ggplot2")
  # class_coord <- utils::getFromNamespace("class_coord", "ggplot2")
  # class_facet <- utils::getFromNamespace("class_facet", "ggplot2")
  # class_layer <- utils::getFromNamespace("class_layer", "ggplot2")
  # class_layout <- utils::getFromNamespace("class_layout", "ggplot2")
  # class_scales_list <- utils::getFromNamespace("class_scales_list", "ggplot2")
  class_theme <- utils::getFromNamespace("class_theme", "ggplot2")
  class_labels <- utils::getFromNamespace("class_labels", "ggplot2")
  # class_mapping <- utils::getFromNamespace("class_mapping", "ggplot2")
  # class_ggplot <- utils::getFromNamespace("class_ggplot", "ggplot2")
  # class_ggplot_built <- utils::getFromNamespace("class_ggplot_built", "ggplot2")

  method(`+`, list(ggmatrix, class_labels)) <-
    function(e1, e2) {
      add_labels_to_ggmatrix(e1, e2)
    }

  method(`+`, list(ggmatrix, class_theme)) <-
    function(e1, e2) {
      add_theme_to_ggmatrix(e1, e2)
    }

  method(`+`, list(ggmatrix, class_ggproto)) <-
    function(e1, e2) {
      add_to_ggmatrix(e1, e2)
    }

  method(`+`, list(ggmatrix, class_list)) <-
    function(e1, e2) {
      add_list_to_ggmatrix(e1, e2)
    }

  method(`+`, list(ggmatrix, class_any)) <-
    function(e1, e2) {
      # Fallback support for ggplot2 <= 3.5.2
      `+.ggmatrix`(e1, e2)
    }
}
