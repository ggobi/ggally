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
    cli::cli_abort("{.arg e1} should be a {.fn ggmatrix}.")
  }
  if (!is_ggproto(e2)) {
    cli::cli_abort("{.arg e2} should be a {.cls ggproto} object.")
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
    cli::cli_abort("{.arg e1} should be a {.fn ggmatrix}.")
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
    cli::cli_abort(c(
      "{.fn ggmatrix} does not know how to add objects that do not have class {.cls theme}, {.cls labels} or {.cls ggproto}.",
      i = "Received object with class: {.cls {class(e2)}}"
    ))
  }
}


if (utils::packageVersion("ggplot2") >= "3.5.2.002") {
  class_ggproto <- ggplot2::class_ggproto
  class_theme <- ggplot2::class_theme
  class_labels <- ggplot2::class_labels

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
