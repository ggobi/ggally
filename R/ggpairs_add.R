
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
#'   \item \code{scale}: replace current scale
#'   \item \code{coord}: override current coordinate system
######   \item \code{facet}: override current coordinate faceting
#' }
#'
#' The \code{+} operator completely replaces elements
#' with elements from e2.
#'
#' @param e1 An object of class \code{ggmatrix}
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
#' ## add a list of information
#' extra <- list(ggplot2::theme_bw(), ggplot2::labs(caption = "My caption!"))
#' pm + extra
"+.gg" <- function(e1, e2) {

  if (!is.ggmatrix(e1)) {
    return(e1 %+% e2)
  }

  if (is.null(e1$gg)) {
    e1$gg <- list()
  }
  if (inherits(e2, "labels")) {
    add_labels_to_ggmatrix(e1, e2)
  } else if (is.theme(e2)) {
    add_theme_to_ggmatrix(e1, e2)
  } else if (is.list(e2)) {
    add_list_to_ggmatrix(e1, e2)
  } else if (is.ggproto(e2)) {
    add_ggproto_to_ggmatrix(e1, e2)
  } else {
    stop(
      "'ggmatrix' does not know how to add objects that do not have class 'theme', 'labels' or 'ggproto'.",
      " Received object with class: '", paste(class(e2), collapse = ", "), "'"
    )
  }
}


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
    if (is.null(e1$gg$labs)) {
      e1$gg$labs <- structure(list(), class = "labels")
    }
    e1$gg$labs[non_ggmatrix_labels] <- e2[non_ggmatrix_labels]
  }

  e1
}

add_theme_to_ggmatrix <- function(e1, e2) {
  # Get the name of what was passed in as e2, and pass along so that it
  # can be displayed in error messages
  # e2name <- deparse(substitute(e2))

  if (is.null(e1$gg$theme)) {
    e1$gg$theme <- e2
  } else {
    # calls ggplot2 add method and stores the result in gg
    e1$gg$theme <- e1$gg$theme %+% e2
  }
  e1
}

#' @export
#' @rdname gg-add
#' @param rows Integer vector. Rows to modify
#' @param cols Integer vector. Cols to modify
#' @param only_fn Character vector. If specified, only subplots
#'   of that type will be modified (see examples).
#' @details
#' \code{add_ggproto_to_ggmatrix} gives you more control to modify
#'   only some subplots.
#' @examples
#' ## modify scale
#' pm + scale_fill_brewer()
#' ## only first row
#' add_ggproto_to_ggmatrix(pm, scale_fill_brewer(), rows = 1)
#' ## only certain types of subplots
#' add_ggproto_to_ggmatrix(
#'   pm, scale_fill_brewer(),
#'   only_fn = c("ggally_box_no_facet", "ggally_barDiag")
#' )
add_ggproto_to_ggmatrix <- function(
  e1, e2,
  rows = 1:e1$nrow, cols = 1:e1$ncol,
  only_fn = NULL) {
  if (!is.ggmatrix(e1))
    stop("e1 should be a ggmatrix.")
  if (!is.ggproto(e2))
    stop("e2 should be a ggproto object.")

  for (i in rows) {
    for (j in cols) {
      pos <- get_pos(e1, i, j)
      fn_name <- attr(e1$plots[[pos]]$fn, "name")
      if (is.null(only_fn) | fn_name %in% only_fn) {
        e1[i, j] <- e1[i, j] + e2
      }
    }
  }

  e1
}

add_list_to_ggmatrix <- function(e1, e2) {
  for (item in e2) {
    e1 <- e1 + item
  }
  e1
}


is.ggmatrix <- function(x) {
  inherits(x, "ggmatrix")
}



#' Modify a ggmatrix object by adding an ggplot2 object to all plots
#'
#' @export
#' @examples
#'
#' ggpairs(iris, 1:2) + v1_ggmatrix_theme()
#' # move the column names to the left and bottom
#' ggpairs(iris, 1:2, switch = "both") + v1_ggmatrix_theme()
v1_ggmatrix_theme <- function() {
  theme(
    strip.background = element_rect(fill = "white"),
    strip.placement = "outside"
  )
}
