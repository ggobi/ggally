#' Create data for glyph map
#'
#' Create the data needed to generate a glyph plot.
#'
#' The \code{glyphs} functions
#' now extract the data object created with \code{\link{cubble::geom_glyph}}
#'
#' @param data A data frame containing variables named in \code{x_major},
#'   \code{x_minor}, \code{y_major} and \code{y_minor}.
#' @param x_major,x_minor,y_major,y_minor The name of the variable (as a
#'   string) for the major and minor x and y axes.  Together, each unique
#    combination of \code{x_major} and \code{y_major} specifies a grid cell.
#' @param polar A logical of length 1, specifying whether the glyphs should
#'   be drawn in polar coordinates.  Defaults to \code{FALSE}.
#' @param height,width The height and width of each glyph. Defaults to 95% of
#'  the \code{\link[ggplot2]{resolution}} of the data. Specify the width
#'  absolutely by supplying a numeric vector of length 1, or relative to the
#   resolution of the data by using \code{\link[ggplot2]{rel}}.
#' @param y_scale,x_scale The scaling function to be applied to each set of
#'  minor values within a grid cell.  Defaults to \code{\link{identity}} so
#'  that no scaling is performed.
#' @export
#' @importFrom cubble geom_glyph geom_glyph_line geom_glyph_box
#' @author Di Cook, Heike Hofmann, Hadley Wickham, Sherry Zhang
#' @examples
#' # Small function to display plots only if it's interactive
#' p_ <- GGally::print_if_interactive
#'
#' data(nasa)
#' nasaLate <- nasa[
#'   nasa$date >= as.POSIXct("1998-01-01") &
#'   nasa$lat >= 20 &
#'   nasa$lat <= 40 &
#'   nasa$long >= -80 &
#'   nasa$long <= -60
#' , ]
#' temp.gly <- glyphs(nasaLate, "long", "day", "lat", "surftemp", height=2.5)
#' p_(ggplot2::ggplot(temp.gly, ggplot2::aes(gx, gy, group = gid)) +
#'   add_ref_lines(temp.gly, color = "grey90") +
#'   add_ref_boxes(temp.gly, color = "grey90") +
#'   ggplot2::geom_path() +
#'   ggplot2::theme_bw() +
#'   ggplot2::labs(x = "", y = ""))
glyphs <- function(
  data,
  x_major, x_minor,
  y_major, y_minor,
  polar = FALSE,
  height = ggplot2::rel(0.95), width = ggplot2::rel(0.95),
  y_scale = identity,
  x_scale = identity
) {

  x_scale <- enquo(x_scale)
  y_scale <- enquo(y_scale)
  out <- glyph_layer(data, component = "glyph",
                     x_major, x_minor, y_major, y_minor,
                     polar, height, width, x_scale, y_scale)
  new_cols <- out %>%
    dplyr::select(group, x, y) %>%
    dplyr::rename(gid = group, gx = x, gy = y)

  data <- data %>% dplyr::bind_cols(new_cols)

  if (!identical(x_scale, identity) || !identical(y_scale, identity)){
    data[[x_minor]] <- out$x_minor
    data[[y_minor]] <- out$y_minor
  }

  tibble::new_tibble(data, nrow = nrow(data),
            width = width, height = height, polar = polar,
            x_major = x_major, y_major = y_major,
            x_minor = x_minor, y_minor = y_minor,
            class = "glyphplot")
}



glyph_layer <- function(data, component = c("glyph", "line", "box"),
                        x_major, x_minor, y_major, y_minor,
                        polar, height, width, x_scale, y_scale){

  if (missing(x_major)) x_major <- attr(data, "x_major")
  if (missing(x_minor)) x_minor <- attr(data, "x_minor")
  if (missing(y_major)) y_major <- attr(data, "y_major")
  if (missing(y_minor)) y_minor <- attr(data, "y_minor")
  if (missing(polar)) polar <- attr(data, "polar")
  if (missing(width)) width <- attr(data, "width")
  if (missing(height)) height <- attr(data, "height")

  p <- ggplot2::ggplot(data = data,
                         aes(x_major = data[[x_major]], x_minor = data[[x_minor]],
                             y_major = data[[y_major]], y_minor = data[[y_minor]],
                             x_scale = rlang::quo_name(x_scale),
                             y_scale = rlang::quo_name(y_scale)),
                         polar = polar, height = height, width = width)

  out <- switch (component,
    "glyph" = p + cubble::geom_glyph(),
    "line" = p + cubble::geom_glyph_line(),
    "box" = p + cubble::geom_glyph_box()
  )

  res <- ggplot2::layer_data(out)

  if (component %in% c("line", "box")) res <- res %>% dplyr::rename(gx = x, gy = y, gid = group)
  res
}

add_ref_lines <- function(data, color = "white", size = 1.5, ...){

  data <- glyph_layer(data, component = "line")
  ggplot2::geom_path(data = data, color = color, size = size, ...)
}

add_ref_boxes <- function(data, var_fill = NULL, color = "white", size = 0.5,
                          fill = NA, ...){

  data <- glyph_layer(data, component = "box")
  data <- data %>% dplyr::select(gid, gx, gy, xmin:ymax)

  if (!is.null(var_fill)) data$fill <- var_fill
  suppressWarnings(ggplot2::geom_rect(
    data = data,
    aes_all(names(data)),
    color = color, size = size,fill = fill, ...))

}


# Glyph plot class -----------------------------------------------------------

#' Glyph plot class
#'
#' @param data A data frame containing variables named in \code{x_major},
#'   \code{x_minor}, \code{y_major} and \code{y_minor}.
#' @param height,width The height and width of each glyph. Defaults to 95% of
#'  the \code{\link[ggplot2]{resolution}} of the data. Specify the width
#'  absolutely by supplying a numeric vector of length 1, or relative to the
#   resolution of the data by using \code{\link[ggplot2]{rel}}.
#' @param polar A logical of length 1, specifying whether the glyphs should
#'   be drawn in polar coordinates.  Defaults to \code{FALSE}.
#' @param x_major,x_minor,y_major,y_minorm The name of the variable (as a
#'   string) for the major x and y axes.  Together, the
#    combination of \code{x_major} and \code{y_major} specifies a grid cell.
#' @export
#' @author Di Cook, Heike Hofmann, Hadley Wickham, Sherry Zhang
glyphplot <- function(data, width, height, polar, x_major, y_major, x_minor, y_minor) {
  tibble::new_tibble(data, nrow = nrow(data),
            width = width, height = height, polar = polar,
            x_major = x_major, y_major = y_major,
            x_minor = x_minor, y_minor = y_minor,
            class = "glyphplot")
}
#' @export
#' @rdname glyphplot
is.glyphplot <- function(x) {
  inherits(x, "glyphplot")
}
#' @export
#' @rdname glyphplot
"[.glyphplot" <- function(x, ...) {
  glyphplot(NextMethod(),
            width = attr(x, "width"), height = attr(x, "height"),
            x_major = attr(x, "x_major"), y_major = attr(x, "y_major"),
            x_minor = attr(x, "x_minor"), y_minor = attr(x, "y_minor"),
            polar = attr(x, "polar"))
}

#' @param x glyphplot to be printed
#' @param ... ignored
#' @export
#' @rdname glyphplot
#' @importFrom tibble tbl_sum
#' @method tbl_sum glyphplot
tbl_sum.glyphplot <- function(x, ...) {
  if (attr(x, "polar")) {
    coord <- "polar"
  } else {
    coord <- "cartesian"
  }

  dim <- glue::glue(nrow(x), " x ", ncol(x))
  width <- format(attr(x, "width"), digits = 3)
  height <- format(attr(x, "height"), digits = 3)

  size <- glue::glue("[", width, ", ", height,  "]\n", sep = "")
  axes <- glue::glue(attr(x, "x_major"), ", ", attr(x, "y_major"), "\n",
      sep = "")

  c(glyphplot = dim, coord = coord, size = size, "major axes" = axes)
}
