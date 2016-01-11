#' Create the data needed to generate a glyph plot.
#'
#' @param data A data frame containing variables named in \code{x_major},
#'   \code{x_minor}, \code{y_major} and \code{y_minor}.
#' @param x_major,x_minor,y_major,y_minor The name of the variable (as a
#'   string) for the major and minor x and y axes.  Together, each unique
#    combination of \code{x_major} and \code{y_major} specifies a grid cell.
#' @param polar A logical of length 1, specifying whether the glyphs should
#'   be drawn in polar coordinates.  Defaults to \code{FALSE}.
#' @param height,width The height and width of each glyph. Defaults to 95\% of
#'  the \code{\link[ggplot2]{resolution}} of the data. Specify the width
#'  absolutely by supplying a numeric vector of length 1, or relative to the
#   resolution of the data by using \code{\link[ggplot2]{rel}}.
#' @param y_scale,x_scale The scaling function to be applied to each set of
#'  minor values within a grid cell.  Defaults to \code{\link{identity}} so
#'  that no scaling is performed.
#' @export
#' @author Di Cook \email{dicook@@monash.edu}, Heike Hofmann, Hadley Wickham
#' @examples
#'  data(nasa)
#'  nasaLate <- nasa[
#'    nasa$date >= as.POSIXct("1998-01-01") &
#'    nasa$lat >= 20 &
#'    nasa$lat <= 40 &
#'    nasa$long >= -80 &
#'    nasa$long <= -60
#'  , ]
#'  temp.gly <- glyphs(nasaLate, "long", "day", "lat", "surftemp", height=2.5)
#'  ggplot2::ggplot(temp.gly, ggplot2::aes(gx, gy, group = gid)) +
#'    add_ref_lines(temp.gly, color = "grey90") +
#'    add_ref_boxes(temp.gly, color = "grey90") +
#'    ggplot2::geom_path() +
#'    ggplot2::theme_bw() +
#'    ggplot2::labs(x = "", y = "")
glyphs <- function(
  data,
  x_major, x_minor,
  y_major, y_minor,
  polar = FALSE,
  height = ggplot2::rel(0.95), width = ggplot2::rel(0.95),
  y_scale = identity,
  x_scale = identity
) {
  data$gid <- interaction(data[[x_major]], data[[y_major]], drop = TRUE)

  if (is.rel(width)) {
    width <- resolution(data[[x_major]], zero = FALSE) * unclass(width)
    message("Using width ", format(width, digits = 3))
  }

  if (is.rel(height)) {
    height <- resolution(data[[y_major]], zero = FALSE) * unclass(height)
    message("Using height ", format(height, digits = 3))
  }

  if (!identical(x_scale, identity) || !identical(y_scale, identity)) {
    data <- ddply(data, "gid", function(df) {
      df[[x_minor]] <- x_scale(df[[x_minor]])
      df[[y_minor]] <- y_scale(df[[y_minor]])
      df
    })
  }

  if (polar) {
    theta <- 2 * pi * rescale01(data[[x_minor]])
    r <- rescale01(data[[y_minor]])

    data$gx <- data[[x_major]] + width  / 2 * r * sin(theta)
    data$gy <- data[[y_major]] + height / 2 * r * cos(theta)
    data <- data[order(data[[x_major]], data[[x_minor]]), ]
  } else {
    data$gx <- data[[x_major]] + rescale11(data[[x_minor]]) * width / 2
    data$gy <- data[[y_major]] + rescale11(data[[y_minor]]) * height / 2
  }

  structure(data,
    width = width, height = height, polar = polar,
    x_major = x_major, y_major = y_major,
    class = c("glyphplot", "data.frame"))
}

# Create reference lines for a glyph plot
ref_lines <- function(data) {
  stopifnot(is.glyphplot(data))

  glyph <- attributes(data)

  cells <- unique(data[c(glyph$x_major, glyph$y_major, "gid")])

  if (glyph$polar) {
    ref_line <- function(df) {
      theta <- seq(0, 2 * pi, length = 30)
      data.frame(
        gid = df$gid,
        gx = df[[glyph$x_major]] + glyph$width / 4 * sin(theta),
        gy = df[[glyph$y_major]] + glyph$height / 4 * cos(theta)
      )

    }
  } else {
    ref_line <- function(df) {
      data.frame(
        gid = df$gid,
        gx = df[[glyph$x_major]] + c(-1, 1) * glyph$width / 2,
        gy = df[[glyph$y_major]]
      )
    }
  }
  ddply(cells, "gid", ref_line)
}

# Create reference boxes for a glyph plot
ref_boxes <- function(data, fill = NULL) {
  stopifnot(is.glyphplot(data))
  glyph <- attributes(data)
  cells <- data.frame(unique(data[c(glyph$x_major, glyph$y_major, "gid", fill)]))

  df <- data.frame(xmin = cells[[glyph$x_major]] - glyph$width / 2,
      xmax = cells[[glyph$x_major]] + glyph$width / 2,
      ymin = cells[[glyph$y_major]] - glyph$height / 2,
      ymax = cells[[glyph$y_major]] + glyph$height / 2)
  if (!is.null(fill)){
    df$fill <- cells[[fill]]
  }
  df
}


# Glyph plot class -----------------------------------------------------------

#' Glyph plot class
#'
#' @param data A data frame containing variables named in \code{x_major},
#'   \code{x_minor}, \code{y_major} and \code{y_minor}.
#' @param height,width The height and width of each glyph. Defaults to 95\% of
#'  the \code{\link[ggplot2]{resolution}} of the data. Specify the width
#'  absolutely by supplying a numeric vector of length 1, or relative to the
#   resolution of the data by using \code{\link[ggplot2]{rel}}.
#' @param polar A logical of length 1, specifying whether the glyphs should
#'   be drawn in polar coordinates.  Defaults to \code{FALSE}.
#' @param x_major,y_major The name of the variable (as a
#'   string) for the major x and y axes.  Together, the
#    combination of \code{x_major} and \code{y_major} specifies a grid cell.
#' @export
#' @author Di Cook \email{dicook@@monash.edu}, Heike Hofmann, Hadley Wickham
glyphplot <- function(data, width, height, polar, x_major, y_major) {
  structure(data,
    width = width, height = height, polar = polar,
    x_major = x_major, y_major = y_major,
    class = c("glyphplot", "data.frame"))
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
    polar = attr(x, "polar"))
}

#' @param x glyphplot to be printed
#' @param ... ignored
#' @export
#' @rdname glyphplot
#' @method print glyphplot
print.glyphplot <- function(x, ...) {
  NextMethod()
  if (attr(x, "polar")) {
    cat("Polar ")
  } else {
    cat("Cartesian ")
  }
  width <- format(attr(x, "width"), digits = 3)
  height <- format(attr(x, "height"), digits = 3)

  cat("glyphplot: \n")
  cat("  Size: [", width, ", ", height,  "]\n", sep = "")
  cat("  Major axes: ", attr(x, "x_major"), ", ", attr(x, "y_major"), "\n",
    sep = "")
  # cat("\n")
}


# Relative dimensions --------------------------------------------------------

# Relative dimensions
#
# @param x numeric value between 0 and 1
# rel <- function(x) {
#   structure(x, class = "rel")
# }
# @export
# rel <- ggplot2::rel

# @rdname rel
# @param ... ignored
# print.rel <- function(x, ...) {
#   print(noquote(paste(x, " *", sep = "")))
# }
## works even though it is not exported
# @export
# ggplot2::print.rel

# @rdname rel
# is.rel <- function(x) {
#   inherits(x, "rel")
# }
## only used internally.  and ggplot2 has this exported
# @export
# ggplot2:::is.rel
is.rel <- ggplot2:::is.rel

# Rescaling functions --------------------------------------------------------

#' Rescaling functions
#'
#' @param x numeric vector
#' @param xlim value used in \code{range}
#' @name rescale01


#' @export
#' @rdname rescale01
range01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

#' @export
#' @rdname rescale01
max1 <- function(x) {
  x / max(x, na.rm = TRUE)
}
#' @export
#' @rdname rescale01
mean0 <- function(x) {
  x - mean(x, na.rm = TRUE)
}
#' @export
#' @rdname rescale01
min0 <- function(x) {
  x - min(x, na.rm = TRUE)
}
#' @export
#' @rdname rescale01
rescale01 <- function(x, xlim=NULL) {
  if (is.null(xlim)) {
    rng <- range(x, na.rm = TRUE)
  } else {
    rng <- xlim
  }
  (x - rng[1]) / (rng[2] - rng[1])
}
#' @export
#' @rdname rescale01
rescale11 <- function(x, xlim=NULL) {
  2 * rescale01(x, xlim) - 1
}

#' Add reference lines for each cell of the glyphmap.
#'
#' @param data A glyphmap structure.
#' @param color Set the color to draw in, default is "white"
#' @param size Set the line size, default is 1.5
#' @param ... other arguments passed onto \code{\link[ggplot2]{geom_line}}
#' @export
add_ref_lines <- function(data, color = "white", size = 1.5, ...){
  rl <- ref_lines(data)
  geom_path(data = rl, color = color, size = size, ...)
}

#' Add reference boxes around each cell of the glyphmap.
#'
#' @param data A glyphmap structure.
#' @param var_fill Variable name to use to set the fill color
#' @param color Set the color to draw in, default is "white"
#' @param size Set the line size, default is 0.5
#' @param fill fill value used if \code{var_fill} is \code{NULL}
#' @param ... other arguments passed onto \code{\link[ggplot2]{geom_rect}}
#' @export
add_ref_boxes <- function(data, var_fill = NULL, color = "white", size = 0.5,
                          fill = NA, ...){
  rb <- ref_boxes(data, var_fill)
  if (!is.null(var_fill)){
    geom_rect(aes_all(names(rb)), data = rb,
              color = color, size = size, inherit.aes = FALSE, ...)
  }
  else{
    geom_rect(aes_all(names(rb)), data = rb,
              color = color, size = size, inherit.aes = FALSE, fill = fill, ...)
  }
}
