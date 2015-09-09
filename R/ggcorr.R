if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(".row.names.", "x", "y", "c", "b"))
}

#' ggcorr - Plot a correlation matrix with ggplot2
#'
#' Function for making a correlation matrix plot, using ggplot2.
#' The function is directly inspired by Tian Zheng and Yu-Sung Su's
#' \code{\link[arm]{corrplot}} function.
#' Please visit \url{http://github.com/briatte/ggcorr} for the latest version
#' of \code{ggcorr}.
#'
#' @export
#' @param data a data frame or matrix containing numeric (continuous) data. If
#' any of the columns contain non-numeric data, they will be dropped with a
#' warning.
#' @param method a vector of two character strings. The first value gives the
#' method for computing covariances in the presence of missing values, and must
#' be (an abbreviation of) one of \code{"everything"}, \code{"all.obs"},
#' \code{"complete.obs"}, \code{"na.or.complete"} or
#' \code{"pairwise.complete.obs"}. The second value gives the type of
#' correlation coefficient to compute, and must be one of \code{"pearson"},
#' \code{"kendall"} or \code{"spearman"}.
#' See \code{\link[stats]{cor}} for details.
#' Defaults to \code{c("pairwise", "pearson")}.
#' @param cor_matrix the named correlation matrix to use for calculations.
#' Defaults to the correlation matrix of \code{data} when \code{data} is
#' supplied.
#' @param palette if \code{nbreaks} has been set to something, a ColorBrewer
#' palette to be used for correlation coefficients. Defaults to \code{"RdYlGn"}.
#' @param name a character string for the legend that shows quintiles of
#' correlation coefficients. Defaults to nothing.
#' @param geom the geom object to use. Accepts either \code{"tile"},
#' \code{"circle"}, \code{"text"} or \code{"blank"}.
#' @param min_size when \code{geom} has been set to \code{"circle"}, the minimum
#' size of the circles.
#' Defaults to \code{2}.
#' @param max_size when \code{geom} has been set to \code{"circle"}, the maximum
#' size of the circles.
#' Defaults to \code{6}.
#' @param label whether to add correlation coefficients to the plot.
#' Defaults to \code{FALSE}.
#' @param label_alpha whether to make the correlation coefficients increasingly
#' transparent as they come close to 0. Also accepts any numeric value between
#' \code{0} and \code{1}, in which case the level of transparency is set to that
#' fixed value.
#' Defaults to \code{FALSE} (no transparency).
#' @param label_color the color of the correlation coefficients.
#' Defaults to \code{"grey75"}.
#' @param label_round the decimal rounding of the correlation coefficients.
#' Defaults to \code{1}.
#' @param nbreaks the number of breaks to apply to the correlation coefficients.
#' Defaults to \code{NULL} (no breaks, continuous scaling).
#' @param digits the number of digits to show in the breaks of the correlation
#' coefficients: see \code{\link[base]{cut}} for details.
#' Defaults to \code{2}.
#' @param drop whether to use the empirical range of the correlation
#' coefficients in the color scale, which is \emph{not} recommended (see
#' 'Details').
#' Defaults to \code{FALSE}.
#' @param low the lower color of the gradient for continuous scaling of the
#' correlation coefficients.
#' Defaults to \code{"#3B9AB2"} (blue).
#' @param mid the midpoint color of the gradient for continuous scaling of the
#' correlation coefficients.
#' Defaults to \code{"#EEEEEE} (very light grey).
#' @param high the upper color of the gradient for continuous scaling of the
#' correlation coefficients.
#' Defaults to \code{"#F21A00"} (red).
#' @param midpoint the midpoint value for continuous scaling of the
#' correlation coefficients.
#' Defaults to \code{0}.
#' @param limits whether to bound the continuous color scaling of the
#' correlation coefficients between -1 and +1.
#' Defaults to \code{TRUE} (recommended).
#' @param legend.position where to put the legend of the correlation
#' coefficients: see \code{\link[ggplot2]{theme}} for details.
#' @param legend.size the size of the legend title and labels, in points: see
#' \code{\link[ggplot2]{theme}} for details.
#' @param ... other arguments supplied to \code{\link[ggplot2]{geom_text}} for
#' the diagonal labels.
#' @details The \code{nbreaks} argument tries to break up the correlation
#' coefficients into an ordinal color scale. Recommended values for the numbers
#' of breaks are \code{3} to \code{11}, as values above 11 are visually
#' difficult to separate and are not supported by diverging ColorBrewer
#' palettes.
#'
#' The breaks will range from \code{-1} to \code{+1}, unless \code{drop} is set
#' to \code{FALSE}, in which case the empirical range of the correlation
#' coefficients is used. The latter is not recommended, as it creates a
#' disbalance between the colors of negative and positive coefficients.
#' @seealso \code{\link[stats]{cor}} and \code{\link[arm]{corrplot}}
#' @author Francois Briatte, with contributions from Amos B. Elberg and
#' Barret Schloerke
#' @importFrom reshape melt melt.data.frame melt.default
#' @examples
#' # Basketball statistics provided by Nathan Yau at Flowing Data.
#' dt <- read.csv("http://datasets.flowingdata.com/ppg2008.csv")
#'
#' # Default output.
#' ggcorr(dt[, -1])
#'
#' # Labelled output, with coefficient transparency.
#' ggcorr(dt[, -1],
#'        label = TRUE,
#'        label_alpha = TRUE)
#'
#' # Custom options.
#' ggcorr(
#'   dt[, -1],
#'   name = expression(rho),
#'   geom = "circle",
#'   max_size = 10,
#'   min_size = 2,
#'   size = 3,
#'   hjust = 0.75,
#'   nbreaks = 6,
#'   angle = -45,
#'   palette = "PuOr" # colorblind safe, photocopy-able
#' )
#'
#' # Supply your own correlation matrix
#' ggcorr(
#'   data = NULL,
#'   cor_matrix = cor(dt[, -1], use = "pairwise")
#' )
ggcorr <- function(
  data,
  method = c("pairwise", "pearson"),
  cor_matrix = NULL,
  nbreaks = NULL,
  digits = 2,
  name = "",
  geom = "tile",
  min_size = 2,
  max_size = 6,
  label = FALSE,
  label_alpha = FALSE,
  label_color = "black",
  label_round = 1,
  drop = FALSE,
  low = "#3B9AB2",  # (blue)  replaces "#d73027" (red)
  mid = "#EEEEEE",  # (grey) replaces "#ffffbf" (light yellow)
  high = "#F21A00", # (red)   replaces "#1a9850" (green)
  midpoint = 0,
  palette = NULL,
  limits = TRUE,
  legend.position = "right",
  legend.size = 9,
  ...) {

  # -- packages ----------------------------------------------------------------

  # require_pkgs("reshape2")

  # -- check geom argument -----------------------------------------------------

  if (length(geom) > 1 || !geom %in% c("blank", "circle", "text", "tile")) {
    stop("incorrect geom value")
  }

  # -- correlation method ------------------------------------------------------

  if (length(method) == 1) {
    method = c(method, "pearson") # for backwards compatibility
  }

  # -- check data columns ------------------------------------------------------

  if (!is.null(data)) {

    if (!is.data.frame(data)) {
      data = as.data.frame(data)
    }

    x = which(!sapply(data, is.numeric))

    if (length(x) > 0) {

      warning(paste("data in column(s)",
                    paste0(paste0("'", names(data)[x], "'"), collapse = ", "),
                    "are not numeric and were ignored"))

      data = data[, -x ]

    }

  }

  # -- correlation matrix ------------------------------------------------------

  if (is.null(cor_matrix)) {
    cor_matrix = cor(data, use = method[1], method = method[2])
  }

  m = cor_matrix
  colnames(m) = rownames(m) = gsub(" ", "_", colnames(m)) # protect spaces

  # -- correlation data.frame --------------------------------------------------

  m = data.frame(m * lower.tri(m))
  m$.row.names. = rownames(m)
  m = melt(m, id.vars = ".row.names.")
  names(m) = c("x", "y", "c")
  m$c[ m$c == 0 ] = NA

  # -- correlation quantiles ---------------------------------------------------

  if (!is.null(nbreaks)) {

    s = seq(-1, 1, length.out = nbreaks + 1)

    if (!nbreaks %% 2) {
      s = unique(sort(c(s, 0)))
    }

    m$b = cut(m$c, breaks = s, include.lowest = TRUE, dig.lab = digits)

  }

  # -- gradient midpoint -------------------------------------------------------

  if (is.null(midpoint)) {

    midpoint = median(m$c, na.rm = TRUE)
    message(paste("Color gradient midpoint set at median correlation to",
                  round(midpoint, 2)))

  }

  # -- plot structure ----------------------------------------------------------

  p = ggplot(na.omit(m), aes(x, y))

  if (geom == "tile") {

    if (is.null(m$b)) {

      # -- tiles, continuous ---------------------------------------------------

      p = p +
        geom_tile(aes(fill = c), color = "white")

    } else {

      # -- tiles, ordinal ------------------------------------------------------

      p = p +
        geom_tile(aes(fill = b), color = "white")

    }

    # -- tiles, color scale ----------------------------------------------------

    if (is.null(m$b) && limits) {

      p = p +
        scale_fill_gradient2(name, low = low, mid = mid, high = high,
                             midpoint = midpoint, limits = c(-1, 1))

    } else if (is.null(m$b)) {

      p = p +
        scale_fill_gradient2(name, low = low, mid = mid, high = high,
                             midpoint = midpoint)

    } else if (is.null(palette)) {

      x = colorRampPalette(c(low, mid, high))(length(levels(m$b)))

      p = p +
        scale_fill_manual(name, values = x, drop = drop)

    } else {

      p = p +
        scale_fill_brewer(name, palette = palette, drop = drop)

    }

  } else if (geom == "circle") {

    p = p +
      geom_point(aes(size = abs(c) * 1.25), color = "grey50") # grey border

    if (is.null(m$b)) {

      # -- circles, continuous -------------------------------------------------

      p = p +
        geom_point(aes(size = abs(c), color = c))

    } else {

      # -- circles, ordinal ----------------------------------------------------

      p = p +
        geom_point(aes(size = abs(c), color = b))

    }

    p = p +
      scale_size_continuous(range = c(min_size, max_size)) +
      guides(size = FALSE)

    r = list(size = (min_size + max_size) / 2)

    # -- circles, color scale --------------------------------------------------

    if (is.null(m$b) && limits) {

      p = p +
        scale_color_gradient2(name, low = low, mid = mid, high = high,
                              midpoint = midpoint, limits = c(-1, 1))

    } else if (is.null(m$b)) {

      p = p +
        scale_color_gradient2(name, low = low, mid = mid, high = high,
                              midpoint = midpoint)

    } else if (is.null(palette)) {

      x = colorRampPalette(c(low, mid, high))(length(levels(m$b)))

      p = p +
        scale_color_manual(name, values = x, drop = drop) +
        guides(color = guide_legend(override.aes = r))

    } else {

      p = p +
        scale_color_brewer(name, palette = palette, drop = drop) +
        guides(color = guide_legend(override.aes = r))

    }

  } else if (geom == "text") {

    if (is.null(m$b)) {

      # -- text, continuous ----------------------------------------------------

      p = p +
        geom_text(aes(label = round(c, label_round), color = c))

    } else {

      # -- text, ordinal -------------------------------------------------------

      p = p +
        geom_text(aes(label = round(c, label_round), color = b))

    }

    # -- text, color scale ----------------------------------------------------

    if (is.null(m$b) && limits) {

      p = p +
        scale_color_gradient2(name, low = low, mid = mid, high = high,
                              midpoint = midpoint, limits = c(-1, 1))

    } else if (is.null(m$b)) {

      p = p +
        scale_color_gradient2(name, low = low, mid = mid, high = high,
                              midpoint = midpoint)

    } else if (is.null(palette)) {

      x = colorRampPalette(c(low, mid, high))(length(levels(m$b)))

      p = p +
        scale_color_manual(name, values = x, drop = drop)

    } else {

      p = p +
        scale_color_brewer(name, palette = palette, drop = drop)

    }

  }

  # -- coefficient labels ------------------------------------------------------

  if (label) {

    m = na.omit(m)
    m$c = round(m$c, label_round)

    if (isTRUE(label_alpha)) {

      p = p +
        geom_text(data = m,
                  aes(x, y, label = c, alpha = abs(c)),
                  color = label_color,
                  show_guide = FALSE)

    } else if (label_alpha > 0) {

      p = p +
        geom_text(data = m,
                  aes(x, y, label = c,
                      alpha = label_alpha, color = label_color,
                      show_guide = FALSE))

    } else {

      p = p +
        geom_text(data = m,
                  aes(x, y, label = c),
                  color = label_color)

    }

  }

  p = p  +
    geom_text(data = m[ m$x == m$y & is.na(m$c), ], aes(label = x), ...) +
    scale_x_discrete(breaks = NULL, limits = levels(m$y)) +
    scale_y_discrete(breaks = NULL, limits = levels(m$y)) +
    labs(x = NULL, y = NULL) +
    coord_equal() +
    theme(
      panel.background = element_blank(),
      legend.key = element_blank(),
      legend.position = legend.position,
      legend.title = element_text(size = legend.size),
      legend.text = element_text(size = legend.size)
    )

  return(p)

}
