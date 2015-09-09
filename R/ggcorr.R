
if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("variable", "value", "num"))
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
#' @param data a data frame or matrix containing numeric (continuous) data.
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
#' @param geom the geom object to use. Accepts either \code{tile} (the default)
#' or \code{circle}, to plot proportionally scaled circles.
#' @param max_size when \code{geom} has been set to \code{"circle"}, the maximum
#' size of the circles, as passed to \code{scale_size_identity} for proportional
#' scaling.
#' Defaults to \code{6}.
#' @param min_size when \code{geom} has been set to \code{"circle"}, the minimum
#' size of the circles, as passed to \code{scale_size_identity} for proportional
#' scaling.
#' Defaults to \code{2}.
#' @param label whether to add correlation coefficients as two-digit numbers
#' over the plot.
#' Defaults to \code{FALSE}.
#' @param label_alpha whether to make the correlation coefficients increasingly
#' transparent as they come close to 0.
#' Defaults to \code{FALSE}.
#' @param label_color the color of the correlation coefficients.
#' Defaults to \code{"black"}.
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
#' Defaults to \code{"#FFFFFF} (white).
#' @param high the upper color of the gradient for continuous scaling of the
#' correlation coefficients.
#' Defaults to \code{"#F21A00"} (red).
#' @param midpoint the midpoint value for continuous scaling of the
#' correlation coefficients.
#' Defaults to \code{0}.
#' @param limits whether to bound the continuous color scaling of the
#' correlation coefficients between -1 and +1.
#' Defaults to \code{TRUE} (recommended).
#' @param ... other arguments supplied to \code{\link[ggplot2]{geom_text}} for
#' the diagonal labels.
#' @details The \code{nbreaks} argument tries to break up the correlation
#' coefficients into an ordinal color scale. Recommended values for the numbers
#' of breaks are \code{3} to \code{11}, as values above 11 are visually
#' difficult to separate and are not supported by diverging ColorBrewer
#' palettes.
#'
#' The breaks will range from -1 to +1, unless drop is set to \code{FALSE}, in
#' which case the empirical range of the correlation coefficients is used. The
#' latter is not recommended, as it creates a disbalance between the colors of
#' negative and positive coefficients.
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
  palette = "RdYlGn",
  name = "",
  geom = "tile",
  min_size = 2,
  max_size = 6,
  label = FALSE,
  label_alpha = FALSE,
  label_color = "black",
  label_round = 1,
  nbreaks = NULL,
  digits = 2,
  drop = FALSE,
  low = "#3B9AB2",  # (blue)  replaces "#d73027" (red)
  mid = "#FFFFFF",  # (white) replaces "#ffffbf" (light yellow)
  high = "#F21A00", # (red)   replaces "#1a9850" (green)
  midpoint = 0,
  limits = TRUE,
  ...) {

  # for backwards compatibility
  if (length(method) == 1) {
    method = c(method, "pearson")
  }

  if (is.null(cor_matrix)) {
    cor_matrix = cor(data, use = method[1], method = method[2])
  }

  if (!is.null(data) && "data.frame" %in% class(data)) {
    r = names(data)[ !sapply(data, is.numeric) ]
  } else if (!is.null(data) && !is.null(colnames(data))) {
    r = colnames(data)[ !apply(data, 2, is.numeric) ]
  } else {
    r = which(!apply(data, 2, is.numeric))
  }

  if (length(r) > 0) {
    stop(paste("column(s)", paste0(r, collapse = ", "), "are not numeric"))
  }

  M = cor_matrix

  # protect against spaces in variable names
  colnames(M) = rownames(M) = gsub(" ", "_", colnames(M))

  # correlation coefficients
  D = round(M, label_round)

  D = D * lower.tri(D)
  D = as.data.frame(D)

  r = names(D)
  D = data.frame(row = r, D)
  D = melt(D, id.vars = "row")

  # correlation quantiles
  M = M * lower.tri(M)
  M = as.data.frame(M)
  M = data.frame(row = r, M)
  M = melt(M, id.vars = "row")
  M$value[ M$value == 0 ] = NA

  if(!is.null(nbreaks)) {

    s = seq(-1, 1, length.out = nbreaks + 1)

    if(!nbreaks %% 2)
      s = unique(sort(c(s, 0)))

    M$value = cut(M$value, breaks = s, include.lowest = TRUE, dig.lab = digits)
    M$value = droplevels(M$value)
    M$value = factor(M$value, levels = unique(cut(s, breaks = s, dig.lab = digits, include.lowest = TRUE)))

  }

  if(is.null(midpoint)) {

    midpoint = median(M$value, na.rm = TRUE)
    message(paste("Color gradient midpoint set at median correlation to",
                  round(midpoint, 2)))

  }

  M$row = factor(M$row, levels = unique(as.character(M$variable)))

  if (geom == "circle") {

    M$num = as.numeric(M$value)
    M$num = abs(M$num - median(unique(M$num), na.rm = TRUE))
    M$num = as.numeric(factor(M$num))
    M$num = seq(min_size, max_size, length.out = length(na.omit(unique(M$num))))[ M$num ]

  }

  diag  = subset(M, row == variable)
  M = M[ complete.cases(M), ]

  p = ggplot(M, aes(x = row, y = variable))

  # apply main geom
  if (geom == "circle") {

    p = p +
      #geom_point(aes(size = num + 0.25), color = "grey50") +
      geom_point(aes(size = num, color = value))

    if (is.null(nbreaks) && limits) {

      p = p +
        scale_size_continuous(range = c(min_size, max_size)) +
        scale_color_gradient2(name, low = low, mid = mid, high = high,
                              midpoint = midpoint, limits = c(-1, 1)) +
        guides(size = FALSE)

    } else if (is.null(nbreaks)) {

      p = p +
        scale_size_continuous(range = c(min_size, max_size)) +
        scale_fill_gradient2(name, low = low, mid = mid, high = high,
                             midpoint = midpoint) +
        guides(size = FALSE)

    } else {

      r = list(size = (min_size + max_size) / 2)
      p = p +
        scale_size_identity(name) +
        scale_color_brewer(name, palette = palette, drop = drop) +
        guides(colour = guide_legend(name, override.aes = r))

    }

  } else {

    p = p +
      geom_tile(aes(fill = value), colour = "white")

    if (is.null(nbreaks) & limits) {

      p = p +
        scale_fill_gradient2(name, low = low, mid = mid, high = high,
                             midpoint = midpoint, limits = c(-1, 1))

    } else if (is.null(nbreaks)) {

      p = p +
        scale_fill_gradient2(name, low = low, mid = mid, high = high,
                             midpoint = midpoint)

    } else {

      p = p +
        scale_fill_brewer(name, palette = palette, drop = drop)

    }

  }

  # add coefficient text
  if(label) {

    if(label_alpha) {

      p = p +
        geom_text(data = subset(D, value != 0),
                  aes(row, variable, label = value, alpha = abs(as.numeric(value))),
                  color = label_color, show_guide = FALSE)

    } else {

      p = p +
        geom_text(data = subset(D, value != 0),
                  aes(row, variable, label = value),
                  color = label_color)

    }
  }

  # add diagonal and options
  p = p  +
    geom_text(data = diag, aes(label = variable), ...) +
    scale_x_discrete(breaks = NULL, limits = levels(M$row)) +
    scale_y_discrete(breaks = NULL, limits = levels(M$variable)) +
    labs(x = NULL, y = NULL) +
    coord_equal() +
    theme(
      panel.background = element_blank(),
      panel.grid = element_blank(),
      legend.key = element_blank()#,
      #axis.text.x = element_text(angle = -90)
    )

  return(p)

}
