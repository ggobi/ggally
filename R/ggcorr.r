

if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("variable", "value", "num"))
}

#' ggcorr - Plot a correlation matrix with ggplot2
#'
#' Function for making a correlation plot starting from a data matrix, using ggplot2. The function is directly inspired by Tian Zheng and Yu-Sung Su's arm::corrplot function. Please visit \url{http://github.com/briatte/ggcorr} for the latest development and descriptions about ggcorr.
#'
#' @export
#' @param data a data matrix. Should contain numerical (continuous) data.
#' @param method a character string giving a method for computing covariances in the presence of missing values. This must be (an abbreviation of) one of the strings \code{"everything"}, \code{"all.obs"}, \code{"complete.obs"}, \code{"na.or.complete"}, or \code{"pairwise.complete.obs"}.
#' Defaults to \code{"pairwise"}.
#' @param cor_matrix the named correlation matrix to use for calculations.
#' @param palette if \code{nbreaks} has been set to something, a ColorBrewer
#' palette to be used for correlation coefficients. Defaults to \code{"RdYlGn"}.
#' @param name a character string for the legend that shows quintiles of
#' correlation coefficients. Defaults to nothing.
#' @param geom the geom object to use. Accepts either \code{tile} (the default)
#' or \code{circle}, to plot proportionally scaled circles.
#' @param max_size the maximum size for circles, as passed to \code{scale_size_identity}
#' for proportional scaling. Defaults to \code{6}.
#' @param min_size the maximum size for circles, as passed to \code{scale_size_identity}
#' for proportional scaling. Defaults to \code{2}.
#' @param label whether to add correlation coefficients as two-digit numbers
#' over the plot. Defaults to \code{FALSE}.
#' @param label_alpha whether to make the correlation coefficients transparent
#' as they come close to 0. Defaults to \code{FALSE}.
#' @param label_color color for the correlation coefficients. Defaults to \code{"black"}.
#' @param label_round decimal rounding of the correlation coefficients.
#' Defaults to \code{1}.
#' @param nbreaks number of breaks to apply to categorize the correlation
#' coefficients. Defaults to \code{NULL} (continuous scaling).
#' @param digits number of digits to show in the breaks of the correlation
#' coefficients. Defaults to \code{2}.
#' @param drop use the empirical range of correlation coefficients for their categorization,
#' which is \emph{not} recommended (see 'Details'). Defaults to \code{FALSE}.
#' @param low lower color of the gradient for continuous scaling of the correlation
#' coefficients. Defaults to \code{d73027} (red).
#' @param mid mid color of the gradient for continuous scaling of the correlation
#' coefficients. Defaults to \code{d73027} (yellow).
#' @param high upper color of the gradient for continuous scaling of the correlation
#' coefficients. Defaults to \code{1a9850} (red).
#' @param midpoint the midpoint value for continuous scaling of the correlation
#' coefficients. Defaults to \code{0}.
#' @param limits whether to bound the continuous color scaling of the correlation
#' coefficients between -1 and +1. Defaults to \code{TRUE}.
#' @param ... other arguments supplied to geom_text for the diagonal labels.
#' Arguments pertaining to the title or other items can be achieved through ggplot2 methods.
#' @details The \code{nbreaks} argument tries to break up the correlation
#' coefficients into an ordinal color scale. Recommended values for the numbers
#' of breaks are 3 to 11, as values above 11 are visually difficult to separate
#' and are not supported by diverging ColorBrewer palettes.
#'
#' The breaks will range from -1 to +1, unless drop is set to \code{FALSE}, in
#' which case the empirical range of the correlation coefficients is used. The
#' latter is not recommended, as it creates a disbalance between the colors of
#' negative and positive coefficients.
#' @seealso \code{\link{cor}} and \code{\link[arm]{corrplot}}
#' @author Francois Briatte \email{f.briatte@@gmail.com} with contributions from
#' Amos B. Elberg \email{amos.elberg@@gmail.com} and Barret Schloerke \email{schloerke@gmail.com}
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
#' ) + ggplot2::labs(title = "Correlation Matrix")
#'
#' # Supply your own correlation matrix
#' ggcorr(
#'   data = NULL,
#'   cor_matrix = cor(dt[,-1], use = "pairwise")
#' )

ggcorr <- function(
  data,
  method = "pairwise",
  cor_matrix = cor(data, use = method),
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
  low = "#d73027",
  mid = "#ffffbf",
  high = "#1a9850",
  midpoint = 0,
  limits = TRUE,
  ...) {

  M <- cor_matrix
  
  # protect against spaces in variable names
  colnames(M) = rownames(M) = gsub(" ", "_", colnames(M))

  # correlation coefficients
  D <- round(M, label_round)

  D <- D * lower.tri(D)
  D <- as.data.frame(D)

  rowNames <- names(D)
  D <- data.frame(row = rowNames, D)
  D <- melt(D, id.vars = "row")

  # correlation quantiles
  M <- M * lower.tri(M)
  M <- as.data.frame(M)
  M <- data.frame(row = rowNames, M)
  M <- melt(M, id.vars = "row")
  M$value[M$value == 0] <- NA

  if(!is.null(nbreaks)) {

    s = seq(-1, 1, length.out = nbreaks + 1)

    if(!nbreaks %% 2)
      s = unique(sort(c(s, 0)))

    M$value = droplevels(cut(M$value, breaks = s, include.lowest = TRUE, dig.lab = digits))
    M$value = factor(M$value, levels = unique(cut(s, breaks = s, dig.lab = digits, include.lowest = TRUE)))

  }

  if(is.null(midpoint)) {

    midpoint = median(M$value, na.rm = TRUE)
    message("Color gradient midpoint set at median correlation to ", round(midpoint, 2))

  }

  M$row <- factor(M$row, levels = unique(as.character(M$variable)))

  # for circles
  M$num = as.numeric(M$value)
  M$num = abs(M$num - median(unique(M$num), na.rm = TRUE))
  M$num = as.numeric(factor(M$num))
  M$num = seq(min_size, max_size, length.out = length(na.omit(unique(M$num))))[ M$num ]

  diag  <- subset(M, row == variable)
  M <- M[complete.cases(M), ]

  # clean plot panel
  po.nopanel <- list(theme(
    panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.key = element_blank(),
    axis.text.x = element_text(angle = -90))
  )

  p = ggplot(M, aes(x = row, y = variable))

  # apply main geom
  if(geom == "circle") {

    p = p +
      geom_point(aes(size = num + 0.25), color = "grey50") +
      geom_point(aes(size = num, color = value))

    if(is.null(nbreaks) & limits)
      p = p +
        scale_size_continuous(range = c(min_size, max_size)) +
        scale_color_gradient2(name, low = low, mid = mid, high = high, midpoint = midpoint,
                              limits = c(-1, 1)) +
        guides(size = FALSE)
    else if(is.null(nbreaks))
      p = p +
        scale_size_continuous(range = c(min_size, max_size)) +
        scale_fill_gradient2(name, low = low, mid = mid, high = high, midpoint = midpoint) +
        guide(size = FALSE)
    else
      p = p +
        scale_size_identity(name) +
        scale_color_brewer(name, palette = palette, drop = drop) +
        guides(colour = guide_legend(name, override.aes = list(size = (min_size + max_size) / 2)))

  }
  else {

    p = p + geom_tile(aes(fill = value), colour = "white")

    if(is.null(nbreaks) & limits)
      p = p + scale_fill_gradient2(name, low = low, mid = mid, high = high, midpoint = midpoint,
                                   limits = c(-1, 1))
    else if(is.null(nbreaks))
      p = p + scale_fill_gradient2(name, low = low, mid = mid, high = high, midpoint = midpoint)
    else
      p = p + scale_fill_brewer(name, palette = palette, drop = drop)

  }

  # add coefficient text
  if(label) {
    if(label_alpha) {
      p = p +
        geom_text(data = subset(D, value != 0),
                  aes(row, variable, label = value, alpha = abs(as.numeric(value))),
                  color = label_color, show_guide = FALSE)
    }
    else {
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
    po.nopanel

  return(p)
}
