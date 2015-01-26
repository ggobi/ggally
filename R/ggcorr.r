

if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("variable", "value", "num"))
}

#' ggcorr - Plot a correlation matrix with ggplot2
#'
#' Function for making a correlation plot starting from a data matrix, using ggplot2. The function is directly inspired by Tian Zheng and Yu-Sung Su's arm::corrplot function. Please visit \url{http://github.com/briatte/ggcorr} for the latest development and descriptions about ggcorr.
#'
#' @export
#' @param data a data matrix. Should contain numerical (continuous) data.
#' @param method a character string giving a method for computing covariances in the presence of missing values. This must be (an abbreviation of) one of the strings \code{"everything"}, \code{"all.obs"}, \code{"complete.obs"}, \code{"na.or.complete"}, or \code{"pairwise.complete.obs"}. Defaults to \code{"pairwise"}.
#' @param palette a ColorBrewer palette to be used for correlation coefficients. Defaults to \code{"RdYlGn"}.
#' @param name a character string for the legend that shows quintiles of correlation coefficients.
#' @param geom the geom object to use. Accepts either \code{tile} (the default) or \code{circle}, to plot proportionally scaled circles.
#' @param max_size the maximum size for circles, as passed to \code{scale_size_area} for proportional scaling. Defaults to \code{6}.
#' @param label whether to add correlation coefficients as two-digit numbers over the plot. Defaults to \code{FALSE}.
#' @param label_alpha whether to make the correlation coefficients transparent as they come close to 0. Defaults to \code{FALSE}.
#' @param label_color color for the correlation coefficients. Defaults to \code{"black"}.
#' @param label_round decimal rounding of the correlation coefficients. Defaults to \code{1}.
#' @param nbreaks number of breaks to apply.  Defaults to \code{8}.
#' @param ... other arguments supplied to geom_text for the diagonal labels.  Arguments pertaining to the title or other items can be achieved through ggplot2 methods.
#' @seealso \code{\link{cor}} and \code{\link[arm]{corrplot}}
#' @author Francois Briatte \email{f.briatte@@gmail.com} with contributions from Amos B. Elberg \email{amos.elberg@@gmail.com}
#' @importFrom reshape melt melt.data.frame melt.default
#' @examples
#' # Basketball statistics provided by Nathan Yau at Flowing Data.
#' nba <- read.csv("http://datasets.flowingdata.com/ppg2008.csv")
#' # Default output.
#' ggcorr(nba[, -1])
#' # Labelled output, with coefficient transparency.
#' ggcorr(nba[, -1],
#'        label = TRUE,
#'        label_alpha = TRUE,
#'        name = "") +
#'   ggplot2::theme(legend.position = "bottom")
#' # Custom options.
#' ggcorr(
#'   nba[, -1],
#'   name = expression(rho),
#'   geom = "circle",
#'   max_size = 6,
#'   size = 3,
#'   hjust = 0.75,
#'   nbreaks = 6,
#'   angle = -45,
#'   palette = "PuOr" # colorblind safe, photocopy-able
#' ) + ggplot2::labs(title = "Points Per Game")
ggcorr <- function(data,
  method = "pairwise",
  palette = "RdYlGn",
  name = "rho",
  geom = "tile",
  min_size = 2,
  max_size = 6,
  label = FALSE,
  label_alpha = FALSE,
  label_color = "black",
  label_round = 1,
  nbreaks = 8,
  ...) {

  M <- cor(data[1:ncol(data)], use = method)

  # correlation coefficients
  D <- round(M, label_round)
  D <- D * lower.tri(D)
  D <- as.data.frame(D)
  D <- data.frame(row = names(data), D)
  D <- melt(D, id.vars = "row")

  # correlation quantiles
  M <- M * lower.tri(M)
  M <- as.data.frame(M)
  M <- data.frame(row = names(data), M)
  M <- melt(M, id.vars = "row")
  M$value[M$value == 0] <- NA
  s <- seq(-1, 1, length.out = nbreaks + 1)
  M$value <- droplevels(cut(M$value, breaks = s, include.lowest = TRUE,
                 label = cut(s, breaks = s)[-1]))
  M$row <- factor(M$row, levels = unique(as.character(M$variable)))
  M$num <- as.numeric(M$value)
  M$num <- abs(M$num - (nbreaks - 1)/2) / (nbreaks/2) * (max_size - min_size) + min_size

  diag  <- subset(M, row == variable)

  # clean plot panel
  po.nopanel <- list(theme(
    panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.x = element_text(angle = -90))
  )

  p = ggplot(M, aes(row, variable))
  g <- guide_legend(name)

  # apply main geom
  if(geom == "circle") {
    p = p +
      scale_colour_brewer(name, palette = palette) +
      scale_size_continuous(name, range = c(max_size/nlevels(M$value), max_size), labels =
      												levels(M$value) ) +
      geom_point(aes(size = num, colour = value)) +
			guides(color = g, size = g)
  }
  else {
    p = p +
      scale_fill_brewer(palette = palette) +
      geom_tile(aes(fill = value), colour = "white") +
      guides(color = g)
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
    scale_x_discrete(breaks = NULL) +
    scale_y_discrete(breaks = NULL) +
    labs(x = NULL, y = NULL) +
    coord_equal() +
    po.nopanel

  return(p)
}
