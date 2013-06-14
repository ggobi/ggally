#' ggcorr - Plot a correlation matrix with ggplot2
#'
#' Function for making a correlation plot starting from a data matrix, using ggplot2. The function is directly inspired by Tian Zheng and Yu-Sung Su's arm:corrplot function.
#'
#' @export
#' @param data a data matrix. Should contain numerical (continuous) data.
#' @param method a character string giving a method for computing covariances in the presence of missing values. This must be (an abbreviation of) one of the strings \code{"everything"}, \code{"all.obs"}, \code{"complete.obs"}, \code{"na.or.complete"}, or \code{"pairwise.complete.obs"}. Defaults to \code{"pairwise"}.
#' @param palette a ColorBrewer palette to be used for correlation coefficients. Defaults to \code{"RdYlGn"}.
#' @param ... other arguments supplied to geom_text for the diagonal labels.  Arguments pertaining to the title or other items can be achieved through ggplot2 methods.
#' @seealso \code{\link{cor}} and \code{\link[arm]{corrplot}}
#' @author Francois Briatte \email{f.briatte@@ed.ac.uk}
#' @examples
#' # Basketball statistics provided by Nathan Yau at Flowing Data.
#' nba <- read.csv("http://datasets.flowingdata.com/ppg2008.csv")
#' ggcorr(nba[-1])
#' ggcorr(
#'   nba[-1],
#'   size = 3,
#'   hjust = 0.75,
#'   angle = -45,
#'   palette = "PuOr" # colorblind safe, photocopy-able
#' ) + labs(title = "Points Per Game")
ggcorr <- function(data, method = "pairwise", palette = "RdYlGn", ...) {

  M <- cor(data[1:ncol(data)], use = method)
  M <- M * lower.tri(M)
  M <- as.data.frame(M)
  M <- data.frame(row = names(data), M)
  M <- melt(M)
  M$value[M$value == 0] <- NA
  s <- seq(-1, 1, by = .25)
  M$value <- cut(M$value, breaks = s, include.lowest = TRUE,
                 label = cut(s, breaks = s)[-1])
  M$row <- factor(M$row, levels = (unique(as.character(M$variable))))
  diag <- subset(M, row == variable)

  po.nopanel <- list(theme(
    panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.x = element_text(angle = -90))
  )

  ggplot(M, aes(row, variable)) +
    scale_fill_brewer(palette = palette, name = "Correlation\ncoefficient") +
    geom_tile(aes(fill = value), colour = "white") +
    geom_text(data = diag, aes(label = variable), ...) +
    scale_x_discrete(breaks = NULL) +
    scale_y_discrete(breaks = NULL) +
    labs(x = "", y = "") +
    po.nopanel
}
