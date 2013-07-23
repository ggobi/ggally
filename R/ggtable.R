#' ggtable - quick bar plots with ggplot2
#'
#' Function to plot fequencies and crosstabulations as horizontal bars or stacked bars, using raw counts or percentages. Please visit \link{http://github.com/briatte/ggtable} for the latest development and descriptions about ggtable.
#'
#' @export
#' @param x, y vectors to plot. If a vector \code{y} is provided, the function returns the stacked bars corresponding to \code{table(x, y)}.
#' @param percent whether to use percentages instead of raw counts. Defaults to \code{FALSE}.
#' @param name when a vector \code{y} is provided, the title of its colour legend.
#' @param order whether to sort the levels of vector \code{x} by decreasing order of frequency. Defaults to \code{TRUE}.
#' @param order.x the order in which to plot the levels of vector \code{x}. Overrules \code{order}.
#' @param order.y the order in which to plot the levels of vector \code{y}.
#' @param label whether to add text labels to the bars. Defaults to \code{FALSE}.
#' @param hjust horizontal adjustment for the bar labels. Defaults to \code{2}.
#' @param color color for the bar labels. Defaults to \code{grey90} (quasi-white).
#' @param append a single character string to append to the bar labels.
#' @param verbose whether to return messages on the plot construction. Defaults to \code{FALSE}.
#' @param palette when a vector \code{y} is provided, the name of a ColorBrewer palette to use to color the bars.
#' @param position when a vector \code{y} is provided, the position of the bars split by \code{y}. Defaults to \code{"stack"}.
#' @param legend.position location of the legend for bar colors and weights. Accepts all positions supported by ggplot2 themes. Defaults to \code{"right"}.
#' @param ... other arguments supplied to \code{geom_text} for the bar labels. Arguments pertaining to the title or other items can be achieved through ggplot2 methods, at the exception of facet methods.
#' @seealso \code{\link[likert]{plot.likert}} in the \link[likert]{likert} package
#' @author François Briatte \email{f.briatte@@ed.ac.uk}
#' @examples
#' ## Examples using the questionr package.
#' require(devtools)
#' install_github("questionr", "briatte")
#' require(questionr)
#' data(hdv2003) 
#' ## One-way tables
#' freq(x <- recode.na(hdv2003$relig, "NSP ou NVPR|Rejet"))
#' ggtable(x)
#' ggtable(x, order = FALSE, label = TRUE)
#' ggtable(x, percent = TRUE, label = TRUE, append = " %")
#' ## Two-way tables
#' freq(y <- hdv2003$sexe)
#' ggtable(x, y)
#' ggtable(x, y, percent = TRUE)
#' ggtable(y, x, order.y = c("Homme", "Femme"))
#' ## Using options.
#' ggtable(y, x, order = c("Homme", "Femme"), horizontal = FALSE, 
#'   percent = TRUE, position = "dodge", legend.position = "bottom")
#' Cleaner theme.
#' ggtable(hdv2003$nivetud, x, percent = TRUE, palette = "RdGy") + 
#'   theme_bw(16) +
#'   theme(legend.position = "top", axis.ticks = element_blank())

ggtable <- function(x, y = NULL, percent = FALSE, name = "",
                    order.x = TRUE, order.y = FALSE, order = order.x,
                    label = FALSE, hjust = 2, color = "grey90", append = "", 
                    verbose = FALSE, palette = NULL, horizontal = TRUE, 
                    position = "stack", legend.position = "right", ...) {
  require(plyr)
  # get table
  t = table(x)
  if(!is.null(y)) t = table(x, y)
  if (percent & is.null(y)) t = 100 * prop.table(t)
  if (percent & !is.null(y)) t = 100 * prop.table(t, 1)
  
  # reset any levels
  t = data.frame(t, stringsAsFactors = FALSE)
  
  # restore existent 
  if (!is.null(levels(x))) {
    t$x = factor(as.character(t$x), levels = levels(x))    
  }
  # optional reorder
  if (isTRUE(order)) {
    if (verbose) message("Ordered by frequency of x.")
    t$x = with(t, reorder(x, Freq, mean))
  }  
  # optional levels(x)
  l = all(unique(na.omit(order)) %in% unique(na.omit(levels(t$x))))
  if (class(order) == "character" & l) {
    if (verbose) message("Using custom levels for x.")
    if (horizontal) order = rev(order)
    t$x = factor(t$x, levels = order)
  }
  # optional levels(y)
  l = all(unique(na.omit(order.y)) %in% unique(na.omit(levels(t$y))))
  if (class(order.y) == "character" & l) {
    if (verbose) message("Using custom levels for y.")
    if (horizontal) order.y = rev(order.y)
    t$y = factor(t$y, levels = order.y)
  }

  # one-way horizontal bars
  if (is.null(y)) {
    g = qplot(data = t, x = x, y = Freq, stat = "identity", geom = "bar") + 
      scale_y_continuous(breaks = seq(0, max(t$Freq),
                                      2*10^(round(log10(max(t$Freq))) - 1)))    
    if (label) g = g + 
      geom_text(aes(label = paste0(round(Freq, 0), append)), hjust = hjust, color = color, ...)
  }
  # two-way horizontal bars
  else {
    pos = ddply(t, .(x), transform, ycoord = cumsum(Freq) - 0.5 * Freq)
    g = qplot(data = t, x = x, y = Freq, fill = y,
              stat = "identity", position = position, geom = "bar")
    if (length(palette) > 0)
      g = g +
      scale_fill_brewer(name, palette = palette)
    else
      g = g +
      scale_fill_discrete(name)
    if (percent)
      g = g + 
      scale_y_continuous(breaks = 20*0:5)
    else
      g = g +
      scale_y_continuous(breaks = seq(0, sum(t$Freq),
                                      2*10^(round(log10(sum(t$Freq))) - 1)))
    if (label) g = g +
      geom_text(data = pos,
                aes(label = paste0(round(Freq, 0), append), x = x, y = ycoord), 
                color = color, ...)
  }
  if (horizontal) g = g + coord_flip()
  g = g + labs(y = NULL, x = NULL) + theme(legend.position = legend.position)
  return(g)
}
