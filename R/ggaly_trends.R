#' Trends line plot
#'
#' Plot trends using line plots.
#' For continuous y variables, plot the evolution of the mean.
#' For binary y variables, plot the evolution of the proportion.
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... other arguments passed to [ggplot2::geom_line()]
#' @param include_zero Should 0 be included on the y-axis?
#' @author Joseph Larmarange
#' @keywords hplot
#' @export
#' @examples
#' # Small function to display plots only if it's interactive
#' p_ <- GGally::print_if_interactive
#'
#' data(tips)
#' tips_f <- tips
#' tips_f$day <- factor(tips$day, c("Thur", "Fri", "Sat", "Sun"))
#'
#' # Numeric variable
#' p_(ggally_trends(tips_f, mapping = aes(x = day, y = total_bill)))
#' p_(ggally_trends(tips_f, mapping = aes(x = day, y = total_bill, colour = time)))
#'
#' # Binary variable
#' p_(ggally_trends(tips_f, mapping = aes(x = day, y = smoker)))
#' p_(ggally_trends(tips_f, mapping = aes(x = day, y = smoker, colour = sex)))
#'
#' # Discrete variable with 3 or more categories
#' p_(ggally_trends(tips_f, mapping = aes(x = smoker, y = day)))
#' p_(ggally_trends(tips_f, mapping = aes(x = smoker, y = day, color = sex)))
#'
#' # Include zero on Y axis
#' p_(ggally_trends(tips_f, mapping = aes(x = day, y = total_bill), include_zero = TRUE))
#' p_(ggally_trends(tips_f, mapping = aes(x = day, y = smoker), include_zero = TRUE))
#'
#' # Change line size
#' p_(ggally_trends(tips_f, mapping = aes(x = day, y = smoker, colour = sex), size = 3))
#'
#' # Define weights with the appropriate aesthetic
#' d <- as.data.frame(Titanic)
#' p_(ggally_trends(
#'   d,
#'   mapping = aes(x = Class, y = Survived, weight = Freq, color = Sex),
#'   include_zero = TRUE
#' ))
ggally_trends <- function(
  data,
  mapping,
  ...,
  include_zero = FALSE
) {
  if (is.null(mapping$x)) stop("'x' aesthetic is required.")
  if (is.null(mapping$y)) stop("'y' aesthetic is required.")

  # computing group
  g <- list()
  if (!is.null(mapping$alpha))
    g <- append(g, list(eval_data_col(data, mapping$alpha)))
  if (!is.null(mapping$colour))
    g <- append(g, list(eval_data_col(data, mapping$colour)))
  if (!is.null(mapping$linetype))
    g <- append(g, list(eval_data_col(data, mapping$linetype)))
  if (!is.null(mapping$size))
    g <- append(g, list(eval_data_col(data, mapping$size)))
  if (length(g) == 0) {
    mapping$group <- aes(group = 1)$group
  } else {
    data$.group <- interaction(g)
    mapping$group <- aes(group = !!as.name(".group"))$group
  }

  # considering the different situations regarding y
  y <- eval_data_col(data, mapping$y)
  if (is.factor(y) || is.character(y) || is.logical(y)) {
    y <- as.factor(y)
    if (length(levels(y)) == 2) {
      # Binary variable
      data[[".ggally_y"]] <- as.integer(y == levels(y)[2])
      mapping$y <- aes(y = !!as.name(".ggally_y"))$y
      p <- ggplot(data, mapping) +
        stat_weighted_mean(geom = "line", ...) +
        scale_y_continuous(labels = scales::label_percent()) +
        ylab("")
    } else {
      # 3 or more categories
      yname <- mapping_string(mapping$y)

      # we need to duplicate date for each level of y
      # and to map y to linetype
      d <- data.frame()
      for (l in levels(y)) {
        tmp <- data
        tmp[[".ggally_y"]] <- as.integer(y == l)
        tmp$y <- l
        d <- dplyr::bind_rows(d, tmp)
      }
      mapping$linetype <- aes(y = !!as.name("y"))$y
      mapping$y <- aes(y = !!as.name(".ggally_y"))$y

      # recomputing groups
      g <- list()
      if (!is.null(mapping$alpha))
        g <- append(g, list(eval_data_col(d, mapping$alpha)))
      if (!is.null(mapping$colour))
        g <- append(g, list(eval_data_col(d, mapping$colour)))
      if (!is.null(mapping$linetype))
        g <- append(g, list(eval_data_col(d, mapping$linetype)))
      if (!is.null(mapping$size))
        g <- append(g, list(eval_data_col(d, mapping$size)))
      d$.group <- interaction(g)
      mapping$group <- aes(group = !!as.name(".group"))$group

      p <- ggplot(d, mapping) +
        stat_weighted_mean(geom = "line", ...) +
        scale_y_continuous(labels = scales::label_percent()) +
        ylab("") +
        labs(linetype = yname)
    }
  } else {
    # Numeric variable
    p <- ggplot(data, mapping) +
      stat_weighted_mean(geom = "line", ...)
  }

  if (include_zero) {
    p <- p + expand_limits(y = 0)
  }

  p
}
