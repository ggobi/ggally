#' Compute weighted y mean
#'
#' This statistic will compute the mean of \strong{y} aesthetic for
#' each unique value of \strong{x}, taking into account \strong{weight}
#' aesthetic if provided.
#'
#' @section Computed variables:
#' \describe{
#'   \item{y}{weighted y (numerator / denominator)}
#'   \item{numerator}{numerator}
#'   \item{denominator}{denominator}
#' }
#'
#' @inheritParams ggplot2::stat_bin
#' @export
#' @examples
#' data(tips, package = "reshape")
#'
#' ggplot(tips) +
#'   aes(x = day, y = total_bill) +
#'   geom_point()
#'
#' ggplot(tips) +
#'   aes(x = day, y = total_bill) +
#'   stat_weighted_mean()
#'
#' ggplot(tips) +
#'   aes(x = day, y = total_bill, group = 1) +
#'   stat_weighted_mean(geom = "line")
#'
#' ggplot(tips) +
#'   aes(x = day, y = total_bill, colour = sex, group = sex) +
#'   stat_weighted_mean(geom = "line")
#'
#' ggplot(tips) +
#'   aes(x = day, y = total_bill, fill = sex) +
#'   stat_weighted_mean(geom = "bar", position = "dodge")
#'
#' # computing a proportion on the fly
#' ggplot(tips) +
#'   aes(x = day, y = as.integer(smoker == "Yes"), fill = sex) +
#'   stat_weighted_mean(geom = "bar", position = "dodge") +
#'   scale_y_continuous(labels = scales::percent)
#'
#' # taking into account some weights
#' d <- as.data.frame(Titanic)
#' ggplot(d) +
#'   aes(x = Class, y = as.integer(Survived == "Yes"), weight = Freq, fill = Sex) +
#'   geom_bar(stat = "weighted_mean", position = "dodge") +
#'   scale_y_continuous(labels = scales::percent)
#'
#'
#' \dontrun{
#' cuse <- read.table("https://data.princeton.edu/wws509/datasets/cuse.dat", header = TRUE)
#' cuse$n <- cuse$notUsing + cuse$using
#' cuse$prop <- cuse$using / cuse$n
#'
#' ggplot(cuse) +
#'   aes(x = education, y = prop, weight = n) +
#'   stat_weighted_mean()
#'
#' ggplot(cuse) +
#'   aes(x = age, y = prop, weight = n, color = education) +
#'   stat_weighted_mean()
#'
#' ggplot(cuse) +
#'   aes(x = education, y = prop, weight = n) +
#'   stat_weighted_mean(geom = "bar")
#'
#' ggplot(cuse) +
#'   aes(x = age, y = prop, weight = n, fill = education) +
#'   stat_weighted_mean(geom = "bar") +
#'   geom_text(aes(label = scales::percent(after_stat(y))), stat = "weighted_mean", vjust = 0) +
#'   facet_grid(~ education)
#' }
stat_weighted_mean <- function(mapping = NULL, data = NULL,
                         geom = "point", position = "identity",
                         ...,
                         na.rm = FALSE,
                         orientation = NA,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatWeightedMean,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}

#' @rdname stat_weighted_mean
#' @format NULL
#' @usage NULL
#' @export
StatWeightedMean <- ggproto(
  "StatSummary",
  Stat,
  required_aes = c("x", "y"),
  extra_params = c("na.rm", "orientation"),
  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params)
    params
  },

  compute_panel = function(data, scales, na.rm = FALSE, flipped_aes = FALSE) {
    data <- ggplot2::flip_data(data, flipped_aes)
    if (is.null(data$weight))
      data$weight <- rep(1, nrow(data))

    summarised <- aggregate(
      cbind(numerator = y * weight, denominator = weight) ~ .,
      data, FUN = sum, na.rm = TRUE
    )
    summarised$y <- summarised$numerator / summarised$denominator

    summarised$flipped_aes <- flipped_aes
    ggplot2::flip_data(summarised, flipped_aes)
  }
)


#' Trends line plot
#'
#' Plot trends using line plots.
#' For continuous y variables, plot the evolution of the mean.
#' For binary y variables, plot the evolution of the proportion.
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... other arguments passed to \code{\link[ggplot2]{geom_line}(...)}
#' @param include_zero Should 0 be included on the y-axis?
#' @author Joseph Larmarange \email{joseph@@larmarange.net}
#' @keywords hplot
#' @export
#' @importFrom plyr rbind.fill
#' @examples
#' data(tips, package = "reshape")
#' tips$day <- factor(tips$day, c("Thur", "Fri", "Sat", "Sun"))
#'
#' # Numeric variable
#' ggally_trends(tips, mapping = aes(x = day, y = total_bill))
#' ggally_trends(tips, mapping = aes(x = day, y = total_bill, colour = time))
#'
#' # Binary variable
#' ggally_trends(tips, mapping = aes(x = day, y = smoker))
#' ggally_trends(tips, mapping = aes(x = day, y = smoker, colour = sex))
#'
#' # Discrete variable with 3 or more categories
#' ggally_trends(tips, mapping = aes(x = smoker, y = day))
#' ggally_trends(tips, mapping = aes(x = smoker, y = day, color = sex))
#'
#' # Include zero on Y axis
#' ggally_trends(tips, mapping = aes(x = day, y = total_bill), include_zero = TRUE)
#' ggally_trends(tips, mapping = aes(x = day, y = smoker), include_zero = TRUE)
#'
#' # Change line size
#' ggally_trends(tips, mapping = aes(x = day, y = smoker, colour = sex), size = 3)
#'
#' # Define weights with the appropriate aesthetic
#' d <- as.data.frame(Titanic)
#' ggally_trends(
#'   d,
#'   mapping = aes(x = Class, y = Survived, weight = Freq, color = Sex),
#'   include_zero = TRUE
#' )
#'
#'
ggally_trends <- function(data, mapping,
                          ...,
                          include_zero = FALSE) {
  if (is.null(mapping$x)) stop("'x' aesthetic is required.")
  if (is.null(mapping$y)) stop("'y' aesthetic is required.")

  # computing group
  g <- list()
  if (!is.null(mapping$alpha)) g <- append(g, list(eval_data_col(data, mapping$alpha)))
  if (!is.null(mapping$colour)) g <- append(g, list(eval_data_col(data, mapping$colour)))
  if (!is.null(mapping$linetype)) g <- append(g, list(eval_data_col(data, mapping$linetype)))
  if (!is.null(mapping$size)) g <- append(g, list(eval_data_col(data, mapping$size)))
  if (length(g) == 0) {
    mapping$group <- aes(group = 1)$group
  } else {
    data$.group <- interaction(g)
    mapping$group <- aes_string(group = ".group")$group
  }

  # considering the different situations regarding y
  y <- eval_data_col(data, mapping$y)
  if (is.factor(y) || is.character(y) || is.logical(y)) {
    y <- as.factor(y)
    if (length(levels(y)) == 2) { # Binary variable
      data$.y <- as.integer(y == levels(y)[2])
      mapping$y <- aes_string(y = ".y")$y
      p <- ggplot(data, mapping) +
        stat_weighted_mean(geom = "line", ...) +
        scale_y_continuous(labels = scales::percent) +
        ylab("")
    } else { # 3 or more categories
      yname <- mapping_string(mapping$y)

      # we need to duplicate date for each level of y
      # and to map y to linetype
      d <- data.frame()
      for (l in levels(y)) {
        tmp <- data
        tmp$.y <- as.integer(y == l)
        tmp$y <- l
        d <- plyr::rbind.fill(d, tmp)
      }
      mapping$linetype <- aes_string(y = "y")$y
      mapping$y <- aes_string(y = ".y")$y

      # recomputing groups
      g <- list()
      if (!is.null(mapping$alpha)) g <- append(g, list(eval_data_col(d, mapping$alpha)))
      if (!is.null(mapping$colour)) g <- append(g, list(eval_data_col(d, mapping$colour)))
      if (!is.null(mapping$linetype)) g <- append(g, list(eval_data_col(d, mapping$linetype)))
      if (!is.null(mapping$size)) g <- append(g, list(eval_data_col(d, mapping$size)))
      d$.group <- interaction(g)
      mapping$group <- aes_string(group = ".group")$group

      p <- ggplot(d, mapping) +
        stat_weighted_mean(geom = "line", ...) +
        scale_y_continuous(labels = scales::percent) +
        ylab("") + labs(linetype = yname)
    }
  } else { # Numeric variable
    p <- ggplot(data, mapping) +
      stat_weighted_mean(geom = "line", ...)
  }

  if (include_zero)
    p <- p + expand_limits(y = 0)

  p
}
