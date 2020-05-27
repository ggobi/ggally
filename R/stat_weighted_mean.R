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

