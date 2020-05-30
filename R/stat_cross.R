#' Compute cross-tabulation statisticics
#'
#' Computes statistics of a 2-dimensional matrix using \code{\link[broom]{augment.htest}}
#' from \pkg{broom}.
#'
#' @inheritParams ggplot2::stat_identity
#' @param geom Override the default connection between \code{\link[ggplot1]{geom_point}}
#'   and \code{stat_prop}.
#' @param na.rm If \code{TRUE}, the default, missing values are removed with a warning.
#'   If `TRUE`, missing values are silently removed.
#' @section Aesthetics:
#' \code{stat_prop} requires the \strong{x} and the \strong{y} aesthetics.
#' @section Computed variables:
#' \describe{
#'   \item{observed}{number of observations in x,y}
#'   \item{prop}{proportion of total}
#'   \item{row.prop}{row proportion}
#'   \item{col.prop}{column proportion}
#'   \item{expected}{expected count under the null hypothesis}
#'   \item{residuals}{Pearson's residual}
#'   \item{stdres}{standardized residual}
#' }
#'
#' @export
#' @examples
#' d <- as.data.frame(Titanic)
#'
#' # by default, plot number of observations
#' ggplot(d) +
#'   aes(x = Class, y = Survived, weight = Freq) +
#'   stat_cross() +
#'   scale_size_area()
#'
#' # custom shape and fill colour based on chi-squared residuals
#' ggplot(d) +
#'   aes(x = Class, y = Survived, weight = Freq, fill = after_stat(stdres)) +
#'   stat_cross(shape = 22) +
#'   scale_fill_steps2(breaks = c(-4, -2, 2, 4), show.limits = TRUE)
#'
#' # plotting the number of observations as a table
#' ggplot(d) +
#'   aes(
#'     x = Class, y = Survived, weight = Freq,
#'     label = scales::percent(after_stat(row.prop)), size = NULL
#'   ) +
#'   geom_text(stat = "cross")
#'
#' # Row proportions with standardized residuals
#' ggplot(d) +
#'   aes(
#'     x = Class, y = Survived, weight = Freq,
#'     label = scales::percent(after_stat(row.prop)),
#'      size = NULL, fill = after_stat(stdres)
#'   ) +
#'   stat_cross(shape = 22, size = 30) +
#'   geom_text(stat = "cross") +
#'   scale_fill_steps2(breaks = c(-3, -2, 3, 4), show.limits = TRUE) +
#'   facet_grid(Sex ~ .) +
#'   labs(fill = "Standardized residuals") +
#'   theme_minimal()
#'
#' # can work with continuous or character variables
#' data(tips, package = "reshape")
#' ggplot(tips) +
#'   aes(x = tip, y = as.character(day)) +
#'   stat_cross(alpha = .1, color = "blue") +
#'   scale_size_area(max_size = 12)
#'
stat_cross <- function(mapping = NULL, data = NULL,
                       geom = "point", position = "identity",
                       ...,
                       na.rm = TRUE,
                       show.legend = NA,
                       inherit.aes = TRUE) {

  params <- list(
    na.rm = na.rm,
    ...
  )

  layer(
    data = data,
    mapping = mapping,
    stat = StatCross,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
}

#' @rdname stat_cross
#' @format NULL
#' @usage NULL
#' @export
StatCross <- ggproto("StatCross", Stat,
  required_aes = c("x", "y"),
  default_aes = aes(
    weight = 1
  ),

  setup_params = function(data, params) {
    params
  },

  extra_params = c("na.rm"),

  compute_panel = function(self, data, scales) {
    if (is.null(data$weight))
      data$weight <- rep(1, nrow(data))

    # compute cross statistics
    panel <- broom::augment(chisq.test(xtabs(weight ~ y + x, data = data)))

    panel_names <- names(panel)
    for (to_name in c(
      "observed",
      "prop",
      "row.prop",
      "col.prop",
      "expected",
      "residuals",
      "stdres"
    )) {
      from_name <- paste0(".", to_name)
      panel_names[which(panel_names == from_name)] <- to_name
    }
    names(panel) <- panel_names

    data <- merge(data, panel, by = c("x", "y"), all.x = TRUE)
    data
  }
)


#' Plots the number of observations
#'
#' Plot the number of observations by using squares points
#' with proportional areas. Could be filled according to chi-squared
#' statistics computed by \code{\link{stat_cross}}. Labels could also
#' be added (see examples).
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... other arguments passed to \code{\link[ggplot2]{geom_point}(...)}
#' @param geom_text_args other arguments passed to \code{\link[ggplot2]{geom_text}(...)}
#' @author Joseph Larmarange \email{joseph@@larmarange.net}
#' @keywords hplot
#' @export
#' @examples
#' data(tips, package = "reshape")
#' ggally_cross(tips, mapping = aes(x = smoker, y = sex))
#'
#' # Custom max size
#' ggally_cross(tips, mapping = aes(x = smoker, y = sex)) +
#'   scale_size_area(max_size = 40)
#'
#' # Custom fill
#' ggally_cross(tips, mapping = aes(x = smoker, y = sex), fill = "red")
#'
#' # Custom shape
#' ggally_cross(tips, mapping = aes(x = smoker, y = sex), shape = 21)
#'
#' # Fill squares according to standardized residuals
#' d <- as.data.frame(Titanic)
#' ggally_cross(
#'   d,
#'   mapping = aes(x = Class, y = Survived, weight = Freq, fill = after_stat(stdres))
#' ) +
#'   scale_fill_steps2(breaks = c(-3, -2, 2, 3), show.limits = TRUE)
#'
#' # Add labels
#' ggally_cross(
#'   tips,
#'   mapping = aes(
#'     x = smoker, y = sex, colour = smoker,
#'     label = scales::percent(after_stat(prop))
#'   )
#' )
#'
#' # Customize labels' appearance and same size for all squares
#' ggally_cross(
#'   tips,
#'   mapping = aes(
#'     x = smoker, y = sex,
#'     size = NULL, # do not map size to a variable
#'     label = scales::percent(after_stat(prop))
#'   ),
#'   size = 40, # fix value for points size
#'   fill = "darkblue",
#'   geom_text_args = list(colour = "white", fontface = "bold", size = 6)
#' )
ggally_cross <- function(data, mapping, ..., geom_text_args = NULL){
  mapping <- remove_color_unless_equal(mapping, to = c("x", "y"))
  mapping <- mapping_color_to_fill(mapping)

  args <- list(...)
  # default values for geom_point
  if (!"size" %in% names(mapping))
    mapping$size <- aes_string(size = "after_stat(observed)")$size
  if (is.null(mapping$shape) & is.null(args$shape))
    args$shape <- 22
  if (is.null(mapping$fill) & is.null(args$fill))
    args$fill <- "white"

  p <- ggplot(data = data, mapping) +
    do.call(stat_cross, args) +
    scale_size_area(max_size = 20)

  # default values for geom_text
  geom_text_args$stat <- "cross"
  if (is.null(geom_text_args$mapping))
    geom_text_args$mapping <- aes(colour = NULL, size = NULL)
  if (is.null(geom_text_args$show.legend))
    geom_text_args$show.legend <- FALSE

  if(!is.null(mapping$label))
    p <- p +
      do.call(geom_text, geom_text_args)

  p
}

#' Display a table of the number of observations
#'
#' Plot the number of observations as a table. Other statistics computed
#' by \code{\link{stat_cross}}  could be used (see examples).
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... other arguments passed to \code{\link[ggplot2]{geom_text}(...)}
#' @param geom_tile_args other arguments passed to \code{\link[ggplot2]{geom_tile}(...)}
#' @note The \strong{colour} aesthetic is taken into account only if equal to
#'   \strong{x} or \strong{y}.
#' @author Joseph Larmarange \email{joseph@@larmarange.net}
#' @keywords hplot
#' @export
#' @examples
#' data(tips, package = "reshape")
#' ggally_table(tips, mapping = aes(x = smoker, y = sex))
#' ggally_table(tips, mapping = aes(x = smoker, y = sex, colour = smoker))
#' ggally_table(tips, mapping = aes(x = smoker, y = sex, colour = day))
#' ggally_tableDiag(tips, mapping = aes(x = smoker))
#'
#' # Custom label size and color
#' ggally_table(tips, mapping = aes(x = smoker, y = sex), size = 16, color = "red")
#'
#' # Display column proportions
#' ggally_table(tips, mapping = aes(x = day, y = sex, label = scales::percent(after_stat(col.prop))))
#'
#' # Draw table cells
#' ggally_table(
#'   tips,
#'   mapping = aes(x = smoker, y = sex),
#'   geom_tile_args = list(colour = "black", fill = "white")
#' )
#'
#' # Use standardized residuals to fill table cells
#' ggally_table(
#'   as.data.frame(Titanic),
#'   mapping = aes(
#'     x = Class, y = Survived, weight = Freq,
#'     fill = after_stat(stdres),
#'     label = scales::percent(after_stat(col.prop), accuracy = .1)
#'   ),
#'   geom_tile_args = list(colour = "black")
#' ) +
#' scale_fill_steps2(breaks = c(-3, -2, 2, 3), show.limits = TRUE)
ggally_table <- function(data, mapping, ..., geom_tile_args = NULL){
  mapping <- keep_colour_if_in_x_or_y(mapping)

  # default values geom_text
  if (!"label" %in% names(mapping))
    mapping$label <- aes_string(label = "after_stat(observed)")$label
  geom_text_args <- list(...)
  geom_text_args$stat <- "cross"

  # default values geom_tile
  geom_tile_args$stat <- "cross"
  geom_tile_args$mapping <- aes(colour = NULL)$colour
  if (is.null(geom_tile_args$colour))
    geom_tile_args$colour <- "transparent"
  if (is.null(mapping$fill) & is.null(geom_tile_args$fill))
    geom_tile_args$fill <- "transparent"

  ggplot(data = data, mapping) +
    do.call(geom_tile, geom_tile_args) +
    do.call(geom_text, geom_text_args)
}

#' @export
ggally_tableDiag <- function(data, mapping, ..., geom_tile_args = NULL) {
  mapping$y <- mapping$x
  ggally_table(data = data, mapping = mapping, ..., geom_tile_args = geom_tile_args)
}
