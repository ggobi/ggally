#' Compute cross-tabulation statistics
#'
#' Computes statistics of a 2-dimensional matrix using \code{\link[broom]{augment.htest}}
#' from \pkg{broom}.
#'
#' @inheritParams ggplot2::stat_identity
#' @param geom Override the default connection between \code{\link[ggplot2]{geom_point}}
#'   and \code{stat_prop}.
#' @param na.rm If \code{TRUE}, the default, missing values are removed with a warning.
#'   If `TRUE`, missing values are silently removed.
#' @param keep.zero.cells If \code{TRUE}, cells with no observations are kept.
#' @section Aesthetics:
#' \code{stat_prop} requires the \strong{x} and the \strong{y} aesthetics.
#' @section Computed variables:
#' \describe{
#'   \item{observed}{number of observations in x,y}
#'   \item{prop}{proportion of total}
#'   \item{row.prop}{row proportion}
#'   \item{col.prop}{column proportion}
#'   \item{expected}{expected count under the null hypothesis}
#'   \item{resid}{Pearson's residual}
#'   \item{std.resid}{standardized residual}
#' }
#'
#' @export
#' @examples
#' # Small function to display plots only if it's interactive
#' p_ <- GGally::print_if_interactive
#'
#' d <- as.data.frame(Titanic)
#'
#' # plot number of observations
#' p_(ggplot(d) +
#'  aes(x = Class, y = Survived, weight = Freq, size = after_stat(observed)) +
#'  stat_cross() +
#'  scale_size_area(max_size = 20))
#'
#' # custom shape and fill colour based on chi-squared residuals
#' p_(ggplot(d) +
#'  aes(
#'    x = Class, y = Survived, weight = Freq,
#'    size = after_stat(observed), fill = after_stat(std.resid)
#'  ) +
#'  stat_cross(shape = 22) +
#'  scale_fill_steps2(breaks = c(-3, -2, 2, 3), show.limits = TRUE) +
#'  scale_size_area(max_size = 20))
#'
#' # plotting the number of observations as a table
#' p_(ggplot(d) +
#'  aes(
#'    x = Class, y = Survived, weight = Freq, label = after_stat(observed)
#'  ) +
#'  geom_text(stat = "cross"))
#'
#' # Row proportions with standardized residuals
#' p_(ggplot(d) +
#'   aes(
#'     x = Class, y = Survived, weight = Freq,
#'     label = scales::percent(after_stat(row.prop)),
#'     size = NULL, fill = after_stat(std.resid)
#'   ) +
#'   stat_cross(shape = 22, size = 30) +
#'   geom_text(stat = "cross") +
#'   scale_fill_steps2(breaks = c(-3, -2, 2, 3), show.limits = TRUE) +
#'   facet_grid(Sex ~ .) +
#'   labs(fill = "Standardized residuals") +
#'   theme_minimal())
#'
#' # can work with continuous or character variables
#' data(tips, package = "reshape")
#' p_(ggplot(tips) +
#'   aes(x = tip, y = as.character(day), size = after_stat(observed)) +
#'   stat_cross(alpha = .1, color = "blue") +
#'   scale_size_area(max_size = 12))
stat_cross <- function(mapping = NULL, data = NULL,
                       geom = "point", position = "identity",
                       ...,
                       na.rm = TRUE,
                       show.legend = NA,
                       inherit.aes = TRUE,
                       keep.zero.cells = FALSE) {

  params <- list(
    na.rm = na.rm,
    keep.zero.cells = keep.zero.cells,
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

  compute_panel = function(self, data, scales, keep.zero.cells = FALSE) {
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
      "resid",
      "std.resid"
    )) {
      from_name <- paste0(".", to_name)
      panel_names[which(panel_names == from_name)] <- to_name
    }
    names(panel) <- panel_names

    # to handle the fact that ggplot2 could transform factors into integers
    # before computation of the statistic
    if(is.numeric(data$x)) panel$x <- as.numeric(panel$x)
    if(is.numeric(data$y)) panel$y <- as.numeric(panel$y)

    # keeping first value of other aesthetics in data
    panel <- merge(panel, data, by = c("x", "y"), all.x = TRUE)
    panel <- panel %>% dplyr::distinct(.data$x, .data$y, .keep_all = TRUE)

    if (!keep.zero.cells) {
      panel <- panel[panel$observed != 0,]
    }

    panel
  }
)


#' Plots the number of observations
#'
#' Plot the number of observations by using square points
#' with proportional areas. Could be filled according to chi-squared
#' statistics computed by [stat_cross()]. Labels could also
#' be added (see examples).
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... other arguments passed to [ggplot2::geom_point()]
#' @param scale_max_size `max_size` argument supplied to [ggplot2::scale_size_area()]
#' @param geom_text_args other arguments passed to [ggplot2::geom_text()]
#' @author Joseph Larmarange
#' @keywords hplot
#' @export
#' @examples
#' # Small function to display plots only if it's interactive
#' p_ <- GGally::print_if_interactive
#'
#' data(tips, package = "reshape")
#' p_(ggally_cross(tips, mapping = aes(x = smoker, y = sex)))
#' p_(ggally_cross(tips, mapping = aes(x = day, y = time)))
#'
#' # Custom max size
#' p_(ggally_cross(tips, mapping = aes(x = smoker, y = sex)) +
#'   scale_size_area(max_size = 40))
#'
#' # Custom fill
#' p_(ggally_cross(tips, mapping = aes(x = smoker, y = sex), fill = "red"))
#'
#' # Custom shape
#' p_(ggally_cross(tips, mapping = aes(x = smoker, y = sex), shape = 21))
#'
#' # Fill squares according to standardized residuals
#' d <- as.data.frame(Titanic)
#' p_(ggally_cross(
#'   d,
#'   mapping = aes(x = Class, y = Survived, weight = Freq, fill = after_stat(std.resid))
#' ) +
#'   scale_fill_steps2(breaks = c(-3, -2, 2, 3), show.limits = TRUE))
#'
#' # Add labels
#' p_(ggally_cross(
#'   tips,
#'   mapping = aes(
#'     x = smoker, y = sex, colour = smoker,
#'     label = scales::percent(after_stat(prop))
#'   )
#' ))
#'
#' # Customize labels' appearance and same size for all squares
#' p_(ggally_cross(
#'   tips,
#'   mapping = aes(
#'     x = smoker, y = sex,
#'     size = NULL, # do not map size to a variable
#'     label = scales::percent(after_stat(prop))
#'   ),
#'   size = 40, # fix value for points size
#'   fill = "darkblue",
#'   geom_text_args = list(colour = "white", fontface = "bold", size = 6)
#' ))
ggally_cross <- function(data, mapping, ..., scale_max_size = 20, geom_text_args = NULL){
  mapping <- remove_color_unless_equal(mapping, to = c("x", "y"))
  mapping <- mapping_color_to_fill(mapping)

  args <- list(...)
  # default values for geom_point
  if (!"size" %in% names(mapping)) {
    mapping$size <- aes_string(size = "after_stat(observed)")$size
  }
  if (is.null(mapping$shape) & is.null(args$shape)) {
    args$shape <- 22
  }
  if (is.null(mapping$fill) & is.null(args$fill)) {
    args$fill <- GeomRect$default_aes$fill
  }
  args$keep.zero.cells <- FALSE

  p <- ggplot(data = data, mapping) +
    do.call(stat_cross, args) +
    scale_size_area(max_size = scale_max_size)

  # default values for geom_text
  geom_text_args$stat <- "cross"
  geom_text_args$keep.zero.cells <- FALSE
  if (is.null(geom_text_args$mapping))
    geom_text_args$mapping <- aes(colour = NULL, size = NULL)
  if (is.null(geom_text_args$show.legend))
    geom_text_args$show.legend <- FALSE

  if (!is.null(mapping$label)) {
    p <- p +
      do.call(geom_text, geom_text_args)
  }

  p
}

#' Display a table of the number of observations
#'
#' Plot the number of observations as a table. Other statistics computed
#' by \code{\link{stat_cross}}  could be used (see examples).
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param keep.zero.cells If \code{TRUE}, display cells with no observation.
#' @param ... other arguments passed to \code{\link[ggplot2]{geom_text}(...)}
#' @param geom_tile_args other arguments passed to \code{\link[ggplot2]{geom_tile}(...)}
#' @note The \strong{colour} aesthetic is taken into account only if equal to
#'   \strong{x} or \strong{y}.
#' @author Joseph Larmarange
#' @keywords hplot
#' @export
#' @examples
#' # Small function to display plots only if it's interactive
#' p_ <- GGally::print_if_interactive
#'
#' data(tips, package = "reshape")
#' p_(ggally_table(tips, mapping = aes(x = smoker, y = sex)))
#' p_(ggally_table(tips, mapping = aes(x = day, y = time)))
#' p_(ggally_table(tips, mapping = aes(x = smoker, y = sex, colour = smoker)))
#'
#' # colour is kept only if equal to x or y
#' p_(ggally_table(tips, mapping = aes(x = smoker, y = sex, colour = day)))
#'
#' # diagonal version
#' p_(ggally_tableDiag(tips, mapping = aes(x = smoker)))
#'
#' # custom label size and color
#' p_(ggally_table(tips, mapping = aes(x = smoker, y = sex), size = 16, color = "red"))
#'
#' # display column proportions
#' p_(ggally_table(
#'   tips,
#'   mapping = aes(x = day, y = sex, label = scales::percent(after_stat(col.prop)))
#' ))
#'
#' # draw table cells
#' p_(ggally_table(
#'   tips,
#'   mapping = aes(x = smoker, y = sex),
#'   geom_tile_args = list(colour = "black", fill = "white")
#' ))
#'
#' # Use standardized residuals to fill table cells
#' p_(ggally_table(
#'   as.data.frame(Titanic),
#'   mapping = aes(
#'     x = Class, y = Survived, weight = Freq,
#'     fill = after_stat(std.resid),
#'     label = scales::percent(after_stat(col.prop), accuracy = .1)
#'   ),
#'   geom_tile_args = list(colour = "black")
#' ) +
#' scale_fill_steps2(breaks = c(-3, -2, 2, 3), show.limits = TRUE))
ggally_table <- function(data, mapping, keep.zero.cells = FALSE, ..., geom_tile_args = NULL){
  mapping <- remove_color_unless_equal(mapping, to = c("x", "y"))

  # default values geom_text
  if (!"label" %in% names(mapping)) {
    mapping$label <- aes_string(label = "after_stat(observed)")$label
  }
  geom_text_args <- list(...)
  geom_text_args$stat <- "cross"
  geom_text_args$keep.zero.cells <- keep.zero.cells

  # default values geom_tile
  geom_tile_args$stat <- "cross"
  geom_tile_args$keep.zero.cells <- keep.zero.cells
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
#' @rdname ggally_table
ggally_tableDiag <- function(data, mapping, keep.zero.cells = FALSE, ..., geom_tile_args = NULL) {
  mapping$y <- mapping$x
  ggally_table(
    data = data, mapping = mapping,
    keep.zero.cells = keep.zero.cells, ...,
    geom_tile_args = geom_tile_args
  )
}

#' Display a cross-tabulated table
#'
#' \code{ggally_crosstable} is a variation of \code{\link{ggally_table}} with few modifications: (i) table cells are drawn; (ii) x and y axis are not expanded (and therefore are not aligned with other \code{ggally_*} plots); (iii) content and fill of cells can be easily controlled with dedicated arguments.
#' @param data data set using
#' @param mapping aesthetics being used
#' @param cells Which statistic should be displayed in table cells?
#' @param fill Which statistic should be used for filling table cells?
#' @param ... other arguments passed to \code{\link[ggplot2]{geom_text}(...)}
#' @param geom_tile_args other arguments passed to \code{\link[ggplot2]{geom_tile}(...)}
#' @export
#' @examples
#' # Small function to display plots only if it's interactive
#' p_ <- GGally::print_if_interactive
#'
#' data(tips, package = "reshape")
#'
#' # differences with ggally_table()
#' p_(ggally_table(tips, mapping = aes(x = day, y = time)))
#' p_(ggally_crosstable(tips, mapping = aes(x = day, y = time)))
#'
#' # display column proportions
#' p_(ggally_crosstable(tips, mapping = aes(x = day, y = sex), cells = "col.prop"))
#'
#' # display row proportions
#' p_(ggally_crosstable(tips, mapping = aes(x = day, y = sex), cells = "row.prop"))
#'
#' # change size of text
#' p_(ggally_crosstable(tips, mapping = aes(x = day, y = sex), size = 8))
#'
#' # fill cells with standardized residuals
#' p_(ggally_crosstable(tips, mapping = aes(x = day, y = sex), fill = "std.resid"))
#'
#' # change scale for fill
#' p_(ggally_crosstable(tips, mapping = aes(x = day, y = sex), fill = "std.resid") +
#'   scale_fill_steps2(breaks = c(-2, 0, 2), show.limits = TRUE))
ggally_crosstable <- function(
  data,
  mapping,
  cells = c("observed", "prop", "row.prop", "col.prop", "expected", "resid", "std.resid"),
  fill = c("none", "std.resid", "resid"),
  ...,
  geom_tile_args = list(colour = "grey50")
){
  fill <- match.arg(fill)
  if (fill == "std.resid")
    mapping$fill <- aes_string(fill = "after_stat(std.resid)")$fill
  if (fill == "resid")
    mapping$fill <- aes_string(fill = "after_stat(resid)")$fill
  if (fill == "none")
    geom_tile_args$fill <- "white"

  cells <- match.arg(cells)
  if (!"label" %in% names(mapping) && cells %in% c("observed", "expected"))
    mapping$label <- aes_string(label = paste0("scales::number(after_stat(", cells, "), accuracy = 1)"))$label
  if (!"label" %in% names(mapping) && cells %in% c("prop", "row.prop", "col.prop"))
    mapping$label <- aes_string(label = paste0("scales::percent(after_stat(", cells, "), accuracy = .1)"))$label
  if (!"label" %in% names(mapping) && cells %in% c("resid", "std.resid"))
    mapping$label <- aes_string(label = paste0("scales::number(after_stat(", cells, "), accuracy = .1)"))$label

  p <- ggally_table(data = data, mapping = mapping, keep.zero.cells = TRUE, geom_tile_args = geom_tile_args, ...) +
    scale_x_discrete(expand = expansion(0, 0)) +
    scale_y_discrete(expand = expansion(0, 0)) +
    theme(axis.ticks = element_blank())

  if (fill == "std.resid")
    p <- p + scale_fill_steps2(breaks = c(-Inf, -3, -2, 2, 3, Inf))

  p
}
