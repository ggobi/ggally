#' Column and row bar plots
#'
#' Plot column or row percentage using bar plots.
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param label_format formatter function for displaying proportions, not taken into account if a label aesthetic is provided in \code{mapping}
#' @param ... other arguments passed to \code{\link[ggplot2]{geom_text}(...)}
#' @param remove_percentage_axis should percentage axis be removed? Removes the y-axis for \code{ggally_colbar()} and x-axis for \code{ggally_rowbar()}
#' @param remove_background should the \code{panel.background} be removed?
#' @param reverse_fill_levels should the levels of the fill variable be reversed?
#' @param geom_bar_args other arguments passed to \code{\link[ggplot2]{geom_bar}(...)}
#' @author Joseph Larmarange
#' @keywords hplot
#' @export
#' @examples
#' # Small function to display plots only if it's interactive
#' p_ <- GGally::print_if_interactive
#'
#' data(tips)
#' p_(ggally_colbar(tips, mapping = aes(x = smoker, y = sex)))
#' p_(ggally_rowbar(tips, mapping = aes(x = smoker, y = sex)))
#'
#' # change labels' size
#' p_(ggally_colbar(tips, mapping = aes(x = smoker, y = sex), size = 8))
#'
#' # change labels' colour and use bold
#' p_(ggally_colbar(tips,
#'   mapping = aes(x = smoker, y = sex),
#'   colour = "white", fontface = "bold"
#' ))
#'
#' # display number of observations instead of proportions
#' p_(ggally_colbar(tips, mapping = aes(x = smoker, y = sex, label = after_stat(count))))
#'
#' # custom bar width
#' p_(ggally_colbar(tips, mapping = aes(x = smoker, y = sex), geom_bar_args = list(width = .5)))
#'
#' # change format of labels
#' p_(ggally_colbar(tips,
#'   mapping = aes(x = smoker, y = sex),
#'   label_format = scales::label_percent(accuracy = .01, decimal.mark = ",")
#' ))
#'
#' p_(ggduo(
#'   data = as.data.frame(Titanic),
#'   mapping = aes(weight = Freq),
#'   columnsX = "Survived",
#'   columnsY = c("Sex", "Class", "Age"),
#'   types = list(discrete = "rowbar"),
#'   legend = 1
#' ))
ggally_colbar <- function(
  data,
  mapping,
  label_format = scales::label_percent(accuracy = .1),
  ...,
  remove_background = FALSE,
  remove_percentage_axis = FALSE,
  reverse_fill_levels = FALSE,
  geom_bar_args = NULL
) {
  if (is.null(mapping$x)) {
    stop("'x' aesthetic is required.")
  }
  if (is.null(mapping$y)) {
    stop("'y' aesthetic is required.")
  }

  # y should be mapped to fill and x to by
  mapping$fill <- mapping$y
  mapping$y <- NULL
  mapping$by <- mapping$x

  # colour should not be mapped in aes
  if (!is.null(mapping$colour)) {
    mapping$colour <- NULL
  }

  # label mapping
  if (!is.null(mapping$label)) {
    mapping_text <- aes()
    mapping_text$label <- mapping$label
  } else {
    mapping_text <- aes(label = label_format(after_stat(!!as.name("prop"))))
  }

  # position for geom_bar
  geom_bar_args$position <- position_fill(reverse = reverse_fill_levels)

  p <- ggplot(data, mapping) +
    do.call(geom_bar, geom_bar_args) +
    geom_text(
      mapping = mapping_text,
      stat = "prop",
      position = position_fill(.5, reverse = reverse_fill_levels),
      ...
    ) +
    scale_y_continuous(
      labels = scales::label_percent(),
      expand = expansion(ifelse(remove_background, 0, .05), 0)
    ) +
    scale_x_discrete(expand = expansion(0, ifelse(remove_background, 0, .6))) +
    ylab("") +
    guides(fill = guide_legend(reverse = reverse_fill_levels))

  if (isTRUE(remove_background)) {
    p <- p +
      theme(
        panel.background = element_blank()
      )
  }

  if (isTRUE(remove_percentage_axis)) {
    p <- p +
      theme(
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
  }

  p
}

#' @rdname ggally_colbar
#' @export
ggally_rowbar <- function(
  data,
  mapping,
  label_format = scales::label_percent(accuracy = .1),
  ...,
  remove_background = FALSE,
  remove_percentage_axis = FALSE,
  reverse_fill_levels = TRUE,
  geom_bar_args = NULL
) {
  mapping <- mapping_swap_x_y(mapping)

  p <- ggally_colbar(
    data = data,
    mapping = mapping,
    label_format = label_format,
    ...,
    remove_background = remove_background,
    remove_percentage_axis = FALSE,
    reverse_fill_levels = reverse_fill_levels,
    geom_bar_args = geom_bar_args
  ) +
    coord_flip() +
    guides(fill = guide_legend(reverse = !reverse_fill_levels))

  if (isTRUE(remove_percentage_axis)) {
    p <- p +
      theme(
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )
  }

  p
}
