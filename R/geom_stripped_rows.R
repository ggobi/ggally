#' Alternating Background Colour
#'
#' Add alternating background color along the y-axis. The geom takes default
#' aesthetics \code{odd} and \code{even} that receive color codes.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_rect
#' @param xfrom,xto limitation of the strips along the x-axis
#' @param width width of the strips
#' @param nudge_x,nudge_y horizontal or vertical adjustment to nudge strips by
#' @export
#' @examples
#' # Small function to display plots only if it's interactive
#' p_ <- GGally::print_if_interactive
#'
#' data(tips, package = "reshape")
#' p <- ggplot(tips) +
#'   aes(x = time, y = day) +
#'   geom_count() +
#'   theme_light()
#'
#' p_(p)
#' p_(p + geom_stripped_rows())
#' p_(p + geom_stripped_cols())
#' p_(p + geom_stripped_rows() + geom_stripped_cols())
#'
#' p <- ggplot(tips) +
#'   aes(x = total_bill, y = day) +
#'   geom_count() +
#'   theme_light()
#' p
#' p_(p + geom_stripped_rows())
#' p_(p + geom_stripped_rows() + scale_y_discrete(expand = expansion(0, 0.5)))
#' p_(p + geom_stripped_rows(xfrom = 10, xto = 35))
#' p_(p + geom_stripped_rows(odd = "blue", even = "yellow"))
#' p_(p + geom_stripped_rows(odd = "blue", even = "yellow", alpha = .1))
#' p_(p + geom_stripped_rows(odd = "#00FF0022", even = "#FF000022"))
#'
#' p_(p + geom_stripped_cols())
#' p_(p + geom_stripped_cols(width = 10))
#' p_(p + geom_stripped_cols(width = 10, nudge_x = 5))
geom_stripped_rows <- function(mapping = NULL,
                               data = NULL,
                               stat = "identity",
                               position = "identity",
                               ...,
                               show.legend = NA,
                               inherit.aes = TRUE,
                               xfrom = -Inf,
                               xto = Inf,
                               width = 1,
                               nudge_y = 0) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomStrippedRows,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(xfrom = xfrom, xto = xto, width = width, nudge_y = nudge_y, ...)
  )
}

GeomStrippedRows <- ggplot2::ggproto("GeomStrippedRows", ggplot2::Geom,
  required_aes = c("y"),

  default_aes = ggplot2::aes(
    odd = "#11111111", even = "#00000000",
    alpha = NA, colour = NA, linetype = "solid", size = .5
  ),

  # draw_key = ggplot2::draw_key_blank,
  draw_key = ggplot2::draw_key_rect,

  draw_panel = function(data, panel_params, coord, xfrom, xto, width = 1, nudge_y = 0) {
    ggplot2::GeomRect$draw_panel(
      data %>%
        dplyr::mutate(
          y = plyr::round_any(.data$y, width),
          ymin = .data$y - width / 2 + nudge_y,
          ymax = .data$y + width / 2 + nudge_y,
          xmin = xfrom,
          xmax = xto
        ) %>%
        dplyr::select(
          .data$xmin, .data$xmax,
          .data$ymin, .data$ymax,
          .data$odd, .data$even,
          .data$alpha, .data$colour, .data$linetype, .data$size
        ) %>%
        dplyr::distinct(.data$ymin, .keep_all = TRUE) %>%
        dplyr::arrange(.data$ymin) %>%
        dplyr::mutate(
          .n = dplyr::row_number(),
          fill = dplyr::if_else(
            .data$.n %% 2L == 1L,
            true = .data$odd,
            false = .data$even
          )
        ) %>%
        dplyr::select(-.data$.n, -.data$odd, -.data$even),
      panel_params,
      coord
    )
  }
)

#' @rdname geom_stripped_rows
#' @param yfrom,yto limitation of the strips along the y-axis
#' @export
geom_stripped_cols <- function(mapping = NULL,
                               data = NULL,
                               stat = "identity",
                               position = "identity",
                               ...,
                               show.legend = NA,
                               inherit.aes = TRUE,
                               yfrom = -Inf,
                               yto = Inf,
                               width = 1,
                               nudge_x = 0) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomStrippedCols,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(yfrom = yfrom, yto = yto, width = width, nudge_x = nudge_x, ...)
  )
}

GeomStrippedCols <- ggplot2::ggproto("GeomStrippedCols", ggplot2::Geom,
  required_aes = c("y"),

  default_aes = ggplot2::aes(
    odd = "#11111111", even = "#00000000",
    alpha = NA, colour = NA, linetype = "solid", size = .5
  ),

  # draw_key = ggplot2::draw_key_blank,
  draw_key = ggplot2::draw_key_rect,

  draw_panel = function(data, panel_params, coord, yfrom, yto, width = 1, nudge_x = 0) {
    ggplot2::GeomRect$draw_panel(
      data %>%
        dplyr::mutate(
          x = plyr::round_any(.data$x, width),
          xmin = .data$x - width / 2 + nudge_x,
          xmax = .data$x + width / 2 + nudge_x,
          ymin = yfrom,
          ymax = yto
        ) %>%
        dplyr::select(
          .data$xmin, .data$xmax,
          .data$ymin, .data$ymax,
          .data$odd, .data$even,
          .data$alpha, .data$colour, .data$linetype, .data$size
        ) %>%
        dplyr::distinct(.data$xmin, .keep_all = TRUE) %>%
        dplyr::arrange(.data$xmin) %>%
        dplyr::mutate(
          .n = dplyr::row_number(),
          fill = dplyr::if_else(
            .data$.n %% 2L == 1L,
            true = .data$odd,
            false = .data$even
          )
        ) %>%
        dplyr::select(-.data$.n, -.data$odd, -.data$even),
      panel_params,
      coord
    )
  }
)
