#' Compute proportions according to custom denominator
#'
#' \code{stat_prop} is a variation of \code{\link[ggplot2]{stat_count}} allowing to compute custom
#' proportions according to the \strong{by} aesthetic defining the denominator
#' (i.e. all proportions for a same value of \strong{by} will sum to 1).
#' The \code{by} aesthetic should be a factor.
#'
#' @inheritParams ggplot2::stat_count
#' @param geom Override the default connection between \code{\link[ggplot2]{geom_bar}}
#'   and \code{stat_prop}.
#' @section Aesthetics:
#' \code{stat_prop} requires the \strong{by} aesthetic and the \code{by} aesthetic
#' should be a factor.
#' @section Computed variables:
#' \describe{
#'   \item{count}{number of points in bin}
#'   \item{prop}{computed proportion}
#' }
#' @seealso \code{\link[ggplot2]{stat_count}}
#'
#' @import ggplot2
#' @importFrom scales percent
#' @export
#' @examples
#'
#' d <- as.data.frame(Titanic)
#'
#' p <- ggplot(d) +
#'   aes(x = Class, fill = Survived, weight = Freq, by = Class) +
#'   geom_bar(position = "fill") +
#'   geom_text(stat = "prop", position = position_fill(.5))
#' p
#' p + facet_grid(~ Sex)
#'
#' ggplot(d) +
#'   aes(x = Class, fill = Survived, weight = Freq) +
#'   geom_bar(position = "dodge") +
#'   geom_text(
#'     aes(by = Survived), stat = "prop",
#'     position = position_dodge(0.9), vjust = "bottom"
#'  )
#'
#' ggplot(d) +
#'   aes(x = Class, fill = Survived, weight = Freq, by = 1) +
#'   geom_bar() +
#'   geom_text(
#'     aes(label = scales::percent(after_stat(prop), accuracy = 1)),
#'     stat = "prop",
#'     position = position_stack(.5)
#'  )
#'
stat_prop <- function(mapping = NULL, data = NULL,
                       geom = "bar", position = "fill",
                       ...,
                       width = NULL,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {

  params <- list(
    na.rm = na.rm,
    width = width,
    ...
  )
  if (!is.null(params$y)) {
    stop("stat_prop() must not be used with a y aesthetic.", call. = FALSE)
  }

  layer(
    data = data,
    mapping = mapping,
    stat = StatProp,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
}

#' @rdname stat_prop
#' @format NULL
#' @usage NULL
#' @export
StatProp <- ggproto("StatProp", Stat,
  required_aes = c("x", "by"),
  default_aes = aes(
    y = stat(count), weight = 1,
    label = scales::percent(after_stat(prop), accuracy = .1)
  ),

  setup_params = function(data, params) {
    if (!is.null(data$y)) {
      stop("stat_prop() must not be used with a y aesthetic.", call. = FALSE)
    }
    # there is an unresolved bug when by is a character vector. To be explored.
    if (is.character(data$by)) {
      stop("The by aesthetic should be a factor instead of a character vector.", call. = FALSE)
    }
    params
  },

  extra_params = c("na.rm"),

  compute_panel = function(self, data, scales, width = NULL) {
   data$weight <- data$weight %||% rep(1, nrow(data))
   width <- width %||% (ggplot2::resolution(data$x) * 0.9)

   # sum weights for each combination of by and aesthetics
   # the use of . allows to consider all aesthetics defined in data
   panel <- aggregate(weight ~ ., data = data, sum, na.rm = TRUE)

   names(panel)[which(names(panel) == "weight")] <- "count"
   panel$count[is.na(panel$count)] <- 0

   # compute proportions by by
   f <- function(x) {sum(abs(x))}
   panel$prop <- panel$count / ave(panel$count, panel$by, FUN = f)
   panel$width <- width

   panel
  }
)


"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

