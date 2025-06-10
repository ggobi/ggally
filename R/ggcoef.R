#' Model coefficients with \pkg{broom} and \pkg{ggplot2}
#'
#' Plot the coefficients of a model with \pkg{broom} and \pkg{ggplot2}.
#' For an updated and improved version, see [ggcoef_model()].
#'
#' @param x a model object to be tidied with [broom::tidy()] or a data frame (see Details)
#' @param mapping default aesthetic mapping
#' @param conf.int display confidence intervals as error bars?
#' @param conf.level level of confidence intervals (passed to [broom::tidy()]
#'   if \code{x} is not a data frame)
#' @param exponentiate if \code{TRUE}, x-axis will be logarithmic (also passed to [broom::tidy()]
#'   if \code{x} is not a data frame)
#' @param exclude_intercept should the intercept be excluded from the plot?
#' @param vline print a vertical line?
#' @param vline_intercept \code{xintercept} for the vertical line.
#' \code{"auto"} for \code{x = 0} (or \code{x = 1} if \code{exponentiate} is \code{TRUE})
#' @param vline_color color of the vertical line
#' @param vline_linetype line type of the vertical line
#' @param vline_size size of the vertical line
#' @param errorbar_color color of the error bars
#' @param errorbar_height height of the error bars
#' @param errorbar_linetype line type of the error bars
#' @param errorbar_size size of the error bars
#' @param sort \code{"none"} (default) do not sort, \code{"ascending"} sort by increasing coefficient value, or \code{"descending"} sort by decreasing coefficient value
#' @param ... additional arguments sent to [ggplot2::geom_point()]
#' @examples
#' # Small function to display plots only if it's interactive
#' p_ <- GGally::print_if_interactive
#'
#' library(broom)
#' reg <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = iris)
#' p_(ggcoef(reg))
#' \donttest{
#' d <- as.data.frame(Titanic)
#' reg2 <- glm(Survived ~ Sex + Age + Class, family = binomial, data = d, weights = d$Freq)
#' ggcoef(reg2, exponentiate = TRUE)
#' ggcoef(
#'   reg2,
#'   exponentiate = TRUE, exclude_intercept = TRUE,
#'   errorbar_height = .2, color = "blue", sort = "ascending"
#' )
#' }
#' @export
ggcoef <- function(
    x,
    mapping = aes(!!as.name("estimate"), !!as.name("term")),
    conf.int = TRUE,
    conf.level = 0.95,
    exponentiate = FALSE,
    exclude_intercept = FALSE,
    vline = TRUE,
    vline_intercept = "auto",
    vline_color = "gray50",
    vline_linetype = "dotted",
    vline_size = 1,
    errorbar_color = "gray25",
    errorbar_height = 0,
    errorbar_linetype = "solid",
    errorbar_size = .5,
    sort = c("none", "ascending", "descending"),
    ...) {
  if (!is.data.frame(x)) {
    require_namespaces("broom")
    x <- broom::tidy(
      x,
      conf.int = conf.int,
      conf.level = conf.level,
      exponentiate = exponentiate
    )
  }
  if (!("term" %in% names(x))) {
    stop("x doesn't contain a column names 'term'.")
  }
  if (!("estimate" %in% names(x))) {
    stop("x doesn't contain a column names 'estimate'.")
  }
  if (exclude_intercept) {
    x <- x[x$term != "(Intercept)", ]
  }

  sort <- match.arg(sort)
  if (sort != "none") {
    x$term <- as.factor(x$term)
    if (sort == "ascending") {
      new_order <- order(x$estimate, decreasing = FALSE)
    } else {
      new_order <- order(x$estimate, decreasing = TRUE)
    }
    x$term <- as.character(x$term)
    x$term <- factor(x$term, levels = x$term[new_order])
  }

  p <- ggplot(x, mapping = mapping)

  if (vline) {
    if (exponentiate) {
      if (vline_intercept == "auto") {
        vline_intercept <- 1
      }
      p <- p +
        geom_vline(
          xintercept = vline_intercept, color = vline_color,
          linetype = vline_linetype, linewidth = vline_size
        ) +
        scale_x_log10()
    } else {
      if (vline_intercept == "auto") {
        vline_intercept <- 0
      }
      p <- p +
        geom_vline(
          xintercept = vline_intercept,
          color = vline_color,
          linetype = vline_linetype,
          linewidth = vline_size
        )
    }
  }
  if (conf.int && "conf.low" %in% names(x) && "conf.high" %in% names(x)) {
    p <- p + geom_errorbar(
      aes(xmin = !!as.name("conf.low"), xmax = !!as.name("conf.high")),
      color = errorbar_color,
      width = errorbar_height,
      linetype = errorbar_linetype,
      linewidth = errorbar_size
    )
  }
  p + geom_point(...)
}
