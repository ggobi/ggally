#' ggcoef - Plot Model Coefficients with broom and ggplot2
#'
#' Plot the coefficients of a model with \pkg{broom} and \pkg{ggplot2}.
#'
#' @param x a model object to be tidied with \code{\link[broom]{tidy}} or a data frame (see Details)
#' @param mapping default aesthetic mapping
#' @param conf.int display confidence intervals as error bars?
#' @param conf.level level of confidence intervals (passed to \code{\link[broom]{tidy}}
#'   if \code{x} is not a data frame)
#' @param exponentiate if \code{TRUE}, x-axis will be logarithmic (also passed to \code{\link[broom]{tidy}}
#'   if \code{x} is not a data frame)
#' @param exclude_intercept should the intercept be excluded from the plot?
#' @param vline print a vertical line?
#' @param vline_intercept \code{xintercept} for the vertical line.
#' \code{"auto"} for \code{x = 0} (or \code{x = 1} if {exponentiate} is \code{TRUE})
#' @param vline_color color of the vertical line
#' @param vline_linetype line type of the vertical line
#' @param vline_size size of the vertical line
#' @param errorbar_color color of the error bars
#' @param errorbar_height height of the error bars
#' @param errorbar_linetype line type of the error bars
#' @param errorbar_size size of the error bars
#' @param ... additional arguments sent to \code{\link[ggplot2]{geom_point}}
#' @examples
#' library(broom)
#' reg <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = iris)
#' ggcoef(reg)
#'
#' d <- as.data.frame(Titanic)
#' reg2 <- glm(Survived ~ Sex + Age + Class, family = binomial, data = d, weights = d$Freq)
#' ggcoef(reg2, exponentiate = TRUE)
#' ggcoef(reg2, exponentiate = TRUE, exclude_intercept = TRUE, errorbar_height = .2, color = "blue")
#' @export
ggcoef <- function(
  x,
  mapping = aes_string(y = "term", x = "estimate"),
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
  ...
) {
  if (!is.data.frame(x)) {
    require_pkgs("broom")
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

  p <- ggplot(x, mapping = mapping)

  if (vline) {
    if (exponentiate) {
      if (vline_intercept == "auto") {
        vline_intercept <- 1
      }
      p <- p +
        geom_vline(
          xintercept = vline_intercept, color = vline_color,
          linetype = vline_linetype, size = vline_size
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
          size = vline_size
        )
    }
  }
  if (conf.int & "conf.low" %in% names(x) & "conf.high" %in% names(x))
    p <- p + geom_errorbarh(
      aes_string(xmin = "conf.low", xmax = "conf.high"),
      color = errorbar_color,
      height = errorbar_height,
      linetype = errorbar_linetype,
      size = errorbar_size
    )
  p + geom_point(...)
}
