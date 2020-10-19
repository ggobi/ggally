#' Display an outcome using several potential explanatory variables
#'
#' \code{ggbivariate} is a variant of \code{\link{ggduo}} for plotting
#' an outcome variable with several potential explanatory variables.
#'
#' @param data dataset to be used, can have both categorical and
#'   numerical variables
#' @param outcome name or position of the outcome variable (one variable only)
#' @param explanatory names or positions of the explanatory variables (if \code{NULL},
#'   will take all variables other than \code{outcome})
#' @param mapping additional aesthetic to be used, for example to indicate
#'   weights (see examples)
#' @param types custom types of plots to use, see \code{\link{ggduo}}
#' @param ... additional arguments passed to \code{\link{ggduo}} (see examples)
#' @param rowbar_args additional arguments passed to \code{\link{ggally_rowbar}}
#'   (see examples)
#' @author Joseph Larmarange
#' @export
#' @examples
#' # Small function to display plots only if it's interactive
#' p_ <- GGally::print_if_interactive
#'
#' data(tips, package = "reshape")
#' p_(ggbivariate(tips, "smoker", c("day", "time", "sex", "tip")))
#'
#' # Personalize plot title and legend title
#' p_(ggbivariate(
#'   tips, "smoker", c("day", "time", "sex", "tip"),
#'   title = "Custom title"
#' ) +
#'   labs(fill = "Smoker ?"))
#'
#' # Customize fill colour scale
#' p_(ggbivariate(tips, "smoker", c("day", "time", "sex", "tip")) +
#'   scale_fill_brewer(type = "qual"))
#'
#' # Customize labels
#' p_(ggbivariate(
#'   tips, "smoker", c("day", "time", "sex", "tip"),
#'   rowbar_args = list(
#'     colour = "white",
#'     size = 4,
#'     fontface = "bold",
#'     label_format = scales::label_percent(accurary = 1)
#'   )
#' ))
#'
#' # Choose the sub-plot from which get legend
#' p_(ggbivariate(tips, "smoker"))
#' p_(ggbivariate(tips, "smoker", legend = 3))
#'
#' # Use mapping to indicate weights
#' d <- as.data.frame(Titanic)
#' p_(ggbivariate(d, "Survived", mapping = aes(weight = Freq)))
#'
#' # outcome can be numerical
#' p_(ggbivariate(tips, outcome = "tip", title = "tip"))
ggbivariate <- function(
  data,
  outcome,
  explanatory = NULL,
  mapping = NULL, types = NULL,
  ...,
  rowbar_args = NULL
) {
  if (length(outcome) != 1)
    stop("You should provide only one `outcome`.")
  if (is.numeric(outcome))
    outcome <- names(data)[outcome]
  if (is.null(explanatory))
    explanatory <- names(data)[!names(data) %in% c(outcome, mapping_string(mapping$weight))]

  if (!is.numeric(data[[outcome]])) {
    # mapping outcome to colour
    mapping$colour <- aes_string(colour = outcome)$colour
  }

  # default behavior
  if (is.null(rowbar_args$remove_percentage_axis))
    rowbar_args$remove_percentage_axis <- TRUE
  if (is.null(rowbar_args$remove_background))
    rowbar_args$remove_background <- TRUE
  if (is.null(types$discrete))
    types$discrete = wrapp(ggally_rowbar, rowbar_args)
  if (is.null(types$comboVertical))
    types$comboVertical <- "box_no_facet"
  if (is.null(types$continuous))
    types$continuous <- "smooth_lm"
  if (is.null(types$comboHorizontal))
    types$comboHorizontal <- "box_no_facet"

  ggduo_args <- list(...)
  ggduo_args$data <- data
  ggduo_args$mapping <- mapping
  ggduo_args$types <- types
  ggduo_args$columnsX <- outcome
  ggduo_args$columnsY <- explanatory

  if (!"yProportions" %in% names(ggduo_args))
    ggduo_args$yProportions <- "auto"

  if (!is.numeric(data[[outcome]]) & !"legend" %in% names(list(...)))
    ggduo_args$legend <- 1

  p <- do.call(ggduo, ggduo_args) +
    theme(
      legend.position = "top",
      strip.text.x = element_blank()
    )

  p
}
