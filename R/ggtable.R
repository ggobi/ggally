#' Cross-tabulated tables of discrete variables
#'
#' \code{ggtable} is a variant of \code{\link{ggduo}} for quick
#' cross-tabulated tables of discrete variables.
#'
#' @param data dataset to be used, can have both categorical and
#'   numerical variables
#' @param columnsX,columnsY names or positions of which columns are used to make plots. Defaults to all columns.
#' @param cells Which statistic should be displayed in table cells?
#' @param fill Which statistic should be used for filling table cells?
#' @param mapping additional aesthetic to be used, for example to indicate
#'   weights (see examples)
#' @param ... additional arguments passed to \code{\link{ggduo}} (see examples)
#' @author Joseph Larmarange
#' @export
#' @examples
#' # small function to display plots only if it's interactive
#' p_ <- GGally::print_if_interactive
#'
#' data(tips)
#' p_(ggtable(tips, "smoker", c("day", "time", "sex")))
#'
#' # displaying row proportions
#' p_(ggtable(tips, "smoker", c("day", "time", "sex"), cells = "row.prop"))
#'
#' # filling cells with standardized residuals
#' p_(ggtable(tips, "smoker", c("day", "time", "sex"), fill = "std.resid", legend = 1))
#'
#' # if continuous variables are provided, just displaying some summary statistics
#' p_(ggtable(tips, c("smoker", "total_bill"), c("day", "time", "sex", "tip")))
#'
#' # specifying weights
#' d <- as.data.frame(Titanic)
#' p_(ggtable(
#'   d,
#'   "Survived",
#'   c("Class", "Sex", "Age"),
#'   mapping = aes(weight = Freq),
#'   cells = "row.prop",
#'   fill = "std.resid"
#' ))
ggtable <- function(
  data,
  columnsX = 1:ncol(data),
  columnsY = 1:ncol(data),
  cells = c(
    "observed",
    "prop",
    "row.prop",
    "col.prop",
    "expected",
    "resid",
    "std.resid"
  ),
  fill = c("none", "std.resid", "resid"),
  mapping = NULL,
  ...
) {
  fill <- match.arg(fill)
  cells <- match.arg(cells)

  types <- list(
    discrete = wrapp(ggally_crosstable, list(cells = cells, fill = fill)),
    continuous = "cor",
    comboVertical = "summarise_by",
    comboHorizontal = "summarise_by"
  )

  ggduo_args <- list(...)
  ggduo_args$data <- data
  ggduo_args$mapping <- mapping
  ggduo_args$types <- types
  ggduo_args$columnsX <- columnsX
  ggduo_args$columnsY <- columnsY

  if (!"xProportions" %in% names(ggduo_args)) {
    ggduo_args$xProportions <- "auto"
  }
  if (!"yProportions" %in% names(ggduo_args)) {
    ggduo_args$yProportions <- "auto"
  }

  p <- do.call(ggduo, ggduo_args)
  p
}
