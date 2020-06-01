#' Plot an outcome with several potential explanotory variables
#'
#' \code{ggtable} is a variant of \code{\link{ggduo}} for plotting
#' an outcome variable with several potential explanatory variables.
#'
#' @param data dataset to be used, can have both categorical and
#'   numerical variables
#' @param columnsX,columnsY names or positions of which columns are used to make plots. Defaults to all columns.
#' @param cells Which statistic should be displayed in table cells?
#' @param fill Wich statistic should be used for filling table cells?
#' @param mapping additional aesthetic to be used, for example to indicate
#'   weights (see examples)
#' @param ... additional arguments passed to \code{\link{ggduo}} (see examples)
#' @author Joseph Larmarange \email{joseph@@larmarange.net}
#' @export
#' @examples
#' data(tips, package = "reshape")
#' ggtable(tips, "smoker", c("day", "time", "sex"))
#'
#' # displaying row proportions
#' ggtable(tips, "smoker", c("day", "time", "sex"), cells = "row.prop")
#'
#' # filling cells with standardized residuals
#' ggtable(tips, "smoker", c("day", "time", "sex"), fill = "stdres")
#'
#' # if continuous variables are provided, just displaying some summary statistics
#' ggtable(tips, c("smoker", "total_bill"), c("day", "time", "sex", "tip"))
ggtable <- function(
  data,
  columnsX = 1:ncol(data),
  columnsY = 1:ncol(data),
  cells = c("observed", "prop", "row.prop", "col.prop", "expected", "residuals", "stdres"),
  fill = c("none", "stdres", "residuals"),
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

  if (!"xProportions" %in% names(ggduo_args))
    ggduo_args$xProportions <- "auto"
  if (!"yProportions" %in% names(ggduo_args))
    ggduo_args$yProportions <- "auto"
  if (!"legend" %in% names(ggduo_args) & fill == "stdres")
    ggduo_args$legend <- 1

  p <- do.call(ggduo, ggduo_args)
  p
}
