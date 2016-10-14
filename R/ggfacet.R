#' ggfacet - single ggplot2 plot matrix with facet_grid
#'
#'
#' @param data data.frame that contains all columns to be displayed.  This data will be melted before being passed into the function \code{fn}
#' @param mapping aesthetic mapping (besides \code{x} and \code{y}).  See \code{\link[ggplot2]{aes}()}
#' @param fn function to be executed. Similar to \code{\link{ggpairs}} and \code{\link{ggduo}}, the function may either be a string identifier or a real function that \code{\link{wrap}} understands.
#' @param ... extra arguments passed directly to \code{fn}
#' @param columnsX columns to be displayed in the plot matrix
#' @param columnsY rows to be displayed in the plot matrix
#' @param columnLabelsX,columnLabelsY column and row labels to display in the plot matrix
#' @param xlab,ylab,title plot matrix labels
#' @param scales parameter supplied to \code{ggplot2::\link[ggplot2]{facet_grid}}. Default behavior is \code{"free"}
#' @export
#' @examples
#' # Small function to display plots only if it's interactive
#' p_ <- GGally::print_if_interactive
#' library(chemometrics)
#' data(NIR)
#' NIR_sub <- data.frame(NIR$yGlcEtOH, NIR$xNIR[,1:3])
#' str(NIR_sub)
#' x_cols <- c("X1115.0", "X1120.0", "X1125.0")
#' y_cols <- c("Glucose", "Ethanol")
#'
#' # using ggduo directly
#' p <- ggduo(NIR_sub, x_cols, y_cols, types = list(continuous = "points"))
#' p_(p)
#'
#' # using ggfacet
#' p <- ggfacet(NIR_sub, x_cols, y_cols)
#' p_(p)
#'
#' # add a smoother
#' p <- ggfacet(NIR_sub, x_cols, y_cols, fn = 'smooth_loess')
#' p_(p)
#' # same output
#' p <- ggfacet(NIR_sub, x_cols, y_cols, fn = ggally_smooth_loess)
#' p_(p)
#'
#' # Change scales to be the same in for every row and for every column
#' p <- ggfacet(NIR_sub, x_cols, y_cols, scales = "fixed")
#' p_(p)
ggfacet <- function(
  data, mapping = NULL,
  columnsX = 1:ncol(data),
  columnsY = 1:ncol(data),
  fn = ggally_points, ...,
  columnLabelsX = names(data[columnsX]),
  columnLabelsY = names(data[columnsY]),
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  scales = "free"
) {

  data <- fix_data(data)
  fn <- wrap(fn)

  # fix args
  if (
    !missing(mapping) & !is.list(mapping) &
    !missing(columnsX) & missing(columnsY)
  ) {
    columnsY <- columnsX
    columnsX <- mapping
    mapping <- NULL
  }

  stop_if_bad_mapping(mapping)

  columnsX <- fix_column_values(data, columnsX, columnLabelsX, "columnsX", "columnLabelsX")
  columnsY <- fix_column_values(data, columnsY, columnLabelsY, "columnsY", "columnLabelsY")

  # could theoretically work like
  # mtc <- mtcars
  # mtc$am <- as.factor(mtc$am)
  # mtc$cyl <- as.factor(mtc$cyl)
  # ggfacet(
  #   mtc,
  #   columnsY = c(1,3,4,5), columnsX = c("am", "cyl"),
  #   fn = function(data, mapping){ggplot(data, mapping) + geom_boxplot()}
  # )
  is_factor_x <- sapply(data[columnsX], is.factor)
  if (sum(is_factor_x) != 0) {
    warning(paste(sum(is_factor_x), " factor variables are being removed from X columns", sep = ""))
    columnsX <- columnsX[!is_factor_x]
    columnLabelsX <- columnLabelsX[!is_factor_x]
  }
  is_factor_y <- sapply(data[columnsY], is.factor)
  if (sum(is_factor_y) != 0) {
    warning(paste(sum(is_factor_y), " factor variables are being removed from Y columns", sep = ""))
    columnsY <- columnsY[!is_factor_y]
    columnLabelsY <- columnLabelsY[!is_factor_y]
  }

  tall_data <- ddply(
    expand.grid(.x_col = columnsX, .y_col = columnsY),
    c(".x_col", ".y_col"),
    function(row) {
      x_var <- row$.x_col[1]
      y_var <- row$.y_col[1]

      ret <- data
      ret[[".x_val"]] <- data[[x_var]]
      ret[[".y_val"]] <- data[[y_var]]
      ret
    }
  )

  if (is.null(mapping)) {
    mapping <- aes()
  }
  mapping[c("x", "y")] <- aes_string(x = ".x_val", y = ".y_val")

  names(columnLabelsX) <- as.character(columnsX)
  names(columnLabelsY) <- as.character(columnsY)
  labeller <- function(vals) {
    val_names <- names(vals)
    if (".x_col" %in% val_names) {
      vals[[".x_col"]] <- columnLabelsX[as.character(vals[[".x_col"]])]
    }
    if (".y_col" %in% val_names) {
      vals[[".y_col"]] <- columnLabelsY[as.character(vals[[".y_col"]])]
    }
    vals
  }
  p <- fn(tall_data, mapping, ...) +
    facet_grid(.y_col ~ .x_col, labeller = labeller, scales = scales) +
    labs(title = title, x = xlab, y = ylab)

  p
}
