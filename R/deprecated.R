#' Modify a \code{\link{ggmatrix}} object by adding an \pkg{ggplot2} object to all
#'
# \lifecycle{deprecated}
#'
#' @export
#' @examples
#'
#' ggpairs(iris, 1:2) + v1_ggmatrix_theme()
#' # move the column names to the left and bottom
#' ggpairs(iris, 1:2, switch = "both") + v1_ggmatrix_theme()
v1_ggmatrix_theme <- function() {
  theme(
    strip.background = element_rect(fill = "white"),
    strip.placement = "outside"
  )
}


#' Correlation value plot
#'
# \lifecycle{deprecated}
#'
#' (Deprecated. See \code{\link{ggally_cor}}.)
#'
#' Estimate correlation from the given data.
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param alignPercent right align position of numbers. Default is 60 percent across the horizontal
#' @param method \code{method} supplied to cor function
#' @param use \code{use} supplied to cor function
#' @param corAlignPercent deprecated. Use parameter \code{alignPercent}
#' @param corMethod deprecated. Use parameter \code{method}
#' @param corUse deprecated. Use parameter \code{use}
#' @param displayGrid if TRUE, display aligned panel gridlines
#' @param ... other arguments being supplied to geom_text
#' @author Barret Schloerke
#' @importFrom stats complete.cases cor
#' @seealso \code{\link{ggally_cor}}
#' @export
#' @keywords hplot
#' @examples
#'  data(tips, package = "reshape")
#'  ggally_cor_v1_5(tips, mapping = ggplot2::aes_string(x = "total_bill", y = "tip"))
#'  # display with no grid
#'  ggally_cor_v1_5(
#'    tips,
#'    mapping = ggplot2::aes_string(x = "total_bill", y = "tip"),
#'    displayGrid = FALSE
#'  )
#'  # change text attributes
#'  ggally_cor_v1_5(
#'    tips,
#'    mapping = ggplot2::aes(x = total_bill, y = tip),
#'    size = 15,
#'    colour = I("red")
#'  )
#'  # split by a variable
#'  ggally_cor_v1_5(
#'    tips,
#'    mapping = ggplot2::aes_string(x = "total_bill", y = "tip", color = "sex"),
#'    size = 5
#'  )
ggally_cor_v1_5 <- function(
  data,
  mapping,
  alignPercent = 0.6,
  method = "pearson", use = "complete.obs",
  corAlignPercent = NULL, corMethod = NULL, corUse = NULL,
  displayGrid = TRUE,
  ...
){

  if (! is.null(corAlignPercent)) {
    stop("'corAlignPercent' is deprecated.  Please use argument 'alignPercent'")
  }
  if (! is.null(corMethod)) {
    stop("'corMethod' is deprecated.  Please use argument 'method'")
  }
  if (! is.null(corUse)) {
    stop("'corUse' is deprecated.  Please use argument 'use'")
  }

  useOptions <- c(
    "all.obs",
    "complete.obs",
    "pairwise.complete.obs",
    "everything",
    "na.or.complete"
  )
  use <- pmatch(use, useOptions)
  if (is.na(use)) {
    warning("correlation 'use' not found.  Using default value of 'all.obs'")
    use <- useOptions[1]
  } else {
    use <- useOptions[use]
  }

  cor_fn <- function(x, y) {
    # also do ddply below if fn is altered
    cor(x, y, method = method, use = use)
  }

  # xVar <- data[[as.character(mapping$x)]]
  # yVar <- data[[as.character(mapping$y)]]
  # x_bad_rows <- is.na(xVar)
  # y_bad_rows <- is.na(yVar)
  # bad_rows <- x_bad_rows | y_bad_rows
  # if (any(bad_rows)) {
  #   total <- sum(bad_rows)
  #   if (total > 1) {
  #     warning("Removed ", total, " rows containing missing values")
  #   } else if (total == 1) {
  #     warning("Removing 1 row that contained a missing value")
  #   }
  #
  #   xVar <- xVar[!bad_rows]
  #   yVar <- yVar[!bad_rows]
  # }

  # mapping$x <- mapping$y <- NULL

  xData <- eval_data_col(data, mapping$x)
  yData <- eval_data_col(data, mapping$y)

  if (is_date(xData)) {
    xData <- as.numeric(xData)
  }
  if (is_date(yData)) {
    yData <- as.numeric(yData)
  }
  colorData <- eval_data_col(data, mapping$colour)
  if (is.numeric(colorData)) {
    stop("ggally_cor: mapping color column must be categorical, not numeric")
  }

  if (use %in% c("complete.obs", "pairwise.complete.obs", "na.or.complete")) {
    if (!is.null(colorData) && (length(colorData) == length(xData))) {
      rows <- complete.cases(xData, yData, colorData)
    } else {
      rows <- complete.cases(xData, yData)
    }

    if (any(!rows)) {
      total <- sum(!rows)
      if (total > 1) {
        warning("Removed ", total, " rows containing missing values")
      } else if (total == 1) {
        warning("Removing 1 row that contained a missing value")
      }
    }

    if (!is.null(colorData) && (length(colorData) == length(xData))) {
      colorData <- colorData[rows]
    }
    xData <- xData[rows]
    yData <- yData[rows]
  }

  xVal <- xData
  yVal <- yData

  # if the mapping has to deal with the data, remove it
  if (packageVersion("ggplot2") > "2.2.1") {
    for (mappingName in names(mapping)) {
      itemData <- eval_data_col(data, mapping[[mappingName]])
      if (!inherits(itemData, "AsIs")) {
        mapping[[mappingName]] <- NULL
      }
    }
  } else {
    if (length(names(mapping)) > 0){
      for (i in length(names(mapping)):1){
        # find the last value of the aes, such as cyl of as.factor(cyl)
        tmp_map_val <- deparse(mapping[names(mapping)[i]][[1]])
        if (tmp_map_val[length(tmp_map_val)] %in% colnames(data))
          mapping[[names(mapping)[i]]] <- NULL

        if (length(names(mapping)) < 1){
          mapping <- NULL
          break;
        }
      }
    }
  }

  if (
    !is.null(colorData) &&
    !inherits(colorData, "AsIs")
  ) {

    cord <- ddply(
      data.frame(x = xData, y = yData, color = colorData),
      "color",
      function(dt) {
        cor_fn(dt$x, dt$y)
      }
    )
    colnames(cord)[2] <- "correlation"

    cord$correlation <- signif(as.numeric(cord$correlation), 3)

    # put in correct order
    lev <- levels(as.factor(colorData))
    ord <- rep(-1, nrow(cord))
    for (i in 1:nrow(cord)) {
      for (j in seq_along(lev)){
        if (identical(as.character(cord$color[i]), as.character(lev[j]))) {
          ord[i] <- j
        }
      }
    }

    # print(order(ord[ord >= 0]))
    # print(lev)
    cord <- cord[order(ord[ord >= 0]), ]
    cord$label <- str_c(cord$color, ": ", cord$correlation)

    # calculate variable ranges so the gridlines line up
    xmin <- min(xVal, na.rm = TRUE)
    xmax <- max(xVal, na.rm = TRUE)
    xrange <- c(xmin - 0.01 * (xmax - xmin), xmax + 0.01 * (xmax - xmin))
    ymin <- min(yVal, na.rm = TRUE)
    ymax <- max(yVal, na.rm = TRUE)
    yrange <- c(ymin - 0.01 * (ymax - ymin), ymax + 0.01 * (ymax - ymin))


    # print(cord)
    p <- ggally_text(
      label   = str_c("Corr: ", signif(cor_fn(xVal, yVal), 3)),
      mapping = mapping,
      xP      = 0.5,
      yP      = 0.9,
      xrange  = xrange,
      yrange  = yrange,
      ...
    )

    xPos <- rep(alignPercent, nrow(cord)) * diff(xrange) + min(xrange, na.rm = TRUE)
    yPos <- seq(
      from = 0.9,
      to = 0.2,
      length.out = nrow(cord) + 1)
    yPos <- yPos * diff(yrange) + min(yrange, na.rm = TRUE)
    yPos <- yPos[-1]
    # print(range(yVal))
    # print(yPos)

    cordf <- data.frame(xPos = xPos, yPos = yPos, labelp = cord$label)
    cordf$labelp <- factor(cordf$labelp, levels = cordf$labelp)
    # print(cordf)
    # print(str(cordf))

    p <- p + geom_text(
      data = cordf,
      aes(
        x = xPos,
        y = yPos,
        label = labelp,
        color = labelp
      ),
      hjust = 1,
      ...

    )
  } else {
    # calculate variable ranges so the gridlines line up
    xmin <- min(xVal, na.rm = TRUE)
    xmax <- max(xVal, na.rm = TRUE)
    xrange <- c(xmin - 0.01 * (xmax - xmin), xmax + 0.01 * (xmax - xmin))
    ymin <- min(yVal, na.rm = TRUE)
    ymax <- max(yVal, na.rm = TRUE)
    yrange <- c(ymin - 0.01 * (ymax - ymin), ymax + 0.01 * (ymax - ymin))

    p <- ggally_text(
      label = paste(
        "Corr:\n",
        signif(
          cor_fn(xVal, yVal),
          3
        ),
        sep = "", collapse = ""
      ),
      mapping,
      xP = 0.5,
      yP = 0.5,
      xrange = xrange,
      yrange = yrange,
      ...
    )
  }

  if (!isTRUE(displayGrid)) {
    p <- p +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  }

  p + theme(legend.position = "none")
}
