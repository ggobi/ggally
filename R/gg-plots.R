# add global variable
if (getRversion() >= "2.15.1") {
  utils::globalVariables(unique(c(
    "labelp", # cor plot
    c("..density..", "..scaled..", "x"), # facetdensitystrip plot
    c("..scaled..", "x"), #density diagonal plot
    c("x", "y", "lab"), # internal axis plot
    c("x", "y", "result", "freq") # fluctuation plot
  )))
}


# retrieve the evaulated data column given the aes (which could possibly do operations)
eval_data_col <- function(data, aes_col) {
  eval(aes_col, data)
}

# is categories on the left?
is_character_column <- is_horizontal <- function(data, mapping, val = "y") {
  yData <- eval_data_col(data, mapping[[val]])

  is.factor(yData) || is.character(yData)
}

mapping_swap_x_y <- function(mapping) {
  tmp <- mapping$x
  mapping$x <- mapping$y
  mapping$y <- tmp
  mapping
}



#' Plots the Scatter Plot
#'
#' Make a scatter plot with a given data set.
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... other arguments are sent to geom_point
#' @author Barret Schloerke  \email{schloerke@@gmail.com}
#' @export
#' @keywords hplot
#' @examples
#' data(mtcars)
#' ggally_points(mtcars, mapping = ggplot2::aes(x = disp, y = hp))
#' ggally_points(mtcars, mapping = ggplot2::aes_string(x = "disp", y = "hp"))
#' ggally_points(
#'   mtcars,
#'   mapping = ggplot2::aes_string(
#'     x     = "disp",
#'     y     = "hp",
#'     color = "as.factor(cyl)",
#'     size  = "gear"
#'   )
#' )
ggally_points <- function(data, mapping, ...){

  p <- ggplot(data = data, mapping = mapping) + geom_point(...)

  p
}

#' Plots the Scatter Plot with Smoothing
#'
#' Add a smoothed condition mean with a given scatter plot.
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... other arguments to add to geom_point
#' @param method \code{method} parameter supplied to \code{\link[ggplot2]{geom_smooth}}
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @export
#' @keywords hplot
#' @rdname ggally_smooth
#' @examples
#'  data(tips, package = "reshape")
#'  ggally_smooth(tips, mapping = ggplot2::aes(x = total_bill, y = tip))
#'  ggally_smooth(tips, mapping = ggplot2::aes_string(x = "total_bill", y = "tip"))
#'  ggally_smooth(tips, mapping = ggplot2::aes_string(x = "total_bill", y = "tip", color = "sex"))
ggally_smooth <- function(data, mapping, ..., method = "lm"){

  p <- ggplot(data = data, mapping)

  p <- p + geom_point(...)

  if (! is.null(mapping$color) || ! is.null(mapping$colour)) {
    p <- p + geom_smooth(method = method)
  } else {
    p <- p + geom_smooth(method = method, colour = I("black"))
  }

  p
}

#' @export
#' @rdname ggally_smooth
ggally_smooth_loess <- function(data, mapping, ...) {
  ggally_smooth(data = data, mapping = mapping, ..., method = "loess")
}
#' @export
#' @rdname ggally_smooth
ggally_smooth_lm <- function(data, mapping, ...) {
  ggally_smooth(data = data, mapping = mapping, ..., method = "lm")
}

#' Plots the Scatter Density Plot
#'
#' Make a scatter density plot from a given data.
#'
#' The aesthetic "fill" determines whether or not stat_density2d (filled) or geom_density2d (lines) is used.
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... parameters sent to either stat_density2d or geom_density2d
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @export
#' @keywords hplot
#' @examples
#'  data(tips, package = "reshape")
#'  ggally_density(tips, mapping = ggplot2::aes(x = total_bill, y = tip))
#'  ggally_density(tips, mapping = ggplot2::aes_string(x = "total_bill", y = "tip"))
#'  ggally_density(
#'    tips,
#'    mapping = ggplot2::aes_string(x = "total_bill", y = "tip", fill = "..level..")
#'  )
#'  ggally_density(
#'    tips,
#'    mapping = ggplot2::aes_string(x = "total_bill", y = "tip", fill = "..level..")
#'  ) + ggplot2::scale_fill_gradient(breaks = c(0.05, 0.1, 0.15, 0.2))
ggally_density <- function(data, mapping, ...){
  rangeX <- range(eval_data_col(data, mapping$x), na.rm = TRUE)
  rangeY <- range(eval_data_col(data, mapping$y), na.rm = TRUE)

  p <- ggplot(data = data) +
    geom_point(
      data = data.frame(rangeX = rangeX, rangeY = rangeY),
      mapping = aes(x = rangeX, y = rangeY),
      alpha = 0
    )

  if (!is.null(mapping$fill)) {
    p <- p + stat_density2d(mapping = mapping, geom = "polygon", ...)
  } else {
    p <- p + geom_density2d(mapping = mapping, ...)
  }

  p
}

#' Correlation from the Scatter Plot
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
#' @param ... other arguments being supplied to geom_text
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @importFrom stats complete.cases cor
#' @export
#' @keywords hplot
#' @examples
#'  data(tips, package = "reshape")
#'  ggally_cor(tips, mapping = ggplot2::aes_string(x = "total_bill", y = "tip"))
#'  ggally_cor(
#'    tips,
#'    mapping = ggplot2::aes(x = total_bill, y = tip),
#'    size = 15,
#'    colour = I("red")
#'  )
#'  ggally_cor(
#'    tips,
#'    mapping = ggplot2::aes_string(x = "total_bill", y = "tip", color = "sex"),
#'    size = 5
#'  )
ggally_cor <- function(
  data,
  mapping,
  alignPercent = 0.6,
  method = "pearson", use = "complete.obs",
  corAlignPercent = NULL, corMethod = NULL, corUse = NULL,
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

  xCol <- deparse(mapping$x)
  yCol <- deparse(mapping$y)

  if (is_date(data[[xCol]]) || is_date(data[[yCol]])) {

    # make sure it's a data.frame, as data.tables don't work well
    if (! identical(class(data), "data.frame")) {
      data <- fix_data(data)
    }

    for (col in c(xCol, yCol)) {
      if (is_date(data[[col]])) {
        data[[col]] <- as.numeric(data[[col]])
      }
    }
  }

  if (is.numeric(eval_data_col(data, mapping$colour))) {
    stop("ggally_cor: mapping color column must be categorical, not numeric")
  }

  colorCol <- deparse(mapping$colour)
  singleColorCol <- ifelse(is.null(colorCol), NULL, paste(colorCol, collapse = ""))

  if (use %in% c("complete.obs", "pairwise.complete.obs", "na.or.complete")) {
    if (length(colorCol) > 0) {
      if (singleColorCol %in% colnames(data)) {
        rows <- complete.cases(data[c(xCol, yCol, colorCol)])
      } else {
        rows <- complete.cases(data[c(xCol, yCol)])
      }
    } else {
      rows <- complete.cases(data[c(xCol, yCol)])
    }

    if (any(!rows)) {
      total <- sum(!rows)
      if (total > 1) {
        warning("Removed ", total, " rows containing missing values")
      } else if (total == 1) {
        warning("Removing 1 row that contained a missing value")
      }
    }
    data <- data[rows, ]
  }

  xVal <- data[[xCol]]
  yVal <- data[[yCol]]

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


  # splits <- str_c(
  #   as.character(mapping$group), as.character(mapping$colour),
  #   sep = ", ", collapse = ", "
  # )
  # splits <- str_c(colorCol, sep = ", ", collapse = ", ")
  if (length(colorCol) < 1) {
    colorCol <- "ggally_NO_EXIST"
  }

  if (
    (singleColorCol != "ggally_NO_EXIST") &&
    (singleColorCol %in% colnames(data))
  ) {

    cord <- ddply(data, c(colorCol), function(x) {
      cor_fn(x[[xCol]], x[[yCol]])
    })
    colnames(cord)[2] <- "ggally_cor"

    cord$ggally_cor <- signif(as.numeric(cord$ggally_cor), 3)

    # put in correct order
    lev <- levels(data[[colorCol]])
    ord <- rep(-1, nrow(cord))
    for (i in 1:nrow(cord)) {
      for (j in seq_along(lev)){
        if (identical(as.character(cord[i, colorCol]), as.character(lev[j]))) {
          ord[i] <- j
        }
      }
    }

    # print(order(ord[ord >= 0]))
    # print(lev)
    cord <- cord[order(ord[ord >= 0]), ]

    cord$label <- str_c(cord[[colorCol]], ": ", cord$ggally_cor)

    # calculate variable ranges so the gridlines line up
    xmin <- min(xVal, na.rm = TRUE)
    xmax <- max(xVal, na.rm = TRUE)
    xrange <- c(xmin - 0.01 * (xmax - xmin), xmax + 0.01 * (xmax - xmin))
    ymin <- min(yVal, na.rm = TRUE)
    ymax <- max(yVal, na.rm = TRUE)
    yrange <- c(ymin - 0.01 * (ymax - ymin), ymax + 0.01 * (ymax - ymin))


    # print(cord)
    p <- ggally_text(
      label   = str_c("Cor : ", signif(cor_fn(xVal, yVal), 3)),
      mapping = mapping,
      xP      = 0.5,
      yP      = 0.9,
      xrange  = xrange,
      yrange  = yrange,
      color   = "black",
      ...
    ) +
    #element_bw() +
    theme(legend.position = "none")

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

    p
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
    ) +
    #element_bw() +
    theme(legend.position = "none")

    p
  }
}


#' Plots the Box Plot
#'
#' Make a box plot with a given data set. \code{ggally_box_no_facet} will be a single panel plot, while \code{ggally_box} will be a faceted plot

#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... other arguments being supplied to geom_boxplot
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords hplot
#' @export
#' @examples
#'  data(tips, package = "reshape")
#'  ggally_box(tips, mapping = ggplot2::aes(x = total_bill, y = sex))
#'  ggally_box(tips, mapping = ggplot2::aes_string(x = "total_bill", y = "sex"))
#'  ggally_box(
#'    tips,
#'    mapping        = ggplot2::aes_string(y = "total_bill", x = "sex", color = "sex"),
#'    outlier.colour = "red",
#'    outlier.shape  = 13,
#'    outlier.size   = 8
#'  )
ggally_box <- function(data, mapping, ...){
  mapping <- mapping_color_to_fill(mapping)

  ggally_dot_and_box(data, mapping, ..., boxPlot = TRUE)
}
#' @export
#' @rdname ggally_box
ggally_box_no_facet <- function(data, mapping, ...) {
  mapping <- mapping_color_to_fill(mapping)

  ggally_dot_and_box_no_facet(data, mapping, ..., boxPlot = TRUE)
}


#' Plots the Box Plot with Dot
#'
#' Add jittering with the box plot. \code{ggally_dot_no_facet} will be a single panel plot, while \code{ggally_dot} will be a faceted plot
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... other arguments being supplied to geom_jitter
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords hplot
#' @export
#' @examples
#'  data(tips, package = "reshape")
#'  ggally_dot(tips, mapping = ggplot2::aes(x = total_bill, y = sex))
#'  ggally_dot(tips, mapping = ggplot2::aes_string(x = "total_bill", y = "sex"))
#'  ggally_dot(
#'    tips,
#'    mapping = ggplot2::aes_string(y = "total_bill", x = "sex", color = "sex")
#'  )
#'  ggally_dot(
#'    tips,
#'    mapping = ggplot2::aes_string(y = "total_bill", x = "sex", color = "sex", shape = "sex")
#'  ) + ggplot2::scale_shape(solid=FALSE)
ggally_dot <- function(data, mapping, ...){
  ggally_dot_and_box(data, mapping, ..., boxPlot = FALSE)
}
#' @export
#' @rdname ggally_dot
ggally_dot_no_facet <- function(data, mapping, ...) {
  ggally_dot_and_box_no_facet(data, mapping, ..., boxPlot = FALSE)
}


#' Plots either Box Plot or Dot Plots
#'
#' Place box plots or dot plots on the graph
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... parameters passed to either geom_jitter or geom_boxplot
#' @param boxPlot boolean to decide to plot either box plots (TRUE) or dot plots (FALSE)
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords hplot
#' @export
#' @examples
#'  data(tips, package = "reshape")
#'  ggally_dot_and_box(
#'    tips,
#'    mapping = ggplot2::aes(x = total_bill, y = sex, color = sex),
#'    boxPlot = TRUE
#'  )
#'  ggally_dot_and_box(
#'    tips,
#'    mapping = ggplot2::aes(x = total_bill, y = sex, color = sex),
#'    boxPlot = FALSE
#'  )
ggally_dot_and_box <- function(data, mapping, ..., boxPlot = TRUE){

  horizontal <- is_horizontal(data, mapping)

  if (horizontal) {
    mapping <- mapping_swap_x_y(mapping)
  }

  xVal <- deparse(mapping$x)
  mapping$x <- 1

  p <- ggplot(data = data)

  if (boxPlot) {
    p <- p + geom_boxplot(mapping, ...)
  } else {
    p <- p + geom_jitter(mapping, ...)
  }

  if (!horizontal) {
    p <- p +
      facet_grid(paste(". ~ ", xVal, sep = ""), scales = "free_x") +
      theme(panel.spacing = unit(0.1, "lines"))

  } else {
    p <- p +
      coord_flip() +
      theme(
        axis.text.y = element_text(
          angle = 90,
          vjust = 0,
          colour = "grey50"
        )
      ) +
      facet_grid(paste(xVal, " ~ .", sep = "")) +
      theme(panel.spacing = unit(0.1, "lines"))
  }

  p <- p + scale_x_continuous(xVal, labels = "", breaks = 1)

  p
}

ggally_dot_and_box_no_facet <- function(data, mapping, ..., boxPlot = TRUE) {

  horizontal <- is_horizontal(data, mapping)

  if (horizontal) {
    mapping <- mapping_swap_x_y(mapping)
  }

  p <- ggplot(data = data)

  if (boxPlot) {
    p <- p + geom_boxplot(mapping, ...)
  } else {
    p <- p + geom_jitter(mapping, ...)
  }

  if (horizontal) {
    p <- p +
      scale_x_discrete(
        limits = rev(levels(eval_data_col(data, mapping$x)))
      ) +
      coord_flip()
  }

  p
}



#' Plots the Histograms by Faceting
#'
#' Make histograms by displaying subsets of the data in different panels.
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... parameters sent to stat_bin()
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords hplot
#' @export
#' @examples
#'  data(tips, package = "reshape")
#'  ggally_facethist(tips, mapping = ggplot2::aes(x = tip, y = sex))
#'  ggally_facethist(tips, mapping = ggplot2::aes_string(x = "tip", y = "sex"), binwidth = 0.1)
ggally_facethist <- function(data, mapping, ...){

  mapping <- mapping_color_to_fill(mapping)

  horizontal <- is_horizontal(data, mapping)

  if (!horizontal) {
    mapping <- mapping_swap_x_y(mapping)
  }

  xVal <- deparse(mapping$x)
  yVal <- deparse(mapping$y)
  mapping$y <- NULL

  p <- ggplot(data = data, mapping)
  p <- p + stat_bin(...)

  if (horizontal) {
    p <- p +
      facet_grid(paste(yVal, " ~ .", sep = "")) +
      theme(panel.spacing = unit(0.1, "lines"))
  } else {
    p <- p +
      facet_grid(paste(". ~", yVal, sep = "")) +
      theme(panel.spacing = unit(0.1, "lines")) +
      coord_flip()
  }
  p <- p + labs(x = xVal, y = yVal)

  p
}


#' Plots the density plots by faceting
#'
#' Make density plots by displaying subsets of the data in different panels.
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... other arguments being sent to stat_density
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords hplot
#' @export
#' @examples
#'  data(tips, package = "reshape")
#'  ggally_facetdensity(tips, mapping = ggplot2::aes(x = total_bill, y = sex))
#'  ggally_facetdensity(
#'    tips,
#'    mapping = ggplot2::aes_string(y = "total_bill", x = "sex", color = "sex")
#'  )
ggally_facetdensity <- function(data, mapping, ...){
  ggally_facetdensitystrip(data, mapping, ..., den_strip = FALSE)
}

#' Plots a tile plot with facets
#'
#' Make Tile Plot as densely as possible.
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... other arguments being sent to stat_bin
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords hplot
#' @export
#' @examples
#'  data(tips, package = "reshape")
#'  ggally_denstrip(tips, mapping = ggplot2::aes(x = total_bill, y = sex))
#'  ggally_denstrip(tips, mapping = ggplot2::aes_string(x = "total_bill", y = "sex"))
#'  ggally_denstrip(
#'    tips,
#'    mapping = ggplot2::aes_string(x = "sex", y = "tip", binwidth = "0.2")
#'  ) + ggplot2::scale_fill_gradient(low = "grey80", high = "black")
ggally_denstrip <- function(data, mapping, ...){
  mapping <- mapping_color_to_fill(mapping)

  ggally_facetdensitystrip(data, mapping, ..., den_strip = TRUE)
}

#' Plots a density plot with facets or a tile plot with facets
#'
#' Make Tile Plot as densely as possible.
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... other arguments being sent to either geom_histogram or stat_density
#' @param den_strip boolean to decide whether or not to plot a density strip(TRUE) or a facet density(FALSE) plot.
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords hplot
#' @export
#' @examples
#' example(ggally_facetdensity)
#' example(ggally_denstrip)
ggally_facetdensitystrip <- function(data, mapping, ..., den_strip = FALSE){
  horizontal <- is_horizontal(data, mapping)

  if (!horizontal) {
    mapping <- mapping_swap_x_y(mapping)
  }

  xVal <- deparse(mapping$x)
  yVal <- deparse(mapping$y)
  mapping$y <- NULL # will be faceted

  p <- ggplot(data = data, mapping) + labs(x = xVal, y = yVal)

  if (identical(den_strip, TRUE)) {

    p <- p +
      geom_histogram(
        mapping = aes(fill = ..density..), # nolint
        position = "fill",
        ...
      ) +
      scale_y_continuous(
        breaks = c(0.5),
        labels = "1"
      )

  } else {
    p <- p +
      stat_density(
      aes(
          y = ..scaled.. * diff(range(x, na.rm = TRUE)) + min(x, na.rm = TRUE) # nolint
        ),
        position = "identity",
        geom = "line",
        ...
      )
  }


  if (horizontal) {
    p <- p + facet_grid(paste(yVal, " ~ .", sep = ""))

    if (identical(den_strip, TRUE)) {
      p <- p + theme(axis.text.y = element_blank())
    }
  } else {
    p <- p + coord_flip()
    p <- p + facet_grid(paste(". ~ ", yVal, sep = ""))

    if (identical(den_strip, TRUE)) {
      p <- p + theme(axis.text.x = element_blank())
    }
  }

  p
}


#' Plots the Density Plots by Using Diagonal
#'
#' Plots the density plots by using Diagonal.
#'
#' @param data data set using
#' @param mapping aesthetics being used.
#' @param ... other arguments sent to stat_density
#' @param rescale boolean to decide whether or not to rescale the count output
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords hplot
#' @export
#' @examples
#'  data(tips, package = "reshape")
#'  ggally_densityDiag(tips, mapping = ggplot2::aes(x = total_bill))
#'  ggally_densityDiag(tips, mapping = ggplot2::aes(x = total_bill, color = day))
ggally_densityDiag <- function(data, mapping, ..., rescale = FALSE){

  mapping <- mapping_color_to_fill(mapping)

  p <- ggplot(data, mapping) +
    scale_y_continuous()

  if (identical(rescale, TRUE)) {
    p <- p +
      stat_density(
        aes(
          y = ..scaled.. * diff(range(x, na.rm = TRUE)) + min(x, na.rm = TRUE) # nolint
        ),
        position = "identity",
        geom = "line",
        ...
      )
  } else {
    p <- p + geom_density(...)
  }

  p
}

#' Plots the Bar Plots by Using Diagonal
#'
#' Plots the bar plots by using Diagonal.
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... other arguments are sent to geom_bar
#' @param rescale boolean to decide whether or not to rescale the count output. Only applies to numeric data
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords hplot
#' @export
#' @examples
#' data(tips, package = "reshape")
#' ggally_barDiag(tips, mapping = ggplot2::aes(x = day))
#' ggally_barDiag(tips, mapping = ggplot2::aes(x = tip), binwidth = 0.25)
ggally_barDiag <- function(data, mapping, ..., rescale = FALSE){

  mapping <- mapping_color_to_fill(mapping)

  mapping$y <- NULL
  x_data <- eval_data_col(data, mapping$x)
  numer <- ("continuous" == plotting_data_type(x_data))

  p <- ggplot(data = data, mapping)

  if (is_date(x_data)) {
    p <- p + geom_histogram(...)
    #TODO make y axis lines match date positions
    # buildInfo <- ggplot_build(p + geom_bar(...))
    # histBarPerc <- buildInfo$data[[1]]$ncount

  } else if (numer) {
    if (identical(rescale, TRUE)) {
      p <- p + geom_histogram(
        aes(
          y = ..density.. / max(..density..) * diff(range(x, na.rm = TRUE)) + min(x, na.rm = TRUE) # nolint
        ),
        ...
      ) + coord_cartesian(ylim = range(eval_data_col(data, mapping$x), na.rm = TRUE))

    } else {
      p <- p + geom_histogram(...)

    }

   } else {
    p <- p + geom_bar(...)

  }

  p
}


#' Text Plot
#'
#' Plot text for a plot.
#'
#' @param label text that you want to appear
#' @param mapping aesthetics that don't relate to position (such as color)
#' @param xP horizontal position percentage
#' @param yP vertical position percentage
#' @param xrange range of the data around it.  Only nice to have if plotting in a matrix
#' @param yrange range of the data around it.  Only nice to have if plotting in a matrix
#' @param ... other arguments for geom_text
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords hplot
#' @export
#' @examples
#' ggally_text("Example 1")
#' ggally_text("Example\nTwo", mapping = ggplot2::aes(size = 15), color = I("red"))
ggally_text <- function(
  label,
  mapping = ggplot2::aes(color = "black"),
  xP = 0.5,
  yP = 0.5,
  xrange = c(0, 1),
  yrange = c(0, 1),
  ...
){

  p <- ggplot() +
    xlim(xrange) +
    ylim(yrange) +
    theme(
      panel.background = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "grey85")
    ) +
    labs(x = NULL, y = NULL)

  new_mapping <- aes_string(
    x = xP * diff(xrange) + min(xrange, na.rm = TRUE),
    y = yP * diff(yrange) + min(yrange, na.rm = TRUE)
  )
  if (is.null(mapping)) {
    mapping <- new_mapping
  } else {
    mapping <- add_and_overwrite_aes(mapping, new_mapping)
  }

  # dont mess with color if it's already there
  if (!is.null(mapping$colour)) {
    colour <- mapping$colour
    # remove colour from the aesthetics, so legend isn't printed
    mapping$colour <- NULL
    p <- p +
       geom_text( label = label, mapping = mapping, colour = colour, ...)
  } else if ("colour" %in% names(aes(...))) {
    p <- p +
       geom_text( label = label, mapping = mapping, ...)
  } else {
    colour <- "grey50"
    p <- p +
       geom_text( label = label, mapping = mapping, colour = colour, ...)
  }

  p <- p + theme(legend.position = "none")

  p

}


#' Get x axis labels
#'
#' Retrieves x axis labels from the plot object directly.
#'
#' @importFrom gtable gtable_filter
#' @param p plot object
#' @param xRange range of x values
#' @keywords internal
get_x_axis_labels <- function(p, xRange) {
  pGrob <- ggplotGrob(p)

  axisTable <- gtable_filter(pGrob, "axis-b")$grobs[[1]]$children$axis

  # have to do a function as filter doesn't work
  get_raw_grob_by_name <- function(g, name) {
    for (item in g$grobs) {
      if (str_detect(item$name, name) ) {
        return(item$children[[1]])
      }
    }
    NULL
  }
  xAxisGrob <- get_raw_grob_by_name(axisTable, "axis.text.x")

  axisBreaks <- as.numeric(xAxisGrob$label)

  axisLabs <- rbind(
    expand.grid(xPos = axisBreaks[1], yPos = axisBreaks),
    expand.grid(xPos = axisBreaks,    yPos = axisBreaks[1])
  )[-1, ]

  axisLabs <- as.data.frame(axisLabs)
  axisLabs$lab <- as.character(apply(axisLabs, 1, max))
  axisLabs$hjust <- 0.5
  axisLabs$vjust <- 0.5

  minPos <- xRange[1]
  maxPos <- xRange[2]

  for (i in seq_len(nrow(axisLabs))) {
    xPos <- axisLabs[i, "xPos"]
    yPos <- axisLabs[i, "yPos"]

    if (yPos < minPos) {
      axisLabs[i, "yPos"] <- minPos
      axisLabs[i, "vjust"] <- 0
    } else if (yPos > maxPos) {
      axisLabs[i, "yPos"] <- maxPos
      axisLabs[i, "vjust"] <- 1
    }

    if (xPos < minPos) {
      axisLabs[i, "xPos"] <- minPos
      axisLabs[i, "hjust"] <- 0
    } else if (xPos > maxPos) {
      axisLabs[i, "xPos"] <- maxPos
      axisLabs[i, "hjust"] <- 1
    }
  }

  axisLabs
}

#' Internal Axis Labeling Plot for ggpairs
#'
#' This function is used when \code{axisLabels == "internal"}.
#'
#' @param data dataset being plotted
#' @param mapping aesthetics being used (x is the variable the plot will be made for)
#' @param label title to be displayed in the middle.  Defaults to \code{mapping$x}
#' @param labelSize size of variable label
#' @param labelXPercent percent of horizontal range
#' @param labelYPercent percent of vertical range
#' @param labelHJust hjust supplied to label
#' @param labelVJust vjust supplied to label
#' @param gridLabelSize size of grid labels
#' @param ... other arguments for geom_text
#' @author Jason Crowley \email{crowley.jason.s@@gmail.com} and Barret Schloerke
#' @export
#' @examples
#'  data(tips, package = "reshape")
#'  ggally_diagAxis(tips, ggplot2::aes(x=tip))
#'  ggally_diagAxis(tips, ggplot2::aes(x=sex))
ggally_diagAxis <- function(
  data,
  mapping,
  label = mapping$x,
  labelSize     = 5,
  labelXPercent = 0.5,
  labelYPercent = 0.55,
  labelHJust    = 0.5,
  labelVJust    = 0.5,
  gridLabelSize = 4,
  ...
) {
  if (is.null(mapping$x)) {
    stop("mapping$x is null.  There must be a column value in this location.")
  }
  mapping$y <- NULL
  numer <- ! is_horizontal(data, mapping, "x")

  if (! is.character(label)) {
    label <- deparse(mapping$x)
  }

  xData <- eval_data_col(data, mapping$x)

  if (numer) {
    xmin <- min(xData, na.rm = TRUE)
    xmax <- max(xData, na.rm = TRUE)

    # add a lil fluff... it looks better
    xrange <- c(xmin - .01 * (xmax - xmin), xmax + .01 * (xmax - xmin))
    # xrange <- c(xmin, xmax)

    p <- ggally_text(
      label   = label,
      mapping = aes(col = "grey50"),
      xrange  = xrange,
      yrange  = xrange,
      size    = labelSize,
      xP      = labelXPercent,
      yP      = labelYPercent,
      hjust   = labelHJust,
      vjust   = labelVJust
    )

    axisBreaks <- get_x_axis_labels(p, xrange)
    # print(axisBreaks)
    p <- p + geom_text(
      data    = axisBreaks,
      mapping = aes_string(
        x     = "xPos",
        y     = "yPos",
        label = "lab",
        hjust = "hjust",
        vjust = "vjust"
      ),
      col     = "grey50",
      size = gridLabelSize
    )

  } else {
    breakLabels <- levels(as.factor(xData))
    numLvls <- length(breakLabels)

    p <- ggally_text(
      label   = label,
      mapping = aes(col = "grey50"),
      xrange  = c(0, 1),
      yrange  = c(0, 1),
      size    = labelSize,
      yP      = labelYPercent,
      xP      = labelXPercent,
      hjust   = labelHJust,
      vjust   = labelVJust
    )
    #axisBreaks <- (1+2*0:(numLvls-1))/(2*numLvls)
    axisBreaks <- 0:(numLvls - 1) * (0.125 + (1 - 0.125 * (numLvls - 1)) / numLvls) +
      (1 - 0.125 * (numLvls - 1)) / (2 * numLvls)

    axisLabs <- data.frame(
      x   = axisBreaks[1:numLvls],
      y   = axisBreaks[numLvls:1],
      lab = breakLabels
    )

    p <- p + geom_text(
      data = axisLabs,
      mapping = aes(
        x     = x,
        y     = y,
        label = lab
      ),
      col = "grey50",
      size = gridLabelSize
    )

    # hack to remove warning message... cuz it doesn't listen to suppress messages
    p$scales$scales[[1]]$breaks <- axisBreaks
    p$scales$scales[[2]]$breaks <- axisBreaks
    # pLabs <- pLabs +
    #   scale_x_continuous(breaks=axisBreaks,limits=c(0,1)) +
    #   scale_y_continuous(breaks=axisBreaks,limits=c(0,1))
  }

  p

}

#' Plots the Bar Plots Faceted by Conditional Variable
#'
#' X variables are plotted using \code{geom_bar} and faceted by the Y variable.
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... other arguments are sent to geom_bar
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords hplot
#' @export
#' @examples
#'  data(tips, package = "reshape")
#'  ggally_facetbar(tips, ggplot2::aes(x = sex, y = smoker, fill = time))
#'  ggally_facetbar(tips, ggplot2::aes(x = smoker, y = sex, fill = time))
ggally_facetbar <- function(data, mapping, ...){

  mapping <- mapping_color_to_fill(mapping)

  # numer <- is.null(attributes(data[,as.character(mapping$x)])$class)
  # xVal <- mapping$x
  yVal <- deparse(mapping$y)
  mapping$y <- NULL
  p <- ggplot(data, mapping) +
    geom_bar(...) +
    facet_grid(paste(yVal, " ~ .", sep = ""))

  p
}


#' Plots a mosaic plot
#'
#' Plots the mosaic plot by using fluctuation.
#'
#' @param data data set using
#' @param mapping aesthetics being used. Only x and y will used and both are required
#' @param ... passed to \code{\link[ggplot2]{geom_tile}(...)}
#' @param floor don't display cells smaller than this value
#' @param ceiling max value to scale frequencies.  If any frequency is larger than the ceiling, the fill color is displayed darker than other rectangles
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords hplot
#' @export
#' @examples
#' data(tips, package = "reshape")
#' ggally_ratio(tips, ggplot2::aes(sex, day))
#' ggally_ratio(tips, ggplot2::aes(sex, day)) + ggplot2::coord_equal()
#' # only plot tiles greater or equal to 20 and scale to a max of 50
#' ggally_ratio(
#'   tips, ggplot2::aes(sex, day),
#'   floor = 20, ceiling = 50
#' ) + ggplot2::theme(aspect.ratio = 4/2)
ggally_ratio <- function(
  data,
  mapping = do.call(ggplot2::aes_string, as.list(colnames(data)[1:2])),
  ...,
  floor = 0,
  ceiling = NULL
) {

  # capture the original names
  xName <- deparse(mapping$x)
  yName <- deparse(mapping$y)

  countData <- plyr::count(data, vars = c(xName, yName))

  # overwrite names so name clashes don't happen
  colnames(countData)[1:2] <- c("x", "y")

  xNames <- levels(countData[["x"]])
  yNames <- levels(countData[["y"]])

  countData <- subset(countData, freq >= floor)

  if (is.null(ceiling)) {
    ceiling <- max(countData$freq)
  }

  countData[["freqSize"]] <- sqrt(pmin(countData[["freq"]], ceiling) / ceiling)
  countData[["col"]] <- ifelse(countData[["freq"]] > ceiling, "grey30", "grey50")

  countData[["xPos"]] <- as.numeric(countData[["x"]]) + (1 / 2) * countData[["freqSize"]]
  countData[["yPos"]] <- as.numeric(countData[["y"]]) + (1 / 2) * countData[["freqSize"]]

  p <- ggplot(
      data = countData,
      mapping = aes_string(
        x = "xPos",
        y = "yPos",
        height = "freqSize",
        width = "freqSize",
        fill = "col"
      )
    ) +
    geom_tile(...) +
    scale_fill_identity() +
    scale_x_continuous(
      name = xName,
      limits = c(0.9999, length(xNames) + 1),
      breaks = 1:(length(xNames) + 1),
      labels = c(xNames, ""),
      minor_breaks = FALSE
    ) +
    scale_y_continuous(
      name = yName,
      limits = c(0.9999, length(yNames) + 1),
      breaks = 1:(length(yNames) + 1),
      labels = c(yNames, ""),
      minor_breaks = FALSE
    ) +
    theme(
      axis.text.x = element_text(
        hjust = 0,
        vjust = 1,
        colour = "grey50"
      ),
      axis.text.y = element_text(
        hjust = 0,
        vjust = 0,
        angle = 90,
        colour = "grey50"
      )
    )

  p
}



#' Blank
#'
#' Draws nothing.
#'
#' Makes a "blank" ggplot object that will only draw white space
#'
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @param ... other arguments ignored
#' @export
#' @keywords hplot
ggally_blank <- function(...){
  aes(...) # ignored
  a <- data.frame(X = 1:2, Y = 1:2)

  p <- ggplot(data = a, aes_string(x = "X", y = "Y")) +
    geom_point( colour = "transparent") +
    theme(
      axis.line         = element_blank(),
      axis.text.x       = element_blank(),
      axis.text.y       = element_blank(),
      axis.ticks        = element_blank(),
      axis.title.x      = element_blank(),
      axis.title.y      = element_blank(),
      legend.background = element_blank(),
      legend.key        = element_blank(),
      legend.text       = element_blank(),
      legend.title      = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_blank(),
      plot.title        = element_blank(),
      strip.background  = element_blank(),
      strip.text.x      = element_blank(),
      strip.text.y      = element_blank()
    )

  class(p) <- c(class(p), "ggmatrix_blank")
  p
}

#' @rdname ggally_blank
#' @export
ggally_blankDiag <- function(...) {
  ggally_blank(...)
}



#' NA plot
#'
#' Draws a large \code{NA} in the middle of the plotting area.  This plot is useful when all X or Y data is \code{NA}
#'
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @param data ignored
#' @param mapping ignored
#' @param size size of the geom_text 'NA'
#' @param color color of the geom_text 'NA'
#' @param ... other arguments sent to geom_text
#' @export
#' @keywords hplot
ggally_na <- function(data = NULL, mapping = NULL, size = 10, color = "grey20", ...) {
  a <- data.frame(x = 1, y = 1, label = "NA")

  p <- ggplot(data = a, aes_string(x = "x", y = "y", label = "label")) +
    geom_text(color = color, size = size, ...) +
    theme(
      axis.line         = element_blank(),
      axis.text.x       = element_blank(),
      axis.text.y       = element_blank(),
      axis.ticks        = element_blank(),
      axis.title.x      = element_blank(),
      axis.title.y      = element_blank(),
      legend.background = element_blank(),
      legend.key        = element_blank(),
      legend.text       = element_blank(),
      legend.title      = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_blank(),
      plot.title        = element_blank(),
      strip.background  = element_blank(),
      strip.text.x      = element_blank(),
      strip.text.y      = element_blank()
    )

  p
}

#' @rdname ggally_na
#' @export
ggally_naDiag <- function(...) {
  ggally_na(...)
}
