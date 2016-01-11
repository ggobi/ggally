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
  p$type <- "continuous"
  p$subType <- "points"
  p
}

#' Plots the Scatter Plot with Smoothing
#'
#' Add a smoothed condition mean with a given scatter plot.
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... other arguments to add to geom_point
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @export
#' @keywords hplot
#' @examples
#'  data(tips, package = "reshape")
#'  ggally_smooth(tips, mapping = ggplot2::aes(x = total_bill, y = tip))
#'  ggally_smooth(tips, mapping = ggplot2::aes_string(x = "total_bill", y = "tip"))
#'  ggally_smooth(tips, mapping = ggplot2::aes_string(x = "total_bill", y = "tip", color = "sex"))
ggally_smooth <- function(data, mapping, ...){

  p <- ggplot(data = data, mapping)

  p <- p + geom_point(...)

  if (! is.null(mapping$color) || ! is.null(mapping$colour)) {
    p <- p + geom_smooth(method = "lm")
  } else {
    p <- p + geom_smooth(method = "lm", colour = I("black"))
  }

  p$type <- "continuous"
  p$subType <- "smooth"
  p
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
  rangeX <- range(eval(mapping$x, data), na.rm = TRUE)
  rangeY <- range(eval(mapping$y, data), na.rm = TRUE)

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

  p$type <- "continuous"
  p$subType <- "density"

  p
}

#' Correlation from the Scatter Plot
#'
#' Estimate correlation from the given data.
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param alignPercent right align position of numbers. Default is 60 percent across the horizontal
#' @param method \code{method} suppied to cor function
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

  # xVar <- data[,as.character(mapping$x)]
  # yVar <- data[,as.character(mapping$y)]
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

  xCol <- as.character(mapping$x)
  yCol <- as.character(mapping$y)

  if (is_date(data[, xCol]) || is_date(data[, yCol])) {

    # make sure it's a data.frame, as data.tables don't work well
    if (! identical(class(data), "data.frame")) {
      data <- as.data.frame(data)
    }

    for (col in c(xCol, yCol)) {
      if (is_date(data[, col])) {
        data[, col] <- as.numeric(data[, col])
      }
    }
  }

  colorCol <- deparse(mapping$colour)
  singleColorCol <- paste(colorCol, collapse = "")

  if (use %in% c("complete.obs", "pairwise.complete.obs", "na.or.complete")) {
    if (length(colorCol) > 0) {
      if (singleColorCol %in% colnames(data)) {
        rows <- complete.cases(data[, c(xCol, yCol, colorCol)])
      } else {
        rows <- complete.cases(data[, c(xCol, yCol)])
      }
    } else {
      rows <- complete.cases(data[, c(xCol, yCol)])
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

  xVal <- data[, xCol]
  yVal <- data[, yCol]

  if (length(names(mapping)) > 0){
    for (i in length(names(mapping)):1){
      # find the last value of the aes, such as cyl of as.factor(cyl)
      tmp_map_val <- as.character(mapping[names(mapping)[i]][[1]])
      if (tmp_map_val[length(tmp_map_val)] %in% colnames(data))
        mapping[names(mapping)[i]] <- NULL

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
      cor_fn(x[, xCol], x[, yCol])
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

    p$type <- "continuous"
    p$subType <- "cor"
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

    p$type <- "continuous"
    p$subType <- "cor"
    p
  }
}


#' Plots the Box Plot
#'
#' Make a box plot with a given data set
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
  ggally_dotAndBox(data, mapping, ..., boxPlot = TRUE)
}


#' Plots the Box Plot with Dot
#'
#' Add jittering with the box plot
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
  ggally_dotAndBox(data, mapping, ..., boxPlot = FALSE)
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
#'  ggally_dotAndBox(
#'    tips,
#'    mapping = ggplot2::aes(x = total_bill, y = sex, color = sex),
#'    boxPlot = TRUE
#'  )
#'  ggally_dotAndBox(tips, mapping = ggplot2::aes(x = total_bill, y = sex, color = sex), boxPlot=FALSE)
ggally_dotAndBox <- function(data, mapping, ..., boxPlot = TRUE){
  horizontal <-
    (is.factor(data[, as.character(mapping$y)])) ||
    (is.character(data[, as.character(mapping$y)]))
#  print(horizontal)

  #print(mapping$x[1])
  if (horizontal) {
    mapping$tmp <- mapping$x
    mapping$x <- mapping$y
    mapping$y <- mapping$tmp
    mapping$tmp <- NULL
#    levels(data[,as.character(mapping$x)]) <- rev(levels(data[,as.character(mapping$x)]))
  }

#  print(as.character(mapping$x))
#  print(levels(data[,as.character(mapping$x)]))
#  print(is.factor(data[,as.character(mapping$x)]))
  xVal <- as.character(mapping$x)
  yVal <- as.character(mapping$x)
  mapping$x <- 1

  p <- ggplot(data = data)

  if (boxPlot) {
    p <- p + geom_boxplot(mapping, ...)
    p$subType <- "box"
  } else {
    p <- p + geom_jitter(mapping, ...)
    p$subType <- "dot"
  }

  if (!horizontal) {
    p <- p + facet_grid(paste(". ~ ", yVal, sep = "")) + theme(panel.margin = unit(0.1, "lines"))
#    p$facet$facets <- paste(". ~ ", yVal, sep = "")
  } else {
#    print(xVal)
#    print(yVal)
    p <- p + coord_flip() + theme(
        axis.text.y = element_text(
          angle = 90,
          vjust = 0,
          colour = "grey50"
        )
      )
    p <- p + facet_grid(paste(yVal, " ~ .", sep = "")) + theme(panel.margin = unit(0.1, "lines"))
#    p$facet$facets <- paste(yVal, " ~ .", sep = "")
#    print(p$facet$facets)
  }

  p <- p + scale_x_continuous(xVal, labels = "", breaks = 1)

  p$type <- "combo"
  p$horizontal <- horizontal
  p
}
# This is done with side by side and not facets
#ggally_dotAndBox <- function(data, mapping, ..., boxPlot = TRUE)
#{
#  horizontal <-  is.factor(data[,as.character(mapping$y)]) || is.character(data[,as.character(mapping$y)])
#
#  if(horizontal) {
#    cat("horizontal dot-box\n")
#    mapping$tmp <- mapping$x
#    mapping$x <- mapping$y
#    mapping$y <- mapping$tmp
#    mapping$tmp <- NULL
#    levels(data[,as.character(mapping$x)]) <- rev(levels(data[,as.character(mapping$x)]))
#  }
#print(str(mapping))
#
#  p <- ggplot(data = data, mapping)
#
#
#  if(boxPlot)
#    p <- p + geom_boxplot(...)
#  else
#    p <- p + geom_jitter(...)
#
#  if(horizontal){
#    p <- p + coord_flip() + theme(
#        axis.text.y = element_text(
#          angle = 90,
#          vjust = 0,
#          colour = "grey50"
#        )
#      )
#  }
#
#  p
#}


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
#  str(mapping)
  #aesString <- aes_string(mapping)
  #cat("\naesString\n");print(str(aesString))

  horizontal <-
    (is.factor(data[, as.character(mapping$y)])) ||
    (is.character(data[, as.character(mapping$y)]))

  if (!horizontal) {
    mapping$tmp <- mapping$x
    mapping$x <- mapping$y
    mapping$y <- mapping$tmp
    mapping$tmp <- NULL
  } #else {
     # horizontal
     # re-order levels to match all other plots
#     levels(data[,as.character(mapping$y)]) <- rev(levels(data[,as.character(mapping$y)]))
  #}

#cat("Horizontal: ", horizontal, "\n")
#cat("\nmapping\n");print(str(mapping))
#cat("\ndata\n");print(head(data))

  xVal <- as.character(mapping$x)
  yVal <- as.character(mapping$y)
  mapping$y <- NULL
#  yVal <- as.character(mapping$x)
#  mapping$x <- 1
#str(mapping)
#str(xVal)
#str(yVal)

  p <- ggplot(data = data, mapping)
#  mapping$x <- NULL
  p <- p + stat_bin(...)

  if (horizontal) {
    # facet_grid(list(".", yVal))
    p <- p +
      facet_grid(paste(as.character(yVal), " ~ .", sep = "")) +
      theme(panel.margin = unit(0.1, "lines"))
#    p$facet$facets <- paste(as.character(yVal), " ~ .", sep = "")
  } else {
    p <- p +
      facet_grid(paste(". ~", as.character(yVal), sep = "")) +
      theme(panel.margin = unit(0.1, "lines")) +
      coord_flip()
#    p$facet$facets <- paste(". ~ ", as.character(yVal), sep = "")
  }
  p <- p + ylab(as.character(yVal)) + xlab(as.character(xVal))

  p$type <- "combo"
  p$subType <- "facethist"
  p$horizontal <- horizontal

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
  horizontal <-
    (is.factor(data[, as.character(mapping$y)])) ||
    (is.character(data[, as.character(mapping$y)]))

  if (!horizontal) {
    mapping$tmp <- mapping$x
    mapping$x <- mapping$y
    mapping$y <- mapping$tmp
    mapping$tmp <- NULL

  }

  xVal <- mapping$x
  yVal <- mapping$y
  mapping$y <- NULL

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
    p$subType <- "denstrip"

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
    p$subType <- "facetdensity"
  }


  if (horizontal) {
    p <- p + facet_grid(paste(as.character(yVal), " ~ .", sep = ""))

    if (identical(den_strip, TRUE))
      p <- p + theme(axis.text.y = element_blank())
  } else {
    p <- p + coord_flip()
    p <- p + facet_grid(paste(". ~ ", as.character(yVal), sep = ""))

    if (identical(den_strip, TRUE))
      p <- p + theme(axis.text.x = element_blank())
  }
  p$type <- "combo"
  p$horizontal <- horizontal

  p
}

#' Plots a mosaic plots
#'
#' Plots the mosaic plot by using fluctuation.
#'
#' Must send only two discrete columns in the data set.
#'
#' @param data data set using
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords hplot
#' @export
#' @examples
#' data(tips, package = "reshape")
#' ggally_ratio(tips[, c("sex", "smoker")])
#' ggally_ratio(tips[, c("sex", "smoker")]) + ggplot2::coord_equal()
#' ggally_ratio(
#'   tips[, c("sex", "day")]
#' ) + ggplot2::theme( aspect.ratio = 4/2)
ggally_ratio <- function(data){
  # dataNames <- colnames(data)
  data <- data[, 2:1]
  tmpData <- table(data)
  tmpData <- tmpData[rev(seq_len(nrow(tmpData))), ]
  tmpData <- as.table(tmpData)
  p <- ggfluctuation2(tmpData)# + labs(x = dataNames[1], y = dataNames[2])
  p$type <- "discrete"
  p$subType <- "ratio"
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

  p <- ggplot(data, mapping) +
    # scale_x_continuous() +
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
    p <- p + geom_density()
  }

  p$type <- "diag"
  p$subType <- "density"
  p

}

#' Plots the Bar Plots by Using Diagonal
#'
#' Plots the bar plots by using Diagonal.
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... other arguements are sent to geom_bar
#' @param rescale boolean to decide whether or not to rescale the count output
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords hplot
#' @export
#' @examples
#' data(tips, package = "reshape")
#' ggally_barDiag(tips, mapping = ggplot2::aes(x = day))
#' ggally_barDiag(tips, mapping = ggplot2::aes(x = tip), binwidth = 0.25)
ggally_barDiag <- function(data, mapping, ..., rescale = FALSE){
  mapping$y <- NULL
  numer <- ("continuous" == plotting_data_type(data[, as.character(mapping$x)]))

  p <- ggplot(data = data, mapping)

  if (is_date(data[, as.character(mapping$x)])) {
    p <- p + geom_histogram(...)
    #TODO make y axis lines match date positions
    # buildInfo <- ggplot_build(p + geom_bar(...))
    # histBarPerc <- buildInfo$data[[1]]$ncount

    p$subType <- "bar_num"

  } else if (numer) {
    # message("is numeric")
    if (identical(rescale, TRUE)) {
      p <- p + geom_histogram(
        aes(
          y = ..density.. / max(..density..) * diff(range(x, na.rm = TRUE)) + min(x, na.rm = TRUE) # nolint
        ),
        ...
      ) + coord_cartesian(ylim = range(data[, as.character(mapping$x)], na.rm = TRUE))
    } else {
      p <- p + geom_histogram(...)

    }

    p$subType <- "bar_num"
   } else {
    # message("is categorical")
    # xVal <- mapping$x
    # mapping <- add_and_overwrite_aes(mapping, aes(x = 1L))
    # # p <- ggplot(m, mapping) + geom_bar(aes(weight = Freq), binwidth = 1, ...)
    # p <- ggplot(data, mapping) + geom_bar(...)
    # # p <- p + scale_x_continuous(NULL, labels ="",breaks = 1)

    # xVal <- mapping$x
    # mapping <- add_and_overwrite_aes(mapping, aes(x = 1L))
    # mapping <- add_and_overwrite_aes(mapping, aes_string(weight = xVal))
    # mapping <- add_and_overwrite_aes(mapping, aes_string(weight = xVal))
    # p <- ggplot(data = data, mapping) + geom_bar(...)
    # p$facet$facets <- paste(". ~ ", as.character(xVal), sep = "")
    p <- p + geom_bar(...)

    p$subType <- "bar_cat"
  }
  p$type <- "diag"
  p
}


#' GGplot Text
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

  #rectData <- data.frame(
  #  x1 = xrange[1],
  #  x2 = xrange[2],
  #  y1 = yrange[1],
  #  y2 = yrange[2]
  #)
  # print(rectData)

  p <- ggplot() + xlim(xrange) + ylim(yrange) +
      theme(panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey85")) +
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
  numer <- !(
    (is.factor(data[, as.character(mapping$x)])) ||
    (is.character(data[, as.character(mapping$x)]))
  )

  if (! is.character(label)) {
    label <- as.character(mapping$x)
  }

  if (numer) {
    xmin <- min(data[, as.character(mapping$x)], na.rm = TRUE)
    xmax <- max(data[, as.character(mapping$x)], na.rm = TRUE)

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
    pLabs <- p + geom_text(
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
    breakLabels <- levels(as.factor(data[, as.character(mapping$x)]))
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

    pLabs <- p + geom_text(
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
    pLabs$scales$scales[[1]]$breaks <- axisBreaks
    pLabs$scales$scales[[2]]$breaks <- axisBreaks
    # pLabs <- pLabs +
    #   scale_x_continuous(breaks=axisBreaks,limits=c(0,1)) +
    #   scale_y_continuous(breaks=axisBreaks,limits=c(0,1))
  }

  pLabs$subType <- "internal"
  pLabs$type <- "label"
  pLabs

}

#' Plots the Bar Plots Faceted by Conditional Variable
#'
#' X variables are plotted using \code{geom_bar} and faceted by the Y variable.
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... other arguements are sent to geom_bar
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords hplot
#' @export
#' @examples
#'  data(tips, package = "reshape")
#'  ggally_facetbar(tips, ggplot2::aes(x = sex, y = smoker, fill = time))
#'  ggally_facetbar(tips, ggplot2::aes(x = smoker, y = sex, fill = time))
ggally_facetbar <- function(data, mapping, ...){

  # numer <- is.null(attributes(data[,as.character(mapping$x)])$class)
  # xVal <- mapping$x
  yVal <- mapping$y
  mapping$y <- NULL
  p <- ggplot(data, mapping) + geom_bar(...)
  p <- p + facet_grid(paste(as.character(yVal), " ~ .", sep = ""))
  #p$facet$facets <- paste(as.character(yVal), " ~ .", sep = "")
  p$subType <- "facetbar"
  p$type <- "diag"

  p
}



#' Fluctuation plot
#'
#' Create a fluctuation plot.
#'
#' A fluctutation diagram is a graphical representation of a contingency table. This fuction currently only supports 2D contingency tables.
#' The function was adopted from experiemntal functions within GGplot2 developed by Hadley Wickham.
#'
#' @param table_data a table of values, or a data frame with three columns, the last column being frequency
#' @param floor don't display cells smaller than this value
#' @param ceiling max value to compare to
#' @author Hadley Wickham \email{h.wickham@@gmail.com}, Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords hplot
#' @importFrom reshape add.all.combinations
#' @export
#' @examples
#' data(tips, package = "reshape")
#' ggfluctuation2(table(tips$sex, tips$day))
#' ggfluctuation2(table(tips[, c("sex", "day")]))
ggfluctuation2 <- function (table_data, floor = 0, ceiling = max(table_data$freq, na.rm = TRUE)) {

  yNames <- rownames(table_data)
  xNames <- colnames(table_data)
  oldnames <- rev(names(dimnames(table_data)))

  if (is.table(table_data)) {
    table_data <- as.data.frame(t(table_data))
  }

  if (all(oldnames == "")) {
    oldnames <- c("X", "Y")
  }

  names(table_data) <- c("x", "y", "result")
  table_data <- add.all.combinations(table_data, list("x", "y")) # nolint
  table_data <- transform(table_data, x = as.factor(x), y = as.factor(y),
    freq = result)

  table_data <- transform(table_data, freq = sqrt(pmin(freq * .95, ceiling) / ceiling),
    border = ifelse(is.na(freq), "grey90", ifelse(freq >
      ceiling, "grey30", "grey50")))
  table_data[is.na(table_data$freq), "freq"] <- 1
  table_data <- subset(table_data, freq * ceiling >= floor)

  xNew <- as.numeric(table_data$x) + (1 / 2) * table_data$freq
  yNew <- as.numeric(table_data$y) + (1 / 2) * table_data$freq

  # maxLen <- max(
  #   diff(range(as.numeric(table_data$x), na.rm = TRUE)),
  #   diff(range(as.numeric(table_data$y), na.rm = TRUE))
  # )


  table_data <- cbind(table_data, xNew, yNew)
  # print(table_data)
  # print(xNames)
  # print(yNames)
  #
  # cat("\nmaxLen");print(maxLen)


  p <- ggplot(
      table_data,
      aes_string(
        x = "xNew",
        y = "yNew",
        height = "freq",
        width = "freq",
        fill = "border"
      )
    ) +
    geom_tile(colour = "white") +
    scale_fill_identity() +
    scale_x_continuous(
      name = oldnames[1],
#      limits=c(1,maxLen + 2),
#      breaks=1:(maxLen + 2),
#      labels=c(xNames,rep("",maxLen - length(xNames) + 2)),
      limits = c(0.9999, length(xNames) + 1),
      breaks = 1:(length(xNames) + 1),
      labels = c(xNames, ""),
      minor_breaks = FALSE
    ) +
    scale_y_continuous(
      name = oldnames[2],
#      limits=c(1,maxLen + 2),
#      breaks=1:(maxLen + 2),
#      labels=c(yNames,rep("",maxLen - length(yNames) + 2)),
      limits = c(0.9999, length(yNames) + 1),
      breaks = 1:(length(yNames) + 1),
      labels = c(yNames, ""),
      minor_breaks = FALSE
    ) +
#    coord_equal() +
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
  p$x_names <- xNames
  p$y_names <- yNames

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
  p$subType <- p$type <- "blank"
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
  p$subType <- p$type <- "na"
  p
}

#' @rdname ggally_na
#' @export
ggally_naDiag <- function(...) {
  ggally_na(...)
}
