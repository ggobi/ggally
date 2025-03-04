# add global variable
if (getRversion() >= "2.15.1") {
  utils::globalVariables(unique(c(
    "labelp", # cor plot
    c("x"), # facetdensitystrip plot
    c("x"), # density diagonal plot
    c("x", "y", "lab"), # internal axis plot
    c("x", "y", "result", "freq"), # fluctuation plot
    c("weight") # ggally_summarise_by
  )))
}




# retrieve the evaulated data column given the aes (which could possibly do operations)
#' Evaluate data column
#' @param data data set to evaluate the data with
#' @param aes_col Single value from an \code{ggplot2::\link[ggplot2]{aes}(...)} object
#' @return Aes mapping with the x and y values switched
#' @export
#' @examples
#' mapping <- ggplot2::aes(Petal.Length)
#' eval_data_col(iris, mapping$x)
eval_data_col <- function(data, aes_col) {
  rlang::eval_tidy(aes_col, data)
}
#' Aes name
#' @param aes_col Single value from \code{ggplot2::\link[ggplot2]{aes}(...)}
#' @return character string
#' @export
#' @examples
#' mapping <- ggplot2::aes(Petal.Length)
#' mapping_string(mapping$x)
mapping_string <- function(aes_col) {
  gsub("^~", "", deparse(aes_col, 500L))
}

# is categories on the left?
#' Check if plot is horizontal
#'
#' @param data data used in ggplot2 plot
#' @param mapping ggplot2 \code{aes()} mapping
#' @param val key to retrieve from \code{mapping}
#' @return Boolean determining if the data is a character-like data
#' @export
#' @rdname is_horizontal
#' @examples
#' is_horizontal(iris, ggplot2::aes(Sepal.Length, Species)) # TRUE
#' is_horizontal(iris, ggplot2::aes(Sepal.Length, Species), "x") # FALSE
#' is_horizontal(iris, ggplot2::aes(Sepal.Length, Sepal.Width)) # FALSE
is_horizontal <- function(data, mapping, val = "y") {
  yData <- eval_data_col(data, mapping[[val]])

  is.factor(yData) || is.character(yData) || is.logical(yData)
}
#' @export
#' @rdname is_horizontal
is_character_column <- is_horizontal

#' Swap x and y mapping
#' @param mapping output of \code{ggplot2::\link[ggplot2]{aes}(...)}
#' @return Aes mapping with the x and y values switched
#' @export
#' @examples
#' mapping <- ggplot2::aes(Petal.Length, Sepal.Width)
#' mapping
#' mapping_swap_x_y(mapping)
mapping_swap_x_y <- function(mapping) {
  tmp <- mapping$x
  mapping$x <- mapping$y
  mapping$y <- tmp
  mapping
}


#' Remove colour mapping unless found in select mapping keys
#' @param mapping output of \code{ggplot2::\link[ggplot2]{aes}(...)}
#' @param to set of mapping keys to check
#' @return Aes mapping with colour mapping kept only if found in selected mapping keys.
#' @export
#' @examples
#' mapping <- aes(x = sex, y = age, colour = sex)
# remove_color_unless_equal(mapping, to = c("x", "y"))
# remove_color_unless_equal(mapping, to = c("y"))
#'
#' mapping <- aes(x = sex, y = age, colour = region)
#' remove_color_unless_equal(mapping)
remove_color_unless_equal <- function(mapping, to = c("x", "y")) {
  if (!is.null(mapping$colour)) {
    color_str <- mapping_string(mapping$colour)
    for (to_val in to) {
      to_str <- mapping_string(mapping[[to_val]])
      if (color_str == to_str) {
        # found! return
        return(mapping)
      }
    }

    # not found. Remove color value
    mapping <- mapping[names(mapping) != "colour"]
  }

  mapping
}


#' Scatter plot
#'
#' Make a scatter plot with a given data set.
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... other arguments are sent to geom_point
#' @author Barret Schloerke
#' @export
#' @keywords hplot
#' @examples
#' # Small function to display plots only if it's interactive
#' p_ <- GGally::print_if_interactive
#'
#' data(mtcars)
#' p_(ggally_points(mtcars, mapping = ggplot2::aes(disp, hp)))
#' p_(ggally_points(mtcars, mapping = ggplot2::aes(disp, hp)))
#' p_(ggally_points(
#'   mtcars,
#'   mapping = ggplot2::aes(
#'     x     = disp,
#'     y     = hp,
#'     color = as.factor(cyl),
#'     size  = gear
#'   )
#' ))
ggally_points <- function(data, mapping, ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(...)

  p
}

#' Scatter plot with a smoothed line
#'
#' Add a smoothed condition mean with a given scatter plot.
#'
#' Y limits are reduced to match original Y range with the goal of keeping the Y axis the same across plots.
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param formula,... other arguments to add to geom_smooth
#' @param method,se parameters supplied to \code{\link[ggplot2]{geom_smooth}}
#' @param shrink boolean to determine if y range is reduced to range of points or points and error ribbon
#' @author Barret Schloerke
#' @export
#' @keywords hplot
#' @rdname ggally_smooth
#' @examples
#' # Small function to display plots only if it's interactive
#' p_ <- GGally::print_if_interactive
#'
#' data(tips)
#' p_(ggally_smooth(tips, mapping = ggplot2::aes(x = total_bill, y = tip)))
#' p_(ggally_smooth(tips, mapping = ggplot2::aes(total_bill, tip, color = sex)))
ggally_smooth <- function(data, mapping, ..., method = "lm", formula = y ~ x, se = TRUE, shrink = TRUE) {
  p <- ggplot(data = data, mapping)

  p <- p + geom_point(...)

  if (!is.null(mapping$color) || !is.null(mapping$colour)) {
    p <- p + geom_smooth(method = method, se = se, formula = formula)
  } else {
    p <- p + geom_smooth(method = method, se = se, formula = formula, colour = I("black"))
  }

  if (isTRUE(shrink)) {
    p <- p +
      coord_cartesian(
        ylim = range(eval_data_col(data, mapping$y), na.rm = TRUE)
      )
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

#' Bivariate density plot
#'
#' Make a 2D density plot from a given data.
#'
#' The aesthetic "fill" determines whether or not \code{stat_density2d} (filled) or \code{geom_density2d} (lines) is used.
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... parameters sent to either stat_density2d or geom_density2d
#' @author Barret Schloerke
#' @export
#' @keywords hplot
#' @examples
#' # Small function to display plots only if it's interactive
#' p_ <- GGally::print_if_interactive
#'
#' data(tips)
#' p_(ggally_density(tips, mapping = ggplot2::aes(x = total_bill, y = tip)))
#' p_(ggally_density(
#'   tips,
#'   mapping = ggplot2::aes(total_bill, tip, fill = after_stat(level))
#' ))
#' p_(ggally_density(
#'   tips,
#'   mapping = ggplot2::aes(total_bill, tip, fill = after_stat(level))
#' ) + ggplot2::scale_fill_gradient(breaks = c(0.05, 0.1, 0.15, 0.2)))
ggally_density <- function(data, mapping, ...) {
  rangeX <- range(eval_data_col(data, mapping$x), na.rm = TRUE)
  rangeY <- range(eval_data_col(data, mapping$y), na.rm = TRUE)

  p <- ggplot(data = data) +
    geom_point(
      data = data.frame(rangeX = rangeX, rangeY = rangeY),
      mapping = aes(x = !!as.name("rangeX"), y = !!as.name("rangeY")),
      alpha = 0
    )

  if (!is.null(mapping$fill)) {
    p <- p + stat_density2d(mapping = mapping, geom = "polygon", ...)
  } else {
    p <- p + geom_density2d(mapping = mapping, ...)
  }

  p
}


#' Correlation value plot
#'
#' Estimate correlation from the given data. If a color variable is supplied, the correlation will also be calculated per group.
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... other arguments being supplied to \code{\link[ggplot2]{geom_text}()} for the title and groups
#' @param stars logical value which determines if the significance stars should be displayed.  Given the \code{\link[stats]{cor.test}} p-values, display \describe{
#'   \item{\code{"***"}}{if the p-value is \verb{< 0.001}}
#'   \item{\code{"**"}}{if the p-value is \verb{< 0.01}}
#'   \item{\code{"*"}}{if the p-value is \verb{< 0.05}}
#'   \item{\code{"."}}{if the p-value is \verb{< 0.10}}
#'   \item{\code{""}}{otherwise}
#' }
#' @param method \code{method} supplied to cor function
#' @param use \code{use} supplied to \code{\link[stats]{cor}} function
#' @param display_grid if \code{TRUE}, display aligned panel grid lines. If \code{FALSE} (default), display a thin panel border.
#' @param digits number of digits to be displayed after the decimal point. See \code{\link[base]{formatC}} for how numbers are calculated.
#' @param title_args arguments being supplied to the title's \code{\link[ggplot2]{geom_text}()}
#' @param group_args arguments being supplied to the split-by-color group's \code{\link[ggplot2]{geom_text}()}
#' @param justify_labels \code{justify} argument supplied when \code{\link[base]{format}}ting the labels
#' @param align_percent relative align position of the text. When \code{justify_labels = 0.5}, this should not be needed to be set.
#' @param alignPercent,displayGrid deprecated. Please use their snake-case counterparts.
#' @param title title text to be displayed
#' @author Barret Schloerke
#' @importFrom stats complete.cases cor
#' @seealso \code{\link{ggally_statistic}}, \code{\link{ggally_cor_v1_5}}
#' @export
#' @keywords hplot
#' @examples
#' # Small function to display plots only if it's interactive
#' p_ <- GGally::print_if_interactive
#'
#' data(tips)
#' p_(ggally_cor(tips, mapping = ggplot2::aes(total_bill, tip)))
#' # display with grid
#' p_(ggally_cor(
#'   tips,
#'   mapping = ggplot2::aes(total_bill, tip),
#'   display_grid = TRUE
#' ))
#' # change text attributes
#' p_(ggally_cor(
#'   tips,
#'   mapping = ggplot2::aes(x = total_bill, y = tip),
#'   size = 15,
#'   colour = I("red"),
#'   title = "Correlation"
#' ))
#' # split by a variable
#' p_(ggally_cor(
#'   tips,
#'   mapping = ggplot2::aes(total_bill, tip, color = sex),
#'   size = 5
#' ))
ggally_cor <- function(
    data,
    mapping,
    ...,
    stars = TRUE,
    method = "pearson",
    use = "complete.obs",
    display_grid = FALSE,
    digits = 3,
    title_args = list(...),
    group_args = list(...),
    justify_labels = "right",
    align_percent = 0.5,
    title = "Corr",
    alignPercent = warning("deprecated. Use `align_percent`"),
    displayGrid = warning("deprecated. Use `display_grid`")) {
  if (!missing(alignPercent)) {
    warning("`alignPercent` is deprecated. Please use `align_percent` if alignment still needs to be adjusted")
    align_percent <- alignPercent
  }
  if (!missing(displayGrid)) {
    warning("`displayGrid` is deprecated. Please use `display_grid`")
    display_grid <- displayGrid
  }

  na.rm <-
    if (missing(use)) {
      # display warnings
      NA
    } else {
      (use %in% c("complete.obs", "pairwise.complete.obs", "na.or.complete"))
    }

  ggally_statistic(
    data = data,
    mapping = mapping,
    na.rm = na.rm,
    align_percent = align_percent,
    display_grid = display_grid,
    title_args = title_args,
    group_args = group_args,
    justify_labels = justify_labels,
    justify_text = "left",
    sep = if ("colour" %in% names(mapping)) ": " else ":\n",
    title = title,
    text_fn = function(x, y) {
      if (is_date(x)) {
        x <- as.numeric(x)
      }
      if (is_date(y)) {
        y <- as.numeric(y)
      }

      if (length(x) <= 2 | length(y) <= 2) {
        warning("Less than 2 observations, returning NA")
        cor_txt <- NA
      } else {
        corObj <- stats::cor.test(x, y, method = method, use = use)

        # make sure all values have X-many decimal places
        cor_est <- as.numeric(corObj$estimate)
        cor_txt <- formatC(cor_est, digits = digits, format = "f")

        # if stars should be added
        if (isTRUE(stars)) {
          cor_txt <- str_c(
            cor_txt,
            signif_stars(corObj$p.value)
          )
        }
      }

      cor_txt
    }
  )
}


#' Generalized text display
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param title title text to be displayed
#' @param text_fn function that takes in \code{x} and \code{y} and returns a text string
#' @param na.rm logical value which determines if \code{NA} values are removed. If \code{TRUE}, no warning message will be displayed.
#' @param display_grid if \code{TRUE}, display aligned panel grid lines. If \code{FALSE} (default), display a thin panel border.
#' @param justify_labels \code{justify} argument supplied when \code{\link[base]{format}}ting the labels
#' @param justify_text \code{justify} argument supplied when \code{\link[base]{format}}ting the returned \code{text_fn(x, y)} values
#' @param sep separation value to be placed between the labels and text
#' @param family font family used when displaying all text.  This value will be set in \code{title_args} or \code{group_args} if no \code{family} value exists.  By using \code{"mono"}, groups will align with each other.
#' @param title_args arguments being supplied to the title's \code{\link[ggplot2]{geom_text}()}
#' @param group_args arguments being supplied to the split-by-color group's \code{\link[ggplot2]{geom_text}()}
#' @param align_percent relative align position of the text. When \code{title_hjust = 0.5} and \code{group_hjust = 0.5}, this should not be needed to be set.
#' @param title_hjust,group_hjust \code{hjust} sent to \code{\link[ggplot2]{geom_text}()} for the title and group values respectively. Any \code{hjust} value supplied in \code{title_args} or \code{group_args} will take precedence.
#' @seealso \code{\link{ggally_cor}}
#' @export
ggally_statistic <- function(
    data,
    mapping,
    text_fn,
    title,
    na.rm = NA,
    display_grid = FALSE,
    justify_labels = "right",
    justify_text = "left",
    sep = ": ",
    family = "mono",
    title_args = list(),
    group_args = list(),
    align_percent = 0.5,
    title_hjust = 0.5,
    group_hjust = 0.5) {
  set_if_not_there <- function(obj, key, value) {
    obj <- as.list(obj)
    # if (! "family" %in% rlang::names2(obj)) {
    #  obj$family <- family
    # }
    obj
  }

  # title_args <- set_if_not_there(title_args, "family", family)
  # group_args <- set_if_not_there(group_args, "family", family)

  title_args <- set_if_not_there(title_args, "hjust", title_hjust)
  group_args <- set_if_not_there(group_args, "hjust", group_hjust)

  xData <- eval_data_col(data, mapping$x)
  yData <- eval_data_col(data, mapping$y)
  colorData <- eval_data_col(data, mapping$colour)

  if (is.numeric(colorData)) {
    stop("`mapping` color column must be categorical, not numeric")
  }

  display_na_rm <- is.na(na.rm)
  if (display_na_rm) {
    na.rm <- TRUE
  }
  if (isTRUE(na.rm)) {
    if (!is.null(colorData) && (length(colorData) == length(xData))) {
      rows <- complete.cases(xData, yData, colorData)
    } else {
      rows <- complete.cases(xData, yData)
    }

    if (any(!rows)) {
      if (!is.null(colorData) && (length(colorData) == length(xData))) {
        colorData <- colorData[rows]
      }
      xData <- xData[rows]
      yData <- yData[rows]

      if (isTRUE(display_na_rm)) {
        total <- sum(!rows)
        if (total > 1) {
          warning("Removed ", total, " rows containing missing values")
        } else if (total == 1) {
          warning("Removing 1 row that contained a missing value")
        }
      }
    }
  }

  xVal <- xData
  yVal <- yData

  # if the mapping has to deal with the data, remove it
  ### IDK what this does. inherited from old code.
  for (mappingName in names(mapping)) {
    itemData <- eval_data_col(data, mapping[[mappingName]])
    if (!inherits(itemData, "AsIs")) {
      mapping[[mappingName]] <- NULL
    }
  }
  ### END IDK

  # calculate variable ranges so the gridlines line up
  xValNum <- as.numeric(xVal)
  yValNum <- as.numeric(yVal)
  xmin <- min(xValNum, na.rm = TRUE)
  xmax <- max(xValNum, na.rm = TRUE)
  xrange <- c(xmin - 0.01 * (xmax - xmin), xmax + 0.01 * (xmax - xmin))
  ymin <- min(yValNum, na.rm = TRUE)
  ymax <- max(yValNum, na.rm = TRUE)
  yrange <- c(ymin - 0.01 * (ymax - ymin), ymax + 0.01 * (ymax - ymin))

  # if there is a color grouping...
  if (!is.null(colorData) && !inherits(colorData, "AsIs")) {
    cord <- ddply(
      data.frame(x = xData, y = yData, color = colorData),
      "color",
      function(dt) {
        text_fn(dt$x, dt$y)
      }
    )
    colnames(cord)[2] <- "text"

    # put in correct order
    lev <- levels(as.factor(colorData))
    ord <- rep(-1, nrow(cord))
    for (i in 1:nrow(cord)) {
      for (j in seq_along(lev)) {
        if (identical(as.character(cord$color[i]), as.character(lev[j]))) {
          ord[i] <- j
        }
      }
    }
    cord <- cord[order(ord[ord >= 0]), ]

    # make labels align together
    cord$label <- str_c(
      format(cord$color, justify = justify_labels),
      sep,
      format(cord$text, justify = justify_text)
    )

    # title
    ggally_text_args <- append(
      list(
        label   = str_c(title, sep, text_fn(xVal, yVal)),
        mapping = mapping,
        xP      = 0.5,
        yP      = 0.9,
        xrange  = xrange,
        yrange  = yrange
      ),
      title_args
    )
    p <- do.call(ggally_text, ggally_text_args)

    xPos <- rep(align_percent, nrow(cord)) * diff(xrange) + min(xrange, na.rm = TRUE)
    yPos <- seq(from = 0.9, to = 0.2, length.out = nrow(cord) + 1)
    yPos <- yPos * diff(yrange) + min(yrange, na.rm = TRUE)
    yPos <- yPos[-1]

    cordf <- data.frame(xPos = xPos, yPos = yPos, labelp = cord$label)
    cordf$labelp <- factor(cordf$labelp, levels = cordf$labelp)

    # group text values
    geom_text_args <- append(
      list(
        data = cordf,
        aes(
          x = !!as.name("xPos"),
          y = !!as.name("yPos"),
          label = !!as.name("labelp"),
          color = !!as.name("labelp")
        )
      ),
      group_args
    )
    p <- p + do.call(geom_text, geom_text_args)
  } else {
    ggally_text_args <- append(
      list(
        label = paste0(title, sep, text_fn(xVal, yVal), collapse = ""),
        mapping,
        xP = 0.5,
        yP = 0.5,
        xrange = xrange,
        yrange = yrange
      ),
      title_args
    )

    p <- do.call(ggally_text, ggally_text_args)
  }

  if (!isTRUE(display_grid)) {
    p <- p +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(
          linetype = "solid",
          color = theme_get()$panel.background$fill,
          fill = "transparent"
        )
      )
  }

  p + theme(legend.position = "none")
}





#' Box plot
#'
#' Make a box plot with a given data set. \code{ggally_box_no_facet} will be a single panel plot, while \code{ggally_box} will be a faceted plot

#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... other arguments being supplied to geom_boxplot
#' @author Barret Schloerke
#' @keywords hplot
#' @export
#' @examples
#' # Small function to display plots only if it's interactive
#' p_ <- GGally::print_if_interactive
#'
#' data(tips)
#' p_(ggally_box(tips, mapping = ggplot2::aes(x = total_bill, y = sex)))
#' p_(ggally_box(
#'   tips,
#'   mapping        = ggplot2::aes(sex, total_bill, color = sex),
#'   outlier.colour = "red",
#'   outlier.shape  = 13,
#'   outlier.size   = 8
#' ))
ggally_box <- function(data, mapping, ...) {
  mapping <- mapping_color_to_fill(mapping)

  ggally_dot_and_box(data, mapping, ..., boxPlot = TRUE)
}
#' @export
#' @rdname ggally_box
ggally_box_no_facet <- function(data, mapping, ...) {
  mapping <- mapping_color_to_fill(mapping)

  ggally_dot_and_box_no_facet(data, mapping, ..., boxPlot = TRUE)
}


#' Grouped dot plot
#'
#' Add jittering with the box plot. \code{ggally_dot_no_facet} will be a single panel plot, while \code{ggally_dot} will be a faceted plot
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... other arguments being supplied to geom_jitter
#' @author Barret Schloerke
#' @keywords hplot
#' @export
#' @examples
#' # Small function to display plots only if it's interactive
#' p_ <- GGally::print_if_interactive
#'
#' data(tips)
#' p_(ggally_dot(tips, mapping = ggplot2::aes(x = total_bill, y = sex)))
#' p_(ggally_dot(
#'   tips,
#'   mapping = ggplot2::aes(sex, total_bill, color = sex)
#' ))
#' p_(ggally_dot(
#'   tips,
#'   mapping = ggplot2::aes(sex, total_bill, color = sex, shape = sex)
#' ) + ggplot2::scale_shape(solid = FALSE))
ggally_dot <- function(data, mapping, ...) {
  ggally_dot_and_box(data, mapping, ..., boxPlot = FALSE)
}
#' @export
#' @rdname ggally_dot
ggally_dot_no_facet <- function(data, mapping, ...) {
  ggally_dot_and_box_no_facet(data, mapping, ..., boxPlot = FALSE)
}


#' Box and dot plot
#'
#' Place box plots or dot plots on the graph
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... parameters passed to either geom_jitter or geom_boxplot
#' @param boxPlot boolean to decide to plot either box plots (TRUE) or dot plots (FALSE)
#' @author Barret Schloerke
#' @keywords internal
#' @export
#' @examples
#' # Small function to display plots only if it's interactive
#' p_ <- GGally::print_if_interactive
#'
#' data(tips)
#' p_(ggally_dot_and_box(
#'   tips,
#'   mapping = ggplot2::aes(x = total_bill, y = sex, color = sex),
#'   boxPlot = TRUE
#' ))
#' p_(ggally_dot_and_box(
#'   tips,
#'   mapping = ggplot2::aes(x = total_bill, y = sex, color = sex),
#'   boxPlot = FALSE
#' ))
ggally_dot_and_box <- function(data, mapping, ..., boxPlot = TRUE) {
  horizontal <- is_horizontal(data, mapping)

  if (horizontal) {
    mapping <- mapping_swap_x_y(mapping)
  }

  xVal <- mapping_string(mapping$x)

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
      facet_grid(paste(xVal, " ~ .", sep = ""), scales = "free_y") +
      theme(panel.spacing = unit(0.1, "lines"))
  }

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
        limits = rev(levels(as.factor(eval_data_col(data, mapping$x))))
      ) +
      coord_flip()
  }

  p
}



#' Faceted histogram
#'
#' Display subsetted histograms of the data in different panels.
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... parameters sent to stat_bin()
#' @author Barret Schloerke
#' @keywords hplot
#' @export
#' @examples
#' # Small function to display plots only if it's interactive
#' p_ <- GGally::print_if_interactive
#'
#' data(tips)
#' p_(ggally_facethist(tips, mapping = ggplot2::aes(x = tip, y = sex)))
#' p_(ggally_facethist(tips, mapping = ggplot2::aes(x = tip, y = sex), binwidth = 0.1))
ggally_facethist <- function(data, mapping, ...) {
  mapping <- mapping_color_to_fill(mapping)

  horizontal <- is_horizontal(data, mapping)

  if (!horizontal) {
    mapping <- mapping_swap_x_y(mapping)
  }

  xVal <- mapping_string(mapping$x)
  yVal <- mapping_string(mapping$y)
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


#' Faceted density plot
#'
#' Make density plots by displaying subsets of the data in different panels.
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... other arguments being sent to stat_density
#' @author Barret Schloerke
#' @keywords hplot
#' @export
#' @examples
#' # Small function to display plots only if it's interactive
#' p_ <- GGally::print_if_interactive
#'
#' data(tips)
#' p_(ggally_facetdensity(tips, mapping = ggplot2::aes(x = total_bill, y = sex)))
#' p_(ggally_facetdensity(
#'   tips,
#'   mapping = ggplot2::aes(sex, total_bill, color = sex)
#' ))
ggally_facetdensity <- function(data, mapping, ...) {
  ggally_facetdensitystrip(data, mapping, ..., den_strip = FALSE)
}

#' Tile plot with facets
#'
#' Displays a Tile Plot as densely as possible.
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... other arguments being sent to stat_bin
#' @author Barret Schloerke
#' @keywords hplot
#' @export
#' @examples
#' # Small function to display plots only if it's interactive
#' p_ <- GGally::print_if_interactive
#'
#' data(tips)
#' p_(ggally_denstrip(tips, mapping = ggplot2::aes(x = total_bill, y = sex)))
#' p_(ggally_denstrip(
#'   tips,
#'   mapping = ggplot2::aes(sex, tip), binwidth = 0.2
#' ) + ggplot2::scale_fill_gradient(low = "grey80", high = "black"))
ggally_denstrip <- function(data, mapping, ...) {
  mapping <- mapping_color_to_fill(mapping)

  ggally_facetdensitystrip(data, mapping, ..., den_strip = TRUE)
}

#' Density or tiles plot with facets
#'
#' Make tile plot or density plot as compact as possible.
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... other arguments being sent to either geom_histogram or stat_density
#' @param den_strip boolean to decide whether or not to plot a density strip(TRUE) or a facet density(FALSE) plot.
#' @author Barret Schloerke
#' @keywords hplot
#' @export
#' @examples
#' example(ggally_facetdensity)
#' example(ggally_denstrip)
ggally_facetdensitystrip <- function(data, mapping, ..., den_strip = FALSE) {
  horizontal <- is_horizontal(data, mapping)

  if (!horizontal) {
    mapping <- mapping_swap_x_y(mapping)
  }

  xVal <- mapping_string(mapping$x)
  yVal <- mapping_string(mapping$y)
  mappingY <- mapping$y # nolint
  mapping$y <- NULL # will be faceted

  p <- ggplot(data = data, mapping) +
    labs(x = xVal, y = yVal)

  if (identical(den_strip, TRUE)) {
    p <- p +
      geom_histogram(
        mapping = aes(fill = after_stat(!!as.name("density"))), # nolint
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
          y = after_stat(!!as.name("scaled")) * diff(range(x, na.rm = TRUE)) + min(x, na.rm = TRUE) # nolint
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


#' Univariate density plot
#'
#' Displays a density plot for the diagonal of a \code{\link{ggpairs}} plot matrix.
#'
#' @param data data set using
#' @param mapping aesthetics being used.
#' @param ... other arguments sent to stat_density
#' @param rescale boolean to decide whether or not to rescale the count output
#' @author Barret Schloerke
#' @keywords hplot
#' @export
#' @examples
#' # Small function to display plots only if it's interactive
#' p_ <- GGally::print_if_interactive
#'
#' data(tips)
#' p_(ggally_densityDiag(tips, mapping = ggplot2::aes(x = total_bill)))
#' p_(ggally_densityDiag(tips, mapping = ggplot2::aes(x = total_bill, color = day)))
ggally_densityDiag <- function(data, mapping, ..., rescale = FALSE) {
  mapping <- mapping_color_to_fill(mapping)

  p <- ggplot(data, mapping) +
    scale_y_continuous()

  if (identical(rescale, TRUE)) {
    p <- p +
      stat_density(
        aes(
          y = after_stat(!!as.name("scaled")) * diff(range(x, na.rm = TRUE)) + min(x, na.rm = TRUE) # nolint
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

#' Bar plot
#'
#' Displays a bar plot for the diagonal of a \code{\link{ggpairs}} plot matrix.
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... other arguments are sent to geom_bar
#' @param rescale boolean to decide whether or not to rescale the count output. Only applies to numeric data
#' @author Barret Schloerke
#' @keywords hplot
#' @export
#' @examples
#' # Small function to display plots only if it's interactive
#' p_ <- GGally::print_if_interactive
#'
#' data(tips)
#' p_(ggally_barDiag(tips, mapping = ggplot2::aes(x = day)))
#' p_(ggally_barDiag(tips, mapping = ggplot2::aes(x = tip), binwidth = 0.25))
ggally_barDiag <- function(data, mapping, ..., rescale = FALSE) {
  mapping <- mapping_color_to_fill(mapping)

  mapping$y <- NULL
  x_data <- eval_data_col(data, mapping$x)
  numer <- ("continuous" == plotting_data_type(x_data))

  p <- ggplot(data = data, mapping)

  if (is_date(x_data)) {
    p <- p + geom_histogram(...)
    # TODO make y axis lines match date positions
    # buildInfo <- ggplot_build(p + geom_bar(...))
    # histBarPerc <- buildInfo$data[[1]]$ncount
  } else if (numer) {
    if (identical(rescale, TRUE)) {
      p <- p + geom_histogram(
        aes(
          y = after_stat(!!as.name("density")) / max(after_stat(!!as.name("density"))) * diff(range(x, na.rm = TRUE)) + min(x, na.rm = TRUE) # nolint
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


#' Text plot
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
#' @author Barret Schloerke
#' @keywords hplot
#' @export
#' @examples
#' # Small function to display plots only if it's interactive
#' p_ <- GGally::print_if_interactive
#'
#' p_(ggally_text("Example 1"))
#' p_(ggally_text("Example\nTwo", mapping = ggplot2::aes(size = 15), color = I("red")))
ggally_text <- function(
    label,
    mapping = ggplot2::aes(color = I("black")),
    xP = 0.5,
    yP = 0.5,
    xrange = c(0, 1),
    yrange = c(0, 1),
    ...) {
  theme <- theme_get()

  p <- ggplot() +
    xlim(xrange) +
    ylim(yrange) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(
        colour = ifnull(theme$panel.background$fill, NA)
      ),
      panel.background = element_rect(
        fill = ifnull(theme$panel.grid.major$colour, NA)
      )
    ) +
    labs(x = NULL, y = NULL)

  new_mapping <- aes(
    x = !!xP * diff(xrange) + min(xrange, na.rm = TRUE),
    y = !!yP * diff(yrange) + min(yrange, na.rm = TRUE)
  )
  if (is.null(mapping)) {
    mapping <- new_mapping
  } else {
    mapping <- add_and_overwrite_aes(mapping, new_mapping)
  }

  # dont mess with color if it's already there
  if (!is.null(mapping$colour)) {
    p <- p +
      geom_text(label = label, mapping = mapping, ...) +
      guides(colour = "none")
  } else if ("colour" %in% names(aes(...))) {
    p <- p +
      geom_text(label = label, mapping = mapping, ...)
  } else {
    bg <- ifnull(theme$panel.background$fill, "grey92")
    fg <- ifnull(theme$axis.text$colour, "gray30")
    colour <- scales::colour_ramp(c(bg, fg))(0.75)
    p <- p +
      geom_text(label = label, mapping = mapping, colour = colour, ...)
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
      if (str_detect(item$name, name)) {
        return(item$children[[1]])
      }
    }
    NULL
  }
  xAxisGrob <- get_raw_grob_by_name(axisTable, "title")

  axisBreaks <- as.numeric(xAxisGrob$label)

  axisLabs <- rbind(
    expand.grid(xPos = axisBreaks[1], yPos = axisBreaks),
    expand.grid(xPos = axisBreaks, yPos = axisBreaks[1])
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

#' Internal axis labels for ggpairs
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
#' @author Jason Crowley and Barret Schloerke
#' @export
#' @examples
#' # Small function to display plots only if it's interactive
#' p_ <- GGally::print_if_interactive
#'
#' data(tips)
#' p_(ggally_diagAxis(tips, ggplot2::aes(x = tip)))
#' p_(ggally_diagAxis(tips, ggplot2::aes(x = sex)))
ggally_diagAxis <- function(
    data,
    mapping,
    label = mapping$x,
    labelSize = 5,
    labelXPercent = 0.5,
    labelYPercent = 0.55,
    labelHJust = 0.5,
    labelVJust = 0.5,
    gridLabelSize = 4,
    ...) {
  if (is.null(mapping$x)) {
    stop("mapping$x is null.  There must be a column value in this location.")
  }
  mapping$y <- NULL
  numer <- !is_horizontal(data, mapping, "x")

  if (!is.character(label)) {
    label <- mapping_string(mapping$x)
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
      mapping = aes(col = I("grey50")),
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
      data = axisBreaks,
      mapping = aes(
        x     = !!as.name("xPos"),
        y     = !!as.name("yPos"),
        label = !!as.name("lab"),
        hjust = !!as.name("hjust"),
        vjust = !!as.name("vjust")
      ),
      col = "grey50",
      size = gridLabelSize
    )
  } else {
    breakLabels <- levels(as.factor(xData))
    numLvls <- length(breakLabels)

    p <- ggally_text(
      label   = label,
      mapping = aes(col = I("grey50")),
      xrange  = c(0, 1),
      yrange  = c(0, 1),
      size    = labelSize,
      yP      = labelYPercent,
      xP      = labelXPercent,
      hjust   = labelHJust,
      vjust   = labelVJust
    )
    # axisBreaks <- (1+2*0:(numLvls-1))/(2*numLvls)
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
        x     = !!as.name("x"),
        y     = !!as.name("y"),
        label = !!as.name("lab")
      ),
      col = "grey50",
      size = gridLabelSize
    )

    # hack to remove warning message... cuz it doesn't listen to suppress messages
    p$scales$scales[[1]]$breaks <- axisBreaks
    p$scales$scales[[2]]$breaks <- axisBreaks
    # pLabs <- pLabs +
    #   scale_x_continuous(breaks = axisBreaks, limits = c(0, 1)) +
    #   scale_y_continuous(breaks = axisBreaks, limits = c(0, 1))
  }

  p
}

#' Faceted bar plot
#'
#' X variables are plotted using \code{geom_bar} and are faceted by the Y variable.
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... other arguments are sent to geom_bar
#' @author Barret Schloerke
#' @keywords hplot
#' @export
#' @examples
#' # Small function to display plots only if it's interactive
#' p_ <- GGally::print_if_interactive
#'
#' data(tips)
#' p_(ggally_facetbar(tips, ggplot2::aes(x = sex, y = smoker, fill = time)))
#' p_(ggally_facetbar(tips, ggplot2::aes(x = smoker, y = sex, fill = time)))
ggally_facetbar <- function(data, mapping, ...) {
  mapping <- mapping_color_to_fill(mapping)

  # numer <- is.null(attributes(data[, as.character(mapping$x)])$class)
  # xVal <- mapping$x
  yVal <- mapping_string(mapping$y)
  mapping$y <- NULL
  p <- ggplot(data, mapping) +
    geom_bar(...) +
    facet_grid(paste(yVal, " ~ .", sep = ""))

  p
}


#' Mosaic plot
#'
#' Plots the mosaic plot by using fluctuation.
#'
#' @param data data set using
#' @param mapping aesthetics being used. Only x and y will used and both are required
#' @param ... passed to \code{\link[ggplot2]{geom_tile}(...)}
#' @param floor don't display cells smaller than this value
#' @param ceiling max value to scale frequencies.  If any frequency is larger than the ceiling, the fill color is displayed darker than other rectangles
#' @author Barret Schloerke
#' @keywords hplot
#' @export
#' @examples
#' # Small function to display plots only if it's interactive
#' p_ <- GGally::print_if_interactive
#'
#' data(tips)
#' p_(ggally_ratio(tips, ggplot2::aes(sex, day)))
#' p_(ggally_ratio(tips, ggplot2::aes(sex, day)) + ggplot2::coord_equal())
#' # only plot tiles greater or equal to 20 and scale to a max of 50
#' p_(ggally_ratio(
#'   tips, ggplot2::aes(sex, day),
#'   floor = 20, ceiling = 50
#' ) + ggplot2::theme(aspect.ratio = 4 / 2))
ggally_ratio <- function(
    data,
    mapping = ggplot2::aes(!!!stats::setNames(lapply(colnames(data)[1:2], as.name), c("x", "y"))),
    ...,
    floor = 0,
    ceiling = NULL) {
  # capture the original names
  xName <- mapping_string(mapping$x)
  yName <- mapping_string(mapping$y)

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

  p <-
    ggplot(
      data = countData,
      mapping = aes(
        x = !!as.name("xPos"),
        y = !!as.name("yPos"),
        height = !!as.name("freqSize"),
        width = !!as.name("freqSize"),
        fill = !!as.name("col")
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



#' Display counts of observations
#'
#' Plot the number of observations by using rectangles
#' with proportional areas.
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... other arguments passed to \code{\link[ggplot2]{geom_tile}(...)}
#' @details
#'   You can adjust the size of rectangles with the \code{x.width} argument.
#' @author Joseph Larmarange
#' @keywords hplot
#' @export
#' @examples
#' # Small function to display plots only if it's interactive
#' p_ <- GGally::print_if_interactive
#'
#' data(tips)
#' p_(ggally_count(tips, mapping = ggplot2::aes(x = smoker, y = sex)))
#' p_(ggally_count(tips, mapping = ggplot2::aes(x = smoker, y = sex, fill = day)))
#'
#' p_(ggally_count(
#'   as.data.frame(Titanic),
#'   mapping = ggplot2::aes(x = Class, y = Survived, weight = Freq)
#' ))
#' p_(ggally_count(
#'   as.data.frame(Titanic),
#'   mapping = ggplot2::aes(x = Class, y = Survived, weight = Freq),
#'   x.width = 0.5
#' ))
ggally_count <- function(data, mapping, ...) {
  mapping <- mapping_color_to_fill(mapping)
  if (is.null(mapping$x)) stop("'x' aesthetic is required.")
  if (is.null(mapping$y)) stop("'y' aesthetic is required.")
  # for stat_ggally_count(), y should be mapped to base_y
  # and always be a factor
  count_col <- ".ggally_y"
  data[[count_col]] <- as.factor(eval_data_col(data, mapping$y))

  # Reverse the y axis here. I'd like to perform this in the
  # `scale_y_continuous(trans="reverse")`, but the trans is applied after
  # `breaks/labels`
  data[[count_col]] <- factor(data[[count_col]], levels = rev(levels(data[[count_col]])))

  ylabel <- mapping_string(mapping$y)
  mapping$base_y <- aes(base_y = !!as.name(count_col))$base_y
  mapping$y <- NULL

  # default values
  args <- list(...)
  if (!"fill" %in% names(args)) {
    if (is.null(mapping$fill)) {
      args$fill <- GeomRect$default_aes$fill
    }
  }

  ggplot(data, mapping) +
    do.call(stat_ggally_count, args) +
    scale_y_continuous(
      breaks = seq_along(levels(data[[count_col]])),
      labels = levels(data[[count_col]])
    ) +
    theme(panel.grid.minor = element_blank()) +
    ylab(ylabel)
}

#' @export
#' @rdname ggally_count
#' @format NULL
#' @usage NULL
#' @export
# na.rm = TRUE to remove warnings if NA (cf. stat_count)
# x.width to control size of tiles
stat_ggally_count <- function(
    mapping = NULL,
    data = NULL,
    geom = "tile", position = "identity",
    ...,
    x.width = .9,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE) {
  params <- list(
    x.width = x.width,
    na.rm = na.rm,
    ...
  )
  if (!is.null(params$y)) {
    stop("stat_ggally_count() must not be used with a y aesthetic,
         but with a base_y aesthetic instead.", call. = FALSE)
  }

  layer(
    data = data,
    mapping = mapping,
    stat = StatGGallyCount,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
}

#' @rdname ggally_count
#' @format NULL
#' @usage NULL
#' @export
StatGGallyCount <- ggproto("StatGGallyCount", Stat,
  required_aes = c("x", "base_y"),
  default_aes = aes(
    weight = 1,
    width = after_stat(width),
    height = after_stat(height),
    y = after_stat(y)
  ),
  setup_params = function(data, params) {
    params
  },
  extra_params = c("na.rm"),
  compute_panel = function(self, data, scales, x.width = NULL) {
    if (is.null(data$weight)) {
      data$weight <- rep(1, nrow(data))
    }

    if (is.null(x.width)) {
      x.width <- .9
    }

    # sum weights for each combination of aesthetics
    # the use of . allows to consider all aesthetics defined in data
    panel <- stats::aggregate(weight ~ ., data = data, sum, na.rm = TRUE)

    names(panel)[which(names(panel) == "weight")] <- "n"

    # Reverse both the y and fill values here.
    # This makes the colors appear the correct order
    # If it is a single color, it won't make any difference in the cum_height
    panel <- panel[rev(seq_len(nrow(panel))), ]

    # compute proportions by x and y
    f <- function(n) {
      sum(abs(n), na.rm = TRUE)
    }
    panel$n_xy <- stats::ave(panel$n, panel$x, panel$base_y, FUN = f)
    panel$prop <- panel$n / panel$n_xy
    panel$width <- sqrt(panel$n_xy) / max(sqrt(panel$n_xy)) * x.width
    panel$height <- panel$width * panel$prop
    panel$cum_height <- stats::ave(panel$height, panel$x, panel$base_y, FUN = cumsum)
    panel$y <- as.numeric(panel$base_y) + panel$cum_height -
      panel$height / 2 - panel$width / 2

    panel
  }
)


#' @rdname ggally_count
#' @export
#' @examples
#' # Small function to display plots only if it's interactive
#' p_ <- GGally::print_if_interactive
#'
#' p_(ggally_countDiag(tips, mapping = ggplot2::aes(x = smoker)))
#' p_(ggally_countDiag(tips, mapping = ggplot2::aes(x = smoker, fill = sex)))
ggally_countDiag <- function(data, mapping, ...) {
  mapping$y <- mapping$x
  ggally_count(data = data, mapping = mapping, ...)
}

#' Blank plot
#'
#' Draws nothing.
#'
#' Makes a "blank" ggplot object that will only draw white space
#'
#' @author Barret Schloerke
#' @param ... other arguments ignored
#' @seealso [ggplot2::element_blank()]
#' @export
#' @keywords hplot
ggally_blank <- function(...) {
  aes(...) # ignored
  a <- data.frame(X = 1:2, Y = 1:2)

  p <- ggplot(data = a, aes(x = !!as.name("X"), y = !!as.name("Y"))) +
    geom_point(colour = "transparent") +
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
#' @author Barret Schloerke
#' @param data ignored
#' @param mapping ignored
#' @param size size of the geom_text 'NA'
#' @param color color of the geom_text 'NA'
#' @param ... other arguments sent to geom_text
#' @export
#' @keywords hplot
ggally_na <- function(data = NULL, mapping = NULL, size = 10, color = "grey20", ...) {
  a <- data.frame(x = 1, y = 1, label = "NA")

  p <- ggplot(data = a, aes(x = !!as.name("X"), y = !!as.name("Y"), label = !!as.name("label"))) +
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



#' Scatterplot for continuous and categorical variables
#'
#' Make scatterplots compatible with both continuous and categorical variables
#' using \code{\link[ggforce]{geom_autopoint}} from package \pkg{ggforce}.
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... other arguments passed to \code{\link[ggforce]{geom_autopoint}(...)}
#' @author Joseph Larmarange
#' @keywords hplot
#' @export
#' @examples
#' # Small function to display plots only if it's interactive
#' p_ <- GGally::print_if_interactive
#'
#' data(tips)
#' p_(ggally_autopoint(tips, mapping = aes(x = tip, y = total_bill)))
#' p_(ggally_autopoint(tips, mapping = aes(x = tip, y = sex)))
#' p_(ggally_autopoint(tips, mapping = aes(x = smoker, y = sex)))
#' p_(ggally_autopoint(tips, mapping = aes(x = smoker, y = sex, color = day)))
#' p_(ggally_autopoint(tips, mapping = aes(x = smoker, y = sex), size = 8))
#' p_(ggally_autopoint(tips, mapping = aes(x = smoker, y = sex), alpha = .9))
#'
#' p_(ggpairs(
#'   tips,
#'   mapping = aes(colour = sex),
#'   upper = list(discrete = "autopoint", combo = "autopoint", continuous = "autopoint"),
#'   diag = list(discrete = "autopointDiag", continuous = "autopointDiag")
#' ))
ggally_autopoint <- function(data, mapping, ...) {
  require_namespaces("ggforce")

  args <- list(...)
  if (!"alpha" %in% names(args) && is.null(mapping$alpha)) {
    args$alpha <- .5
  }
  # mapping needs to be sent directly to geom_autopoint
  args$mapping <- mapping

  ggplot(data, mapping) +
    do.call(ggforce::geom_autopoint, args)
}

#' @rdname ggally_autopoint
#' @export
ggally_autopointDiag <- function(data, mapping, ...) {
  mapping$y <- mapping$x
  ggally_autopoint(data = data, mapping = mapping, ...)
}


#' Summarize a continuous variable by each value of a discrete variable
#'
#' Display summary statistics of a continuous variable for each value of a discrete variable.
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param text_fn function that takes an x and weights and returns a text string
#' @param text_fn_vertical function that takes an x and weights and returns a text string, used when \code{x} is discrete and \code{y} is continuous. If not provided, will use \code{text_fn}, replacing spaces by carriage returns.
#' @param ... other arguments passed to \code{\link[ggplot2]{geom_text}(...)}
#' @author Joseph Larmarange
#' @keywords hplot
#' @export
#' @examples
#' # Small function to display plots only if it's interactive
#' p_ <- GGally::print_if_interactive
#'
#' if (require(Hmisc)) {
#'   data(tips)
#'   p_(ggally_summarise_by(tips, mapping = aes(x = total_bill, y = day)))
#'   p_(ggally_summarise_by(tips, mapping = aes(x = day, y = total_bill)))
#'
#'   # colour is kept only if equal to the discrete variable
#'   p_(ggally_summarise_by(tips, mapping = aes(x = total_bill, y = day, color = day)))
#'   p_(ggally_summarise_by(tips, mapping = aes(x = total_bill, y = day, color = sex)))
#'   p_(ggally_summarise_by(tips, mapping = aes(x = day, y = total_bill, color = day)))
#'
#'   # custom text size
#'   p_(ggally_summarise_by(tips, mapping = aes(x = total_bill, y = day), size = 6))
#'
#'   # change statistic to display
#'   p_(ggally_summarise_by(tips, mapping = aes(x = total_bill, y = day), text_fn = weighted_mean_sd))
#'
#'   # custom stat function
#'   weighted_sum <- function(x, weights = NULL) {
#'     if (is.null(weights)) weights <- 1
#'     paste0("Total : ", round(sum(x * weights, na.rm = TRUE), digits = 1))
#'   }
#'   p_(ggally_summarise_by(tips, mapping = aes(x = total_bill, y = day), text_fn = weighted_sum))
#' }
ggally_summarise_by <- function(
    data,
    mapping,
    text_fn = weighted_median_iqr,
    text_fn_vertical = NULL,
    ...) {
  if (is.null(mapping$x)) stop("'x' aesthetic is required.")
  if (is.null(mapping$y)) stop("'y' aesthetic is required.")

  horizontal <- is_horizontal(data, mapping)
  if (horizontal) {
    res <- ddply(
      data.frame(
        x = eval_data_col(data, mapping$x),
        y = eval_data_col(data, mapping$y),
        weight = eval_data_col(data, mapping$weight) %||% 1,
        stringsAsFactors = FALSE
      ),
      c("y"),
      plyr::here(summarize),
      label = text_fn(x, weight)
    )
    # keep colour if matching the discrete variable
    if (mapping_string(mapping$colour) == mapping_string(mapping$y)) {
      col <- as.name("y")
    } else {
      col <- NULL
    }

    ggplot(res) +
      aes(y = !!as.name("y"), x = 1, label = !!as.name("label"), colour = !!col) +
      geom_text(...) +
      xlab("") +
      ylab(mapping_string(mapping$y)) +
      # theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_rect(
          linetype = "solid",
          color = theme_get()$panel.background$fill,
          fill = "transparent"
        )
      )
  } else {
    if (is.null(text_fn_vertical)) {
      text_fn_vertical <- function(x, weights) {
        gsub(" ", "\n", text_fn(x, weights))
      }
    }
    ggally_summarise_by(data, mapping_swap_x_y(mapping), text_fn_vertical, ...) +
      coord_flip() +
      theme(
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = theme_get()$axis.text,
        axis.ticks.x = theme_get()$axis.ticks
      ) +
      theme(axis.text.x = element_text(size = 9))
  }
}

#' @rdname ggally_summarise_by
#' @param x a numeric vector
#' @param weights an optional numeric vectors of weights. If \code{NULL}, equal weights of 1 will be taken into account.
#' @details
#' \code{weighted_median_iqr} computes weighted median and interquartile range.
#' @export
weighted_median_iqr <- function(x, weights = NULL) {
  require_namespaces("Hmisc")
  s <- round(Hmisc::wtd.quantile(x, weights = weights, probs = c(.25, .5, .75), na.rm = TRUE), digits = 1)
  paste0("Median: ", s[2], " [", s[1], "-", s[3], "]")
}

#' @rdname ggally_summarise_by
#' @details
#' \code{weighted_mean_sd} computes weighted mean and standard deviation.
#' @export
weighted_mean_sd <- function(x, weights = NULL) {
  require_namespaces("Hmisc")
  m <- round(Hmisc::wtd.mean(x, weights = weights, na.rm = TRUE), digits = 1)
  sd <- round(sqrt(Hmisc::wtd.var(x, weights = weights, na.rm = TRUE)), digits = 1)
  paste0("Mean: ", m, " (", sd, ")")
}
