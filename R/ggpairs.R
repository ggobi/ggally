# list of the different plot types to check
# continuous
#    points
#    smooth
#    smooth_loess
#    density
#    cor
#   blank

# combo
#   box
#   box_no_facet
#   dot
#   dot_no_facet
#   facethist
#   facetdensity
#   denstrip
#   blank

# discrete
#   ratio
#   facetbar
#   blank

# diag
#   continuous
#     densityDiag
#     barDiag
#     blankDiag
#   discrete
#     barDiag
#     blankDiag

crosstalk_key <- function() {
  ".crossTalkKey"
}

fortify_SharedData <- function(model, data, ...) {
  key <- model$key()
  set <- model$groupName()
  data <- model$origData()
  # need a consistent name so we know how to access it in ggplotly()
  # MUST be added last. can NOT be done first
  data[[crosstalk_key()]] <- key
  structure(data, set = set)
}

fix_data <- function(data) {

  if (inherits(data, "SharedData")) {
    data <- fortify_SharedData(data)
  }

  data <- fortify(data)
  data <- as.data.frame(data)

  for (i in 1:dim(data)[2] ) {
    if (is.character(data[[i]])) {
      data[[i]] <- as.factor(data[[i]])
    }
  }

  data
}
fix_data_slim <- function(data, isSharedData) {
  if (isSharedData) {
    data[[crosstalk_key()]] <- NULL
  }
  data
}


fix_column_values <- function(
  data,
  columns,
  columnLabels,
  columnsName,
  columnLabelsName,
  isSharedData = FALSE
) {

  colnamesData <- colnames(data)

  if (is.character(columns)) {
    colNumValues <- lapply(columns, function(colName){
      which(colnamesData == colName)
    })
    isFound <- as.logical(unlist(lapply(colNumValues, length)))
    if (any(!isFound)) {
      stop(
        "Columns in '", columnsName, "' not found in data: c(",
        str_c(str_c("'", columns[!isFound], "'"), collapse = ", "),
        "). Choices: c('", paste(colnamesData, collapse = "', '"), "')"
      )
    }
    columns <- unlist(colNumValues)
  }

  if (any(columns > ncol(data))) {
    stop(
      "Make sure your numeric '", columnsName, "'",
      " values are less than or equal to ", ncol(data), ".\n",
      "\t", columnsName, " = c(", str_c(columns, collapse = ", "), ")"
    )
  }
  if (any(columns < 1)) {
    stop(
      "Make sure your numeric '", columnsName, "' values are positive.", "\n",
      "\t", columnsName, " = c(", paste(columns, collapse = ", "), ")"
    )
  }
  if (any( (columns %% 1) != 0)) {
    stop(
      "Make sure your numeric '", columnsName, "' values are integers.", "\n",
      "\t", columnsName, " = c(", paste(columns, collapse = ", "), ")"
    )
  }

  if (!is.null(columnLabels)) {
    if (length(columnLabels) != length(columns)) {
      stop(
        "The length of the '", columnLabelsName, "'",
        " does not match the length of the '", columnsName, "' being used.",
        " Labels: c('", paste(columnLabels, collapse = ", "), "')\n",
        " Columns: c('", paste(columns, collapse = ", "), "')"
      )
    }
  }

  columns
}

warn_deprecated <- function(is_supplied, title) {
  if (is_supplied) {
    warning(paste(
      "'", title, "' will be deprecated in future versions.  Please remove it from your code",
      sep = ""
    ))
  }
}

stop_if_bad_mapping <- function(mapping) {
  if (is.numeric(mapping)) {
    stop(
      "'mapping' should not be numeric",
      " unless 'columns' is missing from function call."
    )
  }
}

warn_if_args_exist <- function(args) {
  if (length(args) > 0) {
    argNames <- names(args)
    warning(str_c(
      "Extra arguments: ",
      str_c(shQuote(argNames), collapse = ", "), " are being ignored.",
      "  If these are meant to be aesthetics, submit them using the",
      " 'mapping' variable within ggpairs with ggplot2::aes or ggplot2::aes_string."
    ))
  }
}

fix_axis_label_choice <- function(axisLabels, axisLabelChoices) {
  if (length(axisLabels) > 1) {
    axisLabels <- axisLabels[1]
  }
  axisLabelChoice <- pmatch(axisLabels, axisLabelChoices)
  if (is.na(axisLabelChoice)) {
    warning(str_c(
      "'axisLabels' not in c(",
      str_c(str_c("'", axisLabelChoices, "'"), collapse = ", "),
      ").  Reverting to '", axisLabelChoices[1], "'"
    ))
    axisLabelChoice <- 1
  }
  axisLabels <- axisLabelChoices[axisLabelChoice]
}

stop_if_high_cardinality <- function(data, columns, threshold) {
  if (is.null(threshold)) {
    return()
  }
  if (identical(threshold, FALSE)) {
    return()
  }
  if (!is.numeric(threshold)) {
    stop("'cardinality_threshold' should be a numeric or NULL")
  }
  for (col in names(data[columns])) {
    data_col <- data[[col]]
    if (!is.numeric(data_col)) {
      level_length <- length(levels(data_col))
      if (level_length > threshold) {
        stop(
          "Column '", col, "' has more levels (", level_length, ")",
          " than the threshold (", threshold, ") allowed.\n",
          "Please remove the column or increase the 'cardinality_threshold' parameter. Increasing the cardinality_threshold may produce long processing times" # nolint
        )
      }
    }
  }
}



#' ggduo - A ggplot2 generalized pairs plot for two columns sets of a data.frame
#'
#' Make a matrix of plots with a given data set with two different column sets
#'
#' @details
#' \code{types} is a list that may contain the variables
#' 'continuous', 'combo', 'discrete', and 'na'. Each element of the list may be a function or a string.  If a string is supplied, it must implement one of the following options:
#'\describe{
#'  \item{continuous}{exactly one of ('points', 'smooth', 'smooth_loess', 'density', 'cor', 'blank'). This option is used for continuous X and Y data.}
#'  \item{comboHorizontal}{exactly one of ('box', 'box_no_facet', 'dot', 'dot_no_facet', 'facethist', 'facetdensity', 'denstrip', 'blank'). This option is used for either continuous X and categorical Y data or categorical X and continuous Y data.}
#'  \item{comboVertical}{exactly one of ('box', 'box_no_facet', 'dot', 'dot_no_facet', 'facethist', 'facetdensity', 'denstrip', 'blank'). This option is used for either continuous X and categorical Y data or categorical X and continuous Y data.}
#'  \item{discrete}{exactly one of ('facetbar', 'ratio', 'blank'). This option is used for categorical X and Y data.}
#'  \item{na}{exactly one of ('na', 'blank').  This option is used when all X data is \code{NA}, all Y data is \code{NA}, or either all X or Y data is \code{NA}.}
#'}
#'
#' If 'blank' is ever chosen as an option, then ggduo will produce an empty plot.
#'
#' If a function is supplied as an option, it should implement the function api of \code{function(data, mapping, ...){#make ggplot2 plot}}.  If a specific function needs its parameters set, \code{\link{wrap}(fn, param1 = val1, param2 = val2)} the function with its parameters.
#'
#' @export
#' @param data data set using.  Can have both numerical and categorical data.
#' @param mapping aesthetic mapping (besides \code{x} and \code{y}).  See \code{\link[ggplot2]{aes}()}.  If \code{mapping} is numeric, \code{columns} will be set to the \code{mapping} value and \code{mapping} will be set to \code{NULL}.
#' @param columnsX,columnsY which columns are used to make plots.  Defaults to all columns.
#' @param title,xlab,ylab title, x label, and y label for the graph
#' @param types see Details
#' @param axisLabels either "show" to display axisLabels or "none" for no axis labels
#' @param columnLabelsX,columnLabelsY label names to be displayed.  Defaults to names of columns being used.
#' @template ggmatrix-labeller-param
#' @template ggmatrix-switch-param
#' @param showStrips boolean to determine if each plot's strips should be displayed. \code{NULL} will default to the top and right side plots only. \code{TRUE} or \code{FALSE} will turn all strips on or off respectively.
#' @template ggmatrix-legend-param
#' @param cardinality_threshold maximum number of levels allowed in a character / factor column.  Set this value to NULL to not check factor columns. Defaults to 15
#' @param legends deprecated
#' @export
#' @examples
#'  # small function to display plots only if it's interactive
#'  p_ <- GGally::print_if_interactive
#'
#'  data(baseball, package = "plyr")
#'
#'  # Keep players from 1990-1995 with at least one at bat
#'  # Add how many singles a player hit
#'  # (must do in two steps as X1b is used in calculations)
#'  dt <- transform(
#'    subset(baseball, year >= 1990 & year <= 1995 & ab > 0),
#'    X1b = h - X2b - X3b - hr
#'  )
#'  # Add
#'  #  the player's batting average,
#'  #  the player's slugging percentage,
#'  #  and the player's on base percentage
#'  # Make factor a year, as each season is discrete
#'  dt <- transform(
#'    dt,
#'    batting_avg = h / ab,
#'    slug = (X1b + 2*X2b + 3*X3b + 4*hr) / ab,
#'    on_base = (h + bb + hbp) / (ab + bb + hbp),
#'    year = as.factor(year)
#'  )
#'
#'
#'  pm <- ggduo(
#'    dt,
#'    c("year", "g", "ab", "lg"),
#'    c("batting_avg", "slug", "on_base"),
#'    mapping = ggplot2::aes(color = lg)
#'  )
#'  # Prints, but
#'  #   there is severe over plotting in the continuous plots
#'  #   the labels could be better
#'  #   want to add more hitting information
#'  p_(pm)
#'
#'  # address overplotting issues and add a title
#'  pm <- ggduo(
#'    dt,
#'    c("year", "g", "ab", "lg"),
#'    c("batting_avg", "slug", "on_base"),
#'    columnLabelsX = c("year", "player game count", "player at bat count", "league"),
#'    columnLabelsY = c("batting avg", "slug %", "on base %"),
#'    title = "Baseball Hitting Stats from 1990-1995",
#'    mapping = ggplot2::aes(color = lg),
#'    types = list(
#'      # change the shape and add some transparency to the points
#'      continuous = wrap("smooth_loess", alpha = 0.50, shape = "+")
#'    ),
#'    showStrips = FALSE
#'  );
#'
#'  p_(pm)
#'
#'
#'
#' # Example derived from:
#' ## R Data Analysis Examples | Canonical Correlation Analysis.  UCLA: Institute for Digital
#' ##   Research and Education.
#' ##   from http://www.stats.idre.ucla.edu/r/dae/canonical-correlation-analysis
#' ##   (accessed May 22, 2017).
#' # "Example 1. A researcher has collected data on three psychological variables, four
#' #  academic variables (standardized test scores) and gender for 600 college freshman.
#' #  She is interested in how the set of psychological variables relates to the academic
#' #  variables and gender. In particular, the researcher is interested in how many
#' #  dimensions (canonical variables) are necessary to understand the association between
#' #  the two sets of variables."
#' data(psychademic)
#' summary(psychademic)
#'
#' (psych_variables <- attr(psychademic, "psychology"))
#' (academic_variables <- attr(psychademic, "academic"))
#'
#' ## Within correlation
#' p_(ggpairs(psychademic, columns = psych_variables))
#' p_(ggpairs(psychademic, columns = academic_variables))
#'
#' ## Between correlation
#' loess_with_cor <- function(data, mapping, ..., method = "pearson") {
#'   x <- eval(mapping$x, data)
#'   y <- eval(mapping$y, data)
#'   cor <- cor(x, y, method = method)
#'   ggally_smooth_loess(data, mapping, ...) +
#'     ggplot2::geom_label(
#'       data = data.frame(
#'         x = min(x, na.rm = TRUE),
#'         y = max(y, na.rm = TRUE),
#'         lab = round(cor, digits = 3)
#'       ),
#'       mapping = ggplot2::aes(x = x, y = y, label = lab),
#'       hjust = 0, vjust = 1,
#'       size = 5, fontface = "bold",
#'       inherit.aes = FALSE # do not inherit anything from the ...
#'     )
#' }
#' pm <- ggduo(
#'   psychademic,
#'   rev(psych_variables), academic_variables,
#'   types = list(continuous = loess_with_cor),
#'   showStrips = FALSE
#' )
#' suppressWarnings(p_(pm)) # ignore warnings from loess
#'
#' # add color according to sex
#' pm <- ggduo(
#'   psychademic,
#'   mapping = ggplot2::aes(color = sex),
#'   rev(psych_variables), academic_variables,
#'   types = list(continuous = loess_with_cor),
#'   showStrips = FALSE,
#'   legend = c(5,2)
#' )
#' suppressWarnings(p_(pm))
#'
#'
#' # add color according to sex
#' pm <- ggduo(
#'   psychademic,
#'   mapping = ggplot2::aes(color = motivation),
#'   rev(psych_variables), academic_variables,
#'   types = list(continuous = loess_with_cor),
#'   showStrips = FALSE,
#'   legend = c(5,2)
#' ) +
#'   ggplot2::theme(legend.position = "bottom")
#' suppressWarnings(p_(pm))
#
#
#
# pm <- ggduo(
#   dt,
#   c("year", "g", "ab", "lg", "lg"),
#   c("batting_avg", "slug", "on_base", "hit_type"),
#   columnLabelsX = c("year", "player game count", "player at bat count", "league", ""),
#   columnLabelsY = c("batting avg", "slug %", "on base %", "hit type"),
#   title = "Baseball Hitting Stats from 1990-1995 (player strike in 1994)",
#   mapping = aes(color = year),
#   types = list(
#     continuous = wrap("smooth_loess", alpha = 0.50, shape = "+"),
#     comboHorizontal = wrap(display_hit_type_combo, binwidth = 15),
#     discrete = wrap(display_hit_type_discrete, color = "black", size = 0.15)
#   ),
#   showStrips = FALSE
# );
#
# # make the 5th column blank, except for the legend
# pm[1,5] <- NULL
# pm[2,5] <- grab_legend(pm[2,1])
# pm[3,5] <- NULL
# pm[4,5] <- NULL
#
# pm
#
# ggduo(
#   australia_PISA2012,
#   c("gender", "age", "homework", "possessions"),
#   c("PV1MATH", "PV2MATH", "PV3MATH", "PV4MATH", "PV5MATH"),
#   types = list(
#     continuous = "points",
#     combo = "box",
#     discrete = "ratio"
#   )
# )
#
# ggduo(
#   australia_PISA2012,
#   c("gender", "age", "homework", "possessions"),
#   c("PV1MATH", "PV2MATH", "PV3MATH", "PV4MATH", "PV5MATH"),
#   mapping = ggplot2::aes(color = gender),
#   types = list(
#     continuous = wrap("smooth", alpha = 0.25, method = "loess"),
#     combo = "box",
#     discrete = "ratio"
#   )
# )
#
# ggduo(australia_PISA2012, c("gender", "age", "homework", "possessions"), c("PV1MATH", "PV1READ", "PV1SCIE"), types = list(continuous = "points", combo = "box", discrete = "ratio"))
# ggduo(australia_PISA2012, c("gender", "age", "homework", "possessions"), c("PV1MATH", "PV1READ", "PV1SCIE"), types = list(continuous = wrap("smooth", alpha = 0.25, method = "loess"), combo = "box", discrete = "ratio"), mapping = ggplot2::aes(color = gender))
ggduo <- function(
  data,
  mapping = NULL,
  columnsX = 1:ncol(data),
  columnsY = 1:ncol(data),
  title = NULL,
  types = list(
    continuous = "smooth_loess",
    comboVertical = "box_no_facet",
    comboHorizontal = "facethist",
    discrete = "ratio"
  ),
  axisLabels = c("show", "none"),
  columnLabelsX = colnames(data[columnsX]),
  columnLabelsY = colnames(data[columnsY]),
  labeller = "label_value",
  switch = NULL,
  xlab = NULL,
  ylab = NULL,
  showStrips = NULL,
  legend = NULL,
  cardinality_threshold = 15,
  legends = stop("deprecated")
) {

  warn_deprecated(!missing(legends), "legends")

  isSharedData <- inherits(data, "SharedData")
  data_ <- fix_data(data)
  data <- fix_data_slim(data_, isSharedData)

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

  stop_if_high_cardinality(data, columnsX, cardinality_threshold)
  stop_if_high_cardinality(data, columnsY, cardinality_threshold)

  types <- check_and_set_ggpairs_defaults(
    "types", types,
    continuous = "smooth_loess", discrete = "ratio", na = "na",
    isDuo = TRUE
  )

  if (!is.null(types[["combo"]])) {
    warning(str_c(
      "\nSetting:\n",
      "\ttypes$comboHorizontal <- types$combo\n",
      "\ttypes$comboVertical <- types$combo"
    ))
    types$comboHorizontal <- types$combo
    types$comboVertical <- types$combo
    types$combo <- NULL
  }
  if (is.null(types[["comboVertical"]])) {
    types$comboVertical <- "box_no_facet"
  }
  if (is.null(types[["comboHorizontal"]])) {
    types$comboHorizontal <- "facethist"
  }

  axisLabels <- fix_axis_label_choice(axisLabels, c("show", "none"))

  # get plot type information
  dataTypes <- plot_types(data, columnsX, columnsY, allowDiag = FALSE)

  ggduoPlots <- lapply(seq_len(nrow(dataTypes)), function(i) {

    plotType <- dataTypes[i, "plotType"]

    # posX <- dataTypes[i, "posX"]
    # posY <- dataTypes[i, "posY"]
    xColName <- dataTypes[i, "xVar"]
    yColName <- dataTypes[i, "yVar"]

    sectionAes <- add_and_overwrite_aes(
      add_and_overwrite_aes(
        aes_(x = as.name(xColName), y = as.name(yColName)),
        mapping
      ),
      types$mapping
    )

    if (plotType == "combo") {
      if (dataTypes[i, "isVertical"]) {
        plotTypesList <- list(combo = types$comboVertical)
      } else {
        plotTypesList <- list(combo = types$comboHorizontal)
      }
    } else {
      plotTypesList <- types
    }
    args <- list(types = plotTypesList, sectionAes = sectionAes)
    plot_fn <- ggmatrix_plot_list(plotType)

    plotObj <- do.call(plot_fn, args)
    return(plotObj)
  })


  plotMatrix <- ggmatrix(
    plots = ggduoPlots,
    byrow = TRUE,
    nrow = length(columnsY),
    ncol = length(columnsX),
    xAxisLabels = columnLabelsX,
    yAxisLabels = columnLabelsY,
    labeller = labeller,
    switch = switch,
    showStrips = showStrips,
    showXAxisPlotLabels = identical(axisLabels, "show"),
    showYAxisPlotLabels = identical(axisLabels, "show"),
    title = title,
    xlab = xlab,
    ylab = ylab,
    data = data_,
    gg = NULL,
    legend = legend
  )

  plotMatrix
}


### Example removed due to not using facet labels anymore
# #Sequence to show how to change label size
# make_small_strip <- function(plot_matrix, from_top, from_left, new_size = 7){
#   up <- from_left > from_top
#   p <- getPlot(plot_matrix, from_top, from_left)
#   if(up)
#     p <- p + opts(strip.text.x = element_text(size = new_size))
#   else
#     p <- p + opts(strip.text.y = element_text(angle = -90, size = new_size))
#
#   putPlot(plot_matrix, p, from_top, from_left)
# }
# small_label_diamond <- make_small_strip(diamondMatrix, 2, 1)
# small_label_diamond <- make_small_strip(small_label_diamond, 1, 2)
# small_label_diamond <- make_small_strip(small_label_diamond, 2, 2)
# #small_label_diamond # now with much smaller strip text

#' ggpairs - A ggplot2 generalized pairs plot
#'
#' Make a matrix of plots with a given data set
#'
#' @details
#' \code{upper} and \code{lower} are lists that may contain the variables
#' 'continuous', 'combo', 'discrete', and 'na'. Each element of the list may be a function or a string.  If a string is supplied, it must implement one of the following options:
#'\describe{
#'  \item{continuous}{exactly one of ('points', 'smooth', 'smooth_loess', 'density', 'cor', 'blank'). This option is used for continuous X and Y data.}
#'  \item{combo}{exactly one of ('box', 'box_no_facet', 'dot', 'dot_no_facet', 'facethist', 'facetdensity', 'denstrip', 'blank'). This option is used for either continuous X and categorical Y data or categorical X and continuous Y data.}
#'  \item{discrete}{exactly one of ('facetbar', 'ratio', 'blank'). This option is used for categorical X and Y data.}
#'  \item{na}{exactly one of ('na', 'blank').  This option is used when all X data is \code{NA}, all Y data is \code{NA}, or either all X or Y data is \code{NA}.}
#'}
#'
#' \code{diag} is a list that may only contain the variables 'continuous', 'discrete', and 'na'. Each element of the diag list is a string implementing the following options:
#'\describe{
#'  \item{continuous}{exactly one of ('densityDiag', 'barDiag', 'blankDiag'). This option is used for continuous X data.}
#'  \item{discrete}{exactly one of ('barDiag', 'blankDiag'). This option is used for categorical X and Y data.}
#'  \item{na}{exactly one of ('naDiag', 'blankDiag').  This option is used when all X data is \code{NA}.}
#'}
#'
#' If 'blank' is ever chosen as an option, then ggpairs will produce an empty plot.
#'
#' If a function is supplied as an option to \code{upper}, \code{lower}, or \code{diag}, it should implement the function api of \code{function(data, mapping, ...){#make ggplot2 plot}}.  If a specific function needs its parameters set, \code{\link{wrap}(fn, param1 = val1, param2 = val2)} the function with its parameters.
#'
#' @export
#' @seealso wrap v1_ggmatrix_theme
#' @param data data set using.  Can have both numerical and categorical data.
#' @param mapping aesthetic mapping (besides \code{x} and \code{y}).  See \code{\link[ggplot2]{aes}()}.  If \code{mapping} is numeric, \code{columns} will be set to the \code{mapping} value and \code{mapping} will be set to \code{NULL}.
#' @param columns which columns are used to make plots.  Defaults to all columns.
#' @param title,xlab,ylab title, x label, and y label for the graph
#' @param upper see Details
#' @param lower see Details
#' @param diag see Details
#' @param params deprecated.  Please see \code{\link{wrap_fn_with_param_arg}}
#' @param ... deprecated. Please use \code{mapping}
#' @param axisLabels either "show" to display axisLabels, "internal" for labels in the diagonal plots, or "none" for no axis labels
#' @param columnLabels label names to be displayed.  Defaults to names of columns being used.
#' @template ggmatrix-labeller-param
#' @template ggmatrix-switch-param
#' @param showStrips boolean to determine if each plot's strips should be displayed. \code{NULL} will default to the top and right side plots only. \code{TRUE} or \code{FALSE} will turn all strips on or off respectively.
#' @template ggmatrix-legend-param
#' @param cardinality_threshold maximum number of levels allowed in a character / factor column.  Set this value to NULL to not check factor columns. Defaults to 15
#' @param legends deprecated
#' @keywords hplot
#' @import ggplot2
#' @references John W Emerson, Walton A Green, Barret Schloerke, Jason Crowley, Dianne Cook, Heike Hofmann, Hadley Wickham. The Generalized Pairs Plot. Journal of Computational and Graphical Statistics, vol. 22, no. 1, pp. 79-91, 2012.
#' @author Barret Schloerke \email{schloerke@@gmail.com}, Jason Crowley \email{crowley.jason.s@@gmail.com}, Di Cook \email{dicook@@iastate.edu}, Heike Hofmann \email{hofmann@@iastate.edu}, Hadley Wickham \email{h.wickham@@gmail.com}
#' @return ggmatrix object that if called, will print
#' @examples
#'  # small function to display plots only if it's interactive
#'  p_ <- GGally::print_if_interactive
#'
#'
#' ## Quick example, with and without colour
#' data(flea)
#' ggpairs(flea, columns = 2:4)
#' pm <- ggpairs(flea, columns = 2:4, ggplot2::aes(colour=species))
#' p_(pm)
#' # Note: colour should be categorical, else you will need to reset
#' # the upper triangle to use points instead of trying to compute corr
#'
#' data(tips, package = "reshape")
#' pm <- ggpairs(tips[, 1:3])
#' p_(pm)
#' pm <- ggpairs(tips, 1:3, columnLabels = c("Total Bill", "Tip", "Sex"))
#' p_(pm)
#' pm <- ggpairs(tips, upper = "blank")
#' p_(pm)
#'
#' ## Plot Types
#' # Change default plot behavior
#' pm <- ggpairs(
#'   tips[, c(1, 3, 4, 2)],
#'   upper = list(continuous = "density", combo = "box_no_facet"),
#'   lower = list(continuous = "points", combo = "dot_no_facet")
#' )
#' p_(pm)
#' # Supply Raw Functions (may be user defined functions!)
#' pm <- ggpairs(
#'   tips[, c(1, 3, 4, 2)],
#'   upper = list(continuous = ggally_density, combo = ggally_box_no_facet),
#'   lower = list(continuous = ggally_points, combo = ggally_dot_no_facet)
#' )
#' p_(pm)
#'
#' # Use sample of the diamonds data
#' data(diamonds, package="ggplot2")
#' diamonds.samp <- diamonds[sample(1:dim(diamonds)[1], 1000), ]
#'
#' # Different aesthetics for different plot sections and plot types
#' pm <- ggpairs(
#'  diamonds.samp[, 1:5],
#'  mapping = ggplot2::aes(color = cut),
#'  upper = list(continuous = wrap("density", alpha = 0.5), combo = "box_no_facet"),
#'  lower = list(continuous = wrap("points", alpha = 0.3), combo = wrap("dot_no_facet", alpha = 0.4)),
#'  title = "Diamonds"
#' )
#' p_(pm)
#'
#' ## Axis Label Variations
#' # Only Variable Labels on the diagonal (no axis labels)
#' pm <- ggpairs(tips[, 1:3], axisLabels="internal")
#' p_(pm)
#' # Only Variable Labels on the outside (no axis labels)
#' pm <- ggpairs(tips[, 1:3], axisLabels="none")
#' p_(pm)
#'
#' ## Facet Label Variations
#' #  Default:
#' df_x <- rnorm(100)
#' df_y <- df_x + rnorm(100, 0, 0.1)
#' df <- data.frame(x = df_x, y = df_y, c = sqrt(df_x^2 + df_y^2))
#' pm <- ggpairs(
#'   df,
#'   columnLabels = c("alpha[foo]", "alpha[bar]", "sqrt(alpha[foo]^2 + alpha[bar]^2)")
#' )
#' p_(pm)
#' #  Parsed labels:
#' pm <- ggpairs(
#'   df,
#'   columnLabels = c("alpha[foo]", "alpha[bar]", "sqrt(alpha[foo]^2 + alpha[bar]^2)"),
#'   labeller = "label_parsed"
#' )
#' p_(pm)
#'
#' ## Plot Insertion Example
#' custom_car <- ggpairs(mtcars[, c("mpg", "wt", "cyl")], upper = "blank", title = "Custom Example")
#' # ggplot example taken from example(geom_text)
#'   plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x=wt, y=mpg, label=rownames(mtcars)))
#'   plot <- plot +
#'     ggplot2::geom_text(ggplot2::aes(colour=factor(cyl)), size = 3) +
#'     ggplot2::scale_colour_discrete(l=40)
#' custom_car[1, 2] <- plot
#' personal_plot <- ggally_text(
#'   "ggpairs allows you\nto put in your\nown plot.\nLike that one.\n <---"
#' )
#' custom_car[1, 3] <- personal_plot
#' p_(custom_car)
#'
#' ## Remove binwidth warning from ggplot2
#' # displays warning about picking a better binwidth
#' pm <- ggpairs(tips, 2:3)
#' p_(pm)
#' # no warning displayed
#' pm <- ggpairs(tips, 2:3, lower = list(combo = wrap("facethist", binwidth = 0.5)))
#' p_(pm)
#' # no warning displayed with user supplied function
#' pm <- ggpairs(tips, 2:3, lower = list(combo = wrap(ggally_facethist, binwidth = 0.5)))
#' p_(pm)
ggpairs <- function(
  data,
  mapping = NULL,
  columns = 1:ncol(data),
  title = NULL,
  upper = list(continuous = "cor", combo = "box_no_facet", discrete = "facetbar", na = "na"),
  lower = list(continuous = "points", combo = "facethist", discrete = "facetbar", na = "na"),
  diag = list(continuous = "densityDiag", discrete = "barDiag", na = "naDiag"),
  params = NULL,
  ...,
  xlab = NULL,
  ylab = NULL,
  axisLabels = c("show", "internal", "none"),
  columnLabels = colnames(data[columns]),
  labeller = "label_value",
  switch = NULL,
  showStrips = NULL,
  legend = NULL,
  cardinality_threshold = 15,
  legends = stop("deprecated")
){

  warn_deprecated(!missing(legends), "legends")
  warn_if_args_exist(list(...))
  stop_if_params_exist(params)

  isSharedData <- inherits(data, "SharedData")

  data_ <- fix_data(data)
  data <- fix_data_slim(data_, isSharedData)

  if (
    !missing(mapping) & !is.list(mapping) &
    missing(columns)
  ) {
      columns <- mapping
      mapping <- NULL
  }
  stop_if_bad_mapping(mapping)

  columns <- fix_column_values(data, columns, columnLabels, "columns", "columnLabels")

  stop_if_high_cardinality(data, columns, cardinality_threshold)

  upper <- check_and_set_ggpairs_defaults(
    "upper", upper,
    continuous = "cor", combo = "box_no_facet", discrete = "facetbar", na = "na"
  )
  lower <- check_and_set_ggpairs_defaults(
    "lower", lower,
    continuous = "points", combo = "facethist", discrete = "facetbar", na = "na"
  )
  diag <- check_and_set_ggpairs_defaults(
    "diag", diag,
    continuous = "densityDiag", discrete = "barDiag", na = "naDiag",
    isDiag = TRUE
  )

  axisLabels <- fix_axis_label_choice(axisLabels, c("show", "internal", "none"))

  # get plot type information
  dataTypes <- plot_types(data, columns, columns, allowDiag = TRUE)

  # make internal labels on the diag axis
  if (identical(axisLabels, "internal")) {
    dataTypes$plotType[dataTypes$posX == dataTypes$posY] <- "label"
  }

  ggpairsPlots <- lapply(seq_len(nrow(dataTypes)), function(i) {

    plotType <- dataTypes[i, "plotType"]

    posX <- dataTypes[i, "posX"]
    posY <- dataTypes[i, "posY"]
    xColName <- dataTypes[i, "xVar"]
    yColName <- dataTypes[i, "yVar"]

    if (posX > posY) {
      types <- upper
    } else if (posX < posY) {
      types <- lower
    } else {
      types <- diag
    }

    sectionAes <- add_and_overwrite_aes(
      add_and_overwrite_aes(
        aes_(x = as.name(xColName), y = as.name(yColName)),
        mapping
      ),
      types$mapping
    )

    args <- list(types = types, sectionAes = sectionAes)
    if (plotType == "label") {
      args$label <- columnLabels[posX]
    }

    plot_fn <- ggmatrix_plot_list(plotType)

    p <- do.call(plot_fn, args)

    return(p)
  })

  plotMatrix <- ggmatrix(
    plots = ggpairsPlots,
    byrow = TRUE,
    nrow = length(columns),
    ncol = length(columns),
    xAxisLabels = (if (axisLabels == "internal") NULL else columnLabels),
    yAxisLabels = (if (axisLabels == "internal") NULL else columnLabels),
    labeller = labeller,
    switch = switch,
    showStrips = showStrips,
    showXAxisPlotLabels = identical(axisLabels, "show"),
    showYAxisPlotLabels = identical(axisLabels, "show"),
    title = title,
    xlab = xlab,
    ylab = ylab,
    data = data_,
    gg = NULL,
    legend = legend
  )

  plotMatrix
}


#' Add new aes
#'
#' Add new aesthetics to a previous aes.
#'
#' @keywords internal
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @return aes_ output
#' @import ggplot2
#' @rdname add_and_overwrite_aes
#' @examples
#' data(diamonds, package="ggplot2")
#' diamonds.samp <- diamonds[sample(1:dim(diamonds)[1], 1000), ]
#' pm <- ggpairs(diamonds.samp, columns = 5:7,
#'   mapping = ggplot2::aes(color = color),
#'   upper = list(continuous = "cor", mapping = ggplot2::aes_string(color = "clarity")),
#'   lower = list(continuous = "cor", mapping = ggplot2::aes_string(color = "cut")),
#'   title = "Diamonds Sample"
#' )
#' str(pm)
#'
add_and_overwrite_aes <- function(current, new) {
  if (length(new) >= 1) {
    for (i in 1:length(new)) {
      current[names(new)[i]] <- new[i]
    }
  }

  for (curName in names(current)) {
    if (is.null(current[[curName]])) {
      current[[curName]] <- NULL
    }
  }

  current
}




#' Aesthetic Mapping Color Fill
#'
#' Replace the fill with the color and make color NULL.
#'
#' @param current the current aesthetics
#' @keywords internal
#' @export
mapping_color_to_fill <- function(current) {
  if (is.null(current)) {
    return(aes())
  }
  currentNames <- names(current)
  color <- c("color", "colour")

  if (any(color %in% currentNames) && "fill" %in% currentNames) {
    # do nothing
  } else if (any(color %in% currentNames)) {
    # fill <- current[["fill" %in% currentNames]]
    # col <- current[[color %in% currentNames]]
    # current <- add_and_overwrite_aes(current, aes_string(fill = col, color = NA))
    current$fill <- current$colour
    current$colour <- NULL
  }

  # if(!is.null(mapping$colour) && !is.null(mapping$fill)) {
  #   # do nothing
  # } else if(!is.null(mapping$colour)) {
  # }
  current
}


set_to_blank_list_if_blank <- function(
  val,
  combo = TRUE,
  blank = "blank",
  isDuo = FALSE
) {
  isBlank <- is.null(val)
  if (!isBlank) {
    isBlank <- (!is.list(val) && (val == blank || val == "blank"))
  }
  if (isBlank) {
    val <- list()
    val$continuous <- blank
    if (combo) {
      val$combo <- blank
    }
    if (isDuo) {
      val$comboVertical <- blank
      val$comboHorizontal <- blank
    }
    val$discrete <- blank
    val$na <- blank
  }

  val
}

check_and_set_ggpairs_defaults <- function(
  name, obj,
  continuous = NULL,
  combo = NULL,
  discrete = NULL,
  na = NULL,
  isDiag = FALSE,
  isDuo = FALSE
) {

  blankVal <- ifelse(isDiag, "blankDiag", "blank")

  obj <- set_to_blank_list_if_blank(
    obj,
    combo = ! isDiag & ! isDuo,
    blank = blankVal,
    isDuo = isDuo
  )

  if (!is.list(obj)) {
    stop("'", name, "' is not a list")
  }
  stop_if_params_exist(obj$params)

  if (is.null(obj$continuous) && (!is.null(continuous))) {
    obj$continuous <- continuous
  }
  if (is.null(obj$combo) && (!is.null(combo))) {
    obj$combo <- combo
  }
  if (is.null(obj$discrete) && (!is.null(discrete))) {
    obj$discrete <- discrete
  }
  if (is.null(obj$na) && (!is.null(na))) {
    obj$na <- na
  }

  if (! is.null(obj$aes_string)) {
    stop(
      "'aes_string' is a deprecated element for the section ", name, ".\n",
      "Please use 'mapping' instead. "
    )
  }

  if (isDiag) {
    for (key in c("continuous", "discrete", "na")) {
      val <- obj[[key]]
      if (is.character(val)) {
        if (! str_detect(val, "Diag$")) {
          newVal <- paste(val, "Diag", sep = "")
          warning(paste(
            "Changing diag$", key, " from '", val, "' to '", newVal, "'",
            sep = ""
          ))
          obj[[key]] <- newVal
        }
      }
    }
  }

  obj
}


get_subtype_name <- function(.subType) {
  fn <- wrapp(.subType)
  ret <- attr(fn, "name")
  if (ret == ".subType") {
    ret <- "custom_function"
  }
  ret
}


stop_if_params_exist <- function(params) {
  if (! is.null(params)) {
      stop(
        "'params' is a deprecated argument.  ",
        "Please 'wrap' the function to supply arguments. ",
        "help(\"wrap\", package = \"GGally\")"
      )
  }
}



#diamondMatrix <- ggpairs(
#  diamonds,
#  columns = 8:10,
#  upper = list(points = "scatterplot", aes_string = aes_string(color = "cut")),
#  lower = list(points = "scatterplot", aes_string = aes_string(color = "cut")),
#  diag = "blank",
##  color = "color",
#  title = "Diamonds"
#)
#if(TRUE)
#{
#
#d <- diamonds[runif(floor(nrow(diamonds)/10), 0, nrow(diamonds)), ]
#
#diamondMatrix <- ggpairs(
#  d,
#  columns = 8:10,
#  upper = list(continuous = "points", aes_string = aes_string(color = "clarity")),
#  lower = list(continuous = "points", aes_string = aes_string(color = "cut")),
#  diag = "blank",
##  color = "color",
#  title = "Diamonds"
#)
#
#
#m <- mtcars
##m$vs <- as.factor(m$vs)
##m$cyl <- as.factor(m$cyl)
##m$qsec <- as.factor(m$qsec)
#carsMatrix <- ggpairs(
#  mtcars,
#  columns = c(1, 3, 4),
#  upper = list(continuous = "points", aes_string = aes_string(shape = "cyl", size = 5)),
#  lower = list(continuous = "points", aes_string = aes_string(size = "cyl")),
#  diag = "blank",
#  color = "cyl",
#  title = "mtcars",
#)
#
#
# carsMatrix <- ggpairs(
#   mtcars,
#   columns = c(1, 3, 4),
#   upper = list(aes_string = aes_string(shape = "as.factor(cyl)", size = 5)),
#   lower = list(aes_string = aes_string(size = "as.factor(cyl)")),
#   diag = "blank",
#   color = "cyl",
#   title = "Custom Cars",
# )
#
#
#}
