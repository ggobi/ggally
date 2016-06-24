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
#   dot plot
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


fix_data <- function(data) {
  data <- as.data.frame(data)
  for (i in 1:dim(data)[2] ) {
    if (is.character(data[, i])) {
      data[, i] <- as.factor(data[, i])
    }
  }
  data
}


fix_column_values <- function(data, columns, columnLabels, columnsName, columnLabelsName) {
  colnamesData <- colnames(data)
  if (is.character(columns)) {
    colNumValues <- lapply(columns, function(colName){
      which(colnamesData == colName)
    })
    isFound <- as.logical(unlist(lapply(colNumValues, length)))
    if (any(!isFound)) {
      stop(str_c(
        "Columns in '", columnsName, "' not found in data: c(",
        str_c(str_c("'", columns[!isFound], "'"), collapse = ", "),
        ")"
      ))
    }
    columns <- unlist(colNumValues)
  }

  if (any(columns > ncol(data))) {
    stop(str_c(
      "Make sure your numeric '", columnsName, "'",
      " values are less than or equal to ", ncol(data), ".\n",
      "\t", columnsName, " = c(", str_c(columns, collapse = ", "), ")"
    ))
  }
  if (any(columns < 1)) {
    stop(str_c(
      "Make sure your numeric '", columnsName, "' values are positive.", "\n",
      "\t", columnsName, " = c(", paste(columns, collapse = ", "), ")"
    ))
  }
  if (any( (columns %% 1) != 0)) {
    stop(str_c(
      "Make sure your numeric '", columnsName, "' values are integers.", "\n",
      "\t", columnsName, " = c(", paste(columns, collapse = ", "), ")"
    ))
  }

  if (length(columnLabels) != length(columns)) {
    stop(
      "The length of the '", columnLabelsName, "'",
      " does not match the length of the '", columnsName, "' being used."
    )
  }

  colnamesUsed <- colnamesData[columns]
  nameIsOnlyNumber <- ! str_detect(colnamesUsed, "[^0-9]")
  if (any(nameIsOnlyNumber)) {
    badColumns <- colnamesUsed[nameIsOnlyNumber]
    names(badColumns) <- paste("column =", columns[nameIsOnlyNumber])
    warning(paste(
      "Data column name is numeric.  Desired behavior may not be as expected.\n\n",
      "c(", paste("'", names(badColumns), "' = '", badColumns, "'", collapse = "", sep = ""), ")",
      sep = ""
    ))
  }

  columns
}

warn_verbose_deprecated <- function(verboseIsSupplied) {
  if (verboseIsSupplied) {
    warning(
      "'verbose' will be deprecated in future versions.  Please remove it from your code"
    )
  }
}

stop_if_bad_mapping <- function(mapping) {
  if (is.numeric(mapping)) {
    stop(str_c(
      "'mapping' should not be numeric",
      " unless 'columns' is missing from function call."
    ))
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



#' ggduo - A ggplot2 generalized pairs plot for two columns sets of a data.frame
#'
#' Make a matrix of plots with a given data set with two different column sets
#'
#' @details
#' \code{types} is a list that may contain the variables
#' 'continuous', 'combo', 'discrete', and 'na'. Each element of the list may be a function or a string.  If a string is supplied, it must implement one of the following options:
#'\describe{
#'  \item{continuous}{exactly one of ('points', 'smooth', 'smooth_loess', 'density', 'cor', 'blank'). This option is used for continuous X and Y data.}
#'  \item{comboHorizontal}{exactly one of ('box', 'dot', 'facethist', 'facetdensity', 'denstrip', 'blank'). This option is used for either continuous X and categorical Y data or categorical X and continuous Y data.}
#'  \item{comboVertical}{exactly one of ('box', 'dot', 'facethist', 'facetdensity', 'denstrip', 'blank'). This option is used for either continuous X and categorical Y data or categorical X and continuous Y data.}
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
#' @param title title for the graph
#' @param types see Details
#' @param axisLabels either "show" to display axisLabels or "none" for no axis labels
#' @param columnLabelsX,columnLabelsY label names to be displayed.  Defaults to names of columns being used.
#' @param showStrips boolean to determine if each plot's strips should be displayed. \code{NULL} will default to the top and right side plots only. \code{TRUE} or \code{FALSE} will turn all strips on or off respectively.
#' @param legends boolean to determine the printing of the legend in each plot. Not recommended.
#' @export
#' @examples
#'  # small function to display plots only if it's interactive
#'  p_ <- function(pm) {
#'    if (interactive()) {
#'      print(pm)
#'    }
#'    invisible()
#'  }
#'
#'  data(baseball, package = "plyr")
#'
#'  # Add how many singles a player hit
#'  # (must do in two steps as X1b is used in calculations)
#'  dt <- transform(
#'    subset(baseball, year >= 1990 & year <= 1995),
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
#'
#'  # Make a fake column that will be calculated when printing
#'  dt$hit_type <- paste("hit_type:", seq_len(nrow(dt)))
#'
# Returns a function that will call the provided plotting function after the data has been #'  transformed
# The plotting function returned will count the number of singles, doubles, tripples, and #'  home runs a player hits given a particular grouping
#'  display_hit_type <- function(plot_fn, is_ratio) {
#'    function(data, mapping, ...) {
#'      # change the color aesthetic to fill aesthetic
#'      mapping <- mapping_color_to_fill(mapping)
#'
#'      # If the y varaible is not 'hit_type', continue like normal
#'      if (deparse(mapping$y) != "hit_type") {
#'        p <- plot_fn(data, mapping, ...)
#'        return(p)
#'      }
#'
#'      # Capture any extra column names needed
#'      extra_columns <- unname(unlist(lapply(
#'        mapping[! names(mapping) %in% c("x", "y")],
#'        deparse
#'      )))
#'      extra_columns <- extra_columns[extra_columns %in% colnames(data)]
#'
#'      x_name <- deparse(mapping$x)
#'
#'      # get the types of hits
#'      hit_types <- c("X1b", "X2b", "X3b", "hr")
#'      hit_names <- c("single", "double", "tripple", "home\nrun")
#'      if (is_ratio) {
#'        hit_types <- rev(hit_types)
#'        hit_names <- rev(hit_names)
#'      }
#'
#'      # retrieve the columns and rename them
#'      data <- data[, c(x_name, hit_types, extra_columns)]
#'      colnames(data) <- c(x_name, hit_names, extra_columns)
#'
#'      # melt the data to get the counts of the unique hit occurances
#'      dt_melt <- reshape::melt.data.frame(data, id = c(x_name, extra_columns))
#'      dt_value <- dt_melt$value
#'
#'      # Make a new data.frame with all the necessary variables repeated
#'      dt_ratio <- data.frame(variable = logical(sum(dt_value)))
#'      for (col in c(x_name, "variable", extra_columns)) {
#'        dt_ratio[[col]] <- rep(dt_melt[[col]], dt_value)
#'      }
#'
#'      # copy the old mapping and overwrite the x and y values
#'      mapping_ratio <- mapping
#'      mapping_ratio[c("x", "y")] <- ggplot2::aes_string(x = x_name, y = "variable")
#'
#'      # make ggplot2 object!
#'      plot_fn(dt_ratio, mapping_ratio, ...)
#'    }
#'  }
#'
#'
#'  display_hit_type_combo <- display_hit_type(ggally_facethist, FALSE)
#'  display_hit_type_discrete <- display_hit_type(ggally_ratio, TRUE)
#'
#'  # remove the strips, as the same information is displayed in the bottom axis area
#'  pm <- ggduo(
#'    dt,
#'    c("year", "g", "ab", "lg"),
#'    c("batting_avg", "slug", "on_base", "hit_type"),
#'    columnLabelsX = c("year", "player game count", "player at bat count", "league"),
#'    columnLabelsY = c("batting avg", "slug %", "on base %", "hit type"),
#'    title = "Baseball Hitting Stats from 1990-1995",
#'    mapping = ggplot2::aes(color = lg),
#'    types = list(
#'      # change the shape and add some transparency to the points
#'      continuous = wrap("smooth_loess", alpha = 0.50, shape = "+"),
#'      # all combinations that are continuous horizontally should have a binwidth of 15
#'      comboHorizontal = wrap(display_hit_type_combo, binwidth = 15),
#'      # the ratio plot should have a black border around the rects of size 0.15
#'      discrete = wrap(display_hit_type_discrete, color = "black", size = 0.15)
#'    ),
#'    showStrips = FALSE
#'  );
#'
#'  p_(pm)
#'
#'
#'
#' # Example derived from:
#' ## R Data Analysis Examples: Canonical Correlation Analysis.  UCLA: Statistical
#' ##   Consulting Group. from http://www.ats.ucla.edu/stat/r/dae/canonical.htm
#' ##   (accessed June 23, 2016).
#' # "Example 1. A researcher has collected data on three psychological variables, four
#' #  academic variables (standardized test scores) and gender for 600 college freshman.
#' #  She is interested in how the set of psychological variables relates to the academic
#' #  variables and gender. In particular, the researcher is interested in how many
#' #  dimensions (canonical variables) are necessary to understand the association between
#' #  the two sets of variables."
#' mm <- read.csv("http://www.ats.ucla.edu/stat/data/mmreg.csv")
#' colnames(mm) <- c("Control", "Concept", "Motivation", "Read", "Write", "Math",
#'     "Science", "Sex")
#' summary(mm)
#'
#' psych_variables <- c("Control", "Concept", "Motivation")
#' academic_variables <- c("Read", "Write", "Math", "Science", "Sex")
#'
#' ## Within correlation
#' p_(ggpairs(mm, columns = psych_variables))
#' p_(ggpairs(mm, columns = academic_variables))
#'
#' ## Between correlation
#' loess_with_cor <- function(data, mapping, ..., method = "pearson") {
#'   x <- data[[deparse(mapping$x)]]
#'   y <- data[[deparse(mapping$y)]]
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
#'       size = 5, fontface = "bold"
#'     )
#' }
#' pm <- ggduo(mm, psych_variables, academic_variables, types = list(continuous = loess_with_cor))
#' p_(pm)
#'
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
  title = "",
  types = list(
    continuous = "smooth_loess",
    comboVertical = "box",
    comboHorizontal = "facethist",
    discrete = "ratio"
  ),
  axisLabels = c("show", "none"),
  columnLabelsX = colnames(data[columnsX]),
  columnLabelsY = colnames(data[columnsY]),
  showStrips = NULL,
  legends = FALSE
) {

  data <- fix_data(data)

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
    types$comboVertical <- "box"
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
        aes_string(x = xColName, y = yColName),
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
    args <- list(plotType = plotType, types = plotTypesList, sectionAes = sectionAes)
    plot_fn <- ggmatrix_plot_list(plotType)

    plotObj <- do.call(plot_fn, args)
    return(plotObj)
  })


  plotMatrix <- ggmatrix(
    plots = ggduoPlots,
    byrow = TRUE,
    nrow = length(columnsY),
    ncol = length(columnsX),
    xAxisLabels = (if (axisLabels == "internal") NULL else columnLabelsX),
    yAxisLabels = (if (axisLabels == "internal") NULL else columnLabelsY),
    showStrips = showStrips,
    showXAxisPlotLabels = identical(axisLabels, "show"),
    showYAxisPlotLabels = identical(axisLabels, "show"),
    title = title,
    data = data,
    gg = NULL,
    legends = legends
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
#'  \item{combo}{exactly one of ('box', 'dot', 'facethist', 'facetdensity', 'denstrip', 'blank'). This option is used for either continuous X and categorical Y data or categorical X and continuous Y data.}
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
#' @seealso wrap
#' @param data data set using.  Can have both numerical and categorical data.
#' @param mapping aesthetic mapping (besides \code{x} and \code{y}).  See \code{\link[ggplot2]{aes}()}.  If \code{mapping} is numeric, \code{columns} will be set to the \code{mapping} value and \code{mapping} will be set to \code{NULL}.
#' @param columns which columns are used to make plots.  Defaults to all columns.
#' @param title title for the graph
#' @param upper see Details
#' @param lower see Details
#' @param diag see Details
#' @param params deprecated.  Please see \code{\link{wrap_fn_with_param_arg}}
#' @param ... deprecated. Please use \code{mapping}
#' @param axisLabels either "show" to display axisLabels, "internal" for labels in the diagonal plots, or "none" for no axis labels
#' @param columnLabels label names to be displayed.  Defaults to names of columns being used.
#' @param showStrips boolean to determine if each plot's strips should be displayed. \code{NULL} will default to the top and right side plots only. \code{TRUE} or \code{FALSE} will turn all strips on or off respectively.
#' @param legends boolean to determine the printing of the legend in each plot. Not recommended.
#' @param verbose deprecated
#' @keywords hplot
#' @import ggplot2
#' @references John W Emerson, Walton A Green, Barret Schloerke, Jason Crowley, Dianne Cook, Heike Hofmann, Hadley Wickham. The Generalized Pairs Plot. Journal of Computational and Graphical Statistics, vol. 22, no. 1, pp. 79-91, 2012.
#' @author Barret Schloerke \email{schloerke@@gmail.com}, Jason Crowley \email{crowley.jason.s@@gmail.com}, Di Cook \email{dicook@@iastate.edu}, Heike Hofmann \email{hofmann@@iastate.edu}, Hadley Wickham \email{h.wickham@@gmail.com}
#' @return ggpair object that if called, will print
#' @examples
#'  # small function to display plots only if it's interactive
#' p_ <- function(pm) {
#'   if (interactive()) {
#'     print(pm)
#'   }
#'   invisible()
#' }
#'
#' data(tips, package = "reshape")
#' pm <- ggpairs(tips[, 1:3])
#' p_(pm)
#' pm <- ggpairs(tips, 1:3, columnLabels = c("Total Bill", "Tip", "Sex"))
#' p_(pm)
#' pm <- ggpairs(tips, upper = "blank")
#' p_(pm)
#'
#'
#' # Custom Example
#' pm <- ggpairs(
#'   tips[, c(1, 3, 4, 2)],
#'   upper = list(continuous = "density", combo = "box"),
#'   lower = list(continuous = "points", combo = "dot")
#' )
#' p_(pm)
#'
#' # Use sample of the diamonds data
#' data(diamonds, package="ggplot2")
#' diamonds.samp <- diamonds[sample(1:dim(diamonds)[1], 200), ]
#'
#' # Custom Example
#' pm <- ggpairs(
#'  diamonds.samp[, 1:5],
#'  mapping = ggplot2::aes(color = cut),
#'  upper = list(continuous = wrap("density", alpha = 0.5), combo = "box"),
#'  lower = list(continuous = wrap("points", alpha = 0.3), combo = wrap("dot", alpha = 0.4)),
#'  title = "Diamonds"
#' )
#' p_(pm)
#'
#' # Only Variable Labels on the diagonal (no axis labels)
#' pm <- ggpairs(tips[, 1:3], axisLabels="internal")
#' p_(pm)
#' # Only Variable Labels on the outside (no axis labels)
#' pm <- ggpairs(tips[, 1:3], axisLabels="none")
#' p_(pm)
#'
#' # Custom Examples
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
ggpairs <- function(
  data,
  mapping = NULL,
  columns = 1:ncol(data),
  title = "",
  upper = list(continuous = "cor", combo = "box", discrete = "facetbar", na = "na"),
  lower = list(continuous = "points", combo = "facethist", discrete = "facetbar", na = "na"),
  diag = list(continuous = "densityDiag", discrete = "barDiag", na = "naDiag"),
  params = NULL,
  ...,
  axisLabels = c("show", "internal", "none"),
  columnLabels = colnames(data[columns]),
  showStrips = NULL,
  legends = FALSE,
  verbose = NULL
){

  data <- fix_data(data)

  stop_if_params_exist(params)

  if (is.numeric(mapping) & missing(columns)) {
      columns <- mapping
      mapping <- NULL
  }

  warn_verbose_deprecated(!missing(verbose))
  stop_if_bad_mapping(mapping)

  warn_if_args_exist(list(...))

  columns <- fix_column_values(data, columns, columnLabels, "columns", "columnLabels")

  upper <- check_and_set_ggpairs_defaults(
    "upper", upper,
    continuous = "cor", combo = "box", discrete = "facetbar", na = "na"
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
        aes_string(x = xColName, y = yColName),
        mapping
      ),
      types$mapping
    )

    args <- list(plotType = plotType, types = types, sectionAes = sectionAes)
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
    showStrips = showStrips,
    showXAxisPlotLabels = identical(axisLabels, "show"),
    showYAxisPlotLabels = identical(axisLabels, "show"),
    title = title,
    data = data,
    gg = NULL,
    legends = legends
  )

  plotMatrix
}


#' Add new aes
#'
#' Add new aesthetics to a previous aes.
#'
#' @keywords internal
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @return aes_string output
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
    stop(str_c("'", name, "' is not a list"))
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
    stop(str_c(
      "'aes_string' is a deprecated element for the section ", name, ".\n",
      "Please use 'mapping' instead. "
    ))
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
      stop(str_c(
        "'params' is a deprecated argument.  ",
        "Please 'wrap' the function to supply arguments. ",
        "help(\"wrap\", package = \"GGally\")"
      ))
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
