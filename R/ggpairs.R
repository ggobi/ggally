# list of the different plot types to check
# continuous
#    points
#    smooth
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
#'  \item{continuous}{exactly one of ('points', 'smooth', 'density', 'cor', 'blank'). This option is used for continuous X and Y data.}
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
#' If a function is supplied to an upper, lower, or diag, it should implement the function api of \code{function(data, mapping, ...){#make ggplot2 plot}}.  If a specific function needs its parameters set, \code{\link{wrap}()} the function with its parameters.
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
#' @param ... other parameters being supplied to geom's aes, such as color
#' @param axisLabels either "show" to display axisLabels, "internal" for labels in the diagonal plots, or "none" for no axis labels
#' @param columnLabels label names to be displayed.  Defaults to names of columns being used.
#' @param showStrips boolean to determine if each plot's strips should be displayed. \code{NULL} will default to the top and right side plots only. \code{TRUE} or \code{FALSE} will turn all strips on or off respectively.
#' @param legends boolean to determine the printing of the legend in each plot. Not recommended.
#' @param verbose boolean to determine the printing of "Plot #1, Plot #2..."
#' @keywords hplot
#' @import ggplot2
#' @references John W Emerson, Walton A Green, Barret Schloerke, Jason Crowley, Dianne Cook, Heike Hofmann, Hadley Wickham. The Generalized Pairs Plot. Journal of Computational and Graphical Statistics, vol. 22, no. 1, pp. 79-91, 2012.
#' @author Barret Schloerke \email{schloerke@@gmail.com}, Jason Crowley \email{crowley.jason.s@@gmail.com}, Di Cook \email{dicook@@iastate.edu}, Heike Hofmann \email{hofmann@@iastate.edu}, Hadley Wickham \email{h.wickham@@gmail.com}
#' @return ggpair object that if called, will print
#' @examples
#' # plotting is reduced to the first couple of examples.
#' # Feel free to print the ggpair objects created in the examples
#'
#' data(tips, package = "reshape")
#' pm <- ggpairs(tips[, 1:3])
#' # pm
#' pm <- ggpairs(tips, 1:3, columnLabels = c("Total Bill", "Tip", "Sex"))
#' # pm
#' pm <- ggpairs(tips, upper = "blank")
#' # pm
#'
#'
#' # Custom Example
#' pm <- ggpairs(
#'   tips[, c(1, 3, 4, 2)],
#'   upper = list(continuous = "density", combo = "box"),
#'   lower = list(continuous = "points", combo = "dot")
#' )
#' # pm
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
#' # pm
#'
#' # Only Variable Labels on the diagonal (no axis labels)
#' pm <- ggpairs(tips[, 1:3], axisLabels="internal")
#' # pm
#' # Only Variable Labels on the outside (no axis labels)
#' pm <- ggpairs(tips[, 1:3], axisLabels="none")
#' # pm
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
#' # custom_car
ggpairs <- function(
  data,
  mapping = NULL,
  columns = 1:ncol(data),
  title = "",
  upper = list(),
  lower = list(),
  diag = list(),
  params = NULL,
  ...,
  axisLabels = "show",
  columnLabels = colnames(data[, columns]),
  showStrips = NULL,
  legends = FALSE,
  verbose = FALSE
){

  if (! is.null(params)) {
    display_param_error()
  }

  if (is.numeric(mapping) & missing(columns)) {
      columns <- mapping
      mapping <- NULL
  }

  if (is.numeric(mapping)) {
    stop(str_c(
      "'mapping' should not be numeric",
      " unless 'columns' is missing from function call."
    ))
  }

  args <- list(...)
  if ("printInfo" %in% names(args)) {
    printInfo <- args[["printInfo"]]
    args[["printInfo"]] <- NULL
  } else {
    printInfo <- FALSE
  }

  if (length(args) > 0) {
    argNames <- names(args)
    warning(str_c(
      "Extra arguments: ",
      str_c(shQuote(argNames), collapse = ", "), " are being ignored.",
      "  If these are meant to be aesthetics, submit them using the",
      " 'mapping' variable within ggpairs with ggplot2::aes or ggplot2::aes_string."
    ))
  }

  if (! identical(class(data), "data.frame")) {
    data <- as.data.frame(data)
  }

  verbose <- verbose || printInfo

  axisLabelChoices <- c("show", "internal", "none")
  axisLabelChoice <- pmatch(axisLabels, axisLabelChoices)
  if (is.na(axisLabelChoice)) {
    warning("'axisLabels' not in c('show', 'internal', 'none').  Reverting to 'show'")
    axisLabelChoice <- 1
  }
  axisLabels <- axisLabelChoices[axisLabelChoice]

  if (is.character(columns)) {
    columns <- unlist(lapply(columns, function(colName){
      which(colnames(data) == colName)
    }))
  }

  if (any(columns > ncol(data))) {
    stop(str_c(
      "Make sure your 'columns' values are less than or equal to ", ncol(data), ".\n",
      "\tcolumns = c(", str_c(columns, collapse = ", "), ")"
    ))
  }
  if (any(columns < 1)) {
    stop(str_c(
      "Make sure your 'columns' values are positive.", "\n",
      "\tcolumns = c(", paste(columns, collapse = ", "), ")"
    ))
  }
  if (any( (columns %% 1) != 0)) {
    stop(str_c(
      "Make sure your 'columns' values are integers.", "\n",
      "\tcolumns = c(", paste(columns, collapse = ", "), ")"
    ))
  }

  if (length(columnLabels) != length(columns)) {
    stop("The length of the 'columnLabels' does not match the length of the 'columns' being used.")
  }

  nameIsOnlyNumber <- ! str_detect(colnames(data[, columns]), "[^0-9]")
  if (any(nameIsOnlyNumber)) {
    badColumns <- colnames(data[, columns])[nameIsOnlyNumber]
    names(badColumns) <- paste("column =", columns[nameIsOnlyNumber])
    warning(paste(
      "Column name is numeric.  Behavior will not be as expected.\n\n",
      "c(", paste("'", names(badColumns), "' = '", badColumns, "'", collapse = "", sep = ""), ")",
      sep = ""
    ))
  }

  upper <- set_to_blank_list_if_blank(upper)
  lower <- set_to_blank_list_if_blank(lower)
  diag  <- set_to_blank_list_if_blank(diag, combo = FALSE, blank = "blankDiag")

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

  data <- as.data.frame(data)
  for (i in 1:dim(data)[2] ) {
    if (is.character(data[, i])) {
      data[, i] <- as.factor(data[, i])
    }
  }

  numCol <- length(columns)
  if (printInfo) {
    cat("data col: ", numCol, "\n")
  }

  ggpairsPlots <- list()

  grid <- rev(expand.grid(y = 1:ncol(data[columns]), x = 1:ncol(data[columns])))

  all <- do.call("rbind", lapply(1:nrow(grid), function(i) {
    xcol <- grid[i, "x"]
    ycol <- grid[i, "y"]
    data.frame(xvar = names(data[columns])[ycol], yvar = names(data[columns])[xcol])
  }))

  if (printInfo) {
    cat("\n\n\nALL\n");print(all)
  }

  dataTypes <- plot_types(data[columns])
  if (printInfo) {
    cat("\n\n\nDATA TYPES\n");print(dataTypes)
  }

  if (identical(axisLabels, "internal")) {
    dataTypes$Type <- as.character(dataTypes$Type)
    dataTypes$Type[dataTypes$posx == dataTypes$posy] <- "label"
    dataTypes$Type <- as.factor(dataTypes$Type)
  }


  for (i in 1:nrow(dataTypes)) {
    p <- "blank"
    type <- dataTypes[i, "Type"]

    posX <- as.numeric(as.character(dataTypes[i, "posx"]))
    posY <- as.numeric(as.character(dataTypes[i, "posy"]))
    xColName <- as.character(dataTypes[i, "xvar"])
    yColName <- as.character(dataTypes[i, "yvar"])

    plotAes <- add_and_overwrite_aes(aes_string(x = xColName, y = yColName), mapping)

    up <- posX > posY

    if (printInfo) {
      cat("Pos #", i, "\t(", posX, ", ", posY, ")\t type: ")
    }

    sectionAes <- NULL

    if (up) {
      sectionAes <- upper$mapping
    } else {
      sectionAes <- lower$mapping
    }

    if (type %in% c("NA", "NA-diag")) {

      subType <- if (type == "NA-diag") {
        diag$na
      } else {
        # type is "NA"
        if (up) {
          upper$na
        } else {
          lower$na
        }
      }
      p <- make_ggmatrix_plot_obj(
        wrap_fn_with_param_arg(subType, params = c()),
        mapping = aes()
      )

    } else if (type %in% c("scatterplot", "box-hori", "box-vert")) {
      isContinuous <- (type == "scatterplot")
      if (printInfo) {
        if (isContinuous) {
          cat("continuous\n")
        } else {
          cat("combo\n")
        }
      }

      if (up) {
        subType <- if (isContinuous) upper$continuous else upper$combo
      } else {
        subType <-  if (isContinuous) lower$continuous else lower$combo
      }

      # comboAes <- add_and_overwrite_aes(aes_string(x = xColName, y = yColName), sectionAes)
      comboAes <- add_and_overwrite_aes(plotAes, sectionAes)

      subTypeName <- get_subtype_name(subType)
      if (isContinuous) {
        if (identical(subTypeName, "density")) {
          comboAes <- add_and_overwrite_aes(comboAes, aes_string(group = comboAes$colour))
        }
      } else {
        # isCombo

        # ! subType %in% c("dot", "facetdensity")
        # subType %in% c("box", "facethist", denstrip)

        if (! (identical(subTypeName, "dot") || identical(subTypeName, "facetdensity"))) {
          comboAes <- mapping_color_fill(comboAes)
        }

      }

      p <- make_ggmatrix_plot_obj(
        wrap_fn_with_param_arg(subType, params = c()),
        mapping = comboAes
      )

    } else if (type == "mosaic") {
      if (printInfo) {
        cat("mosaic\n")
      }
      subType <- if (up) upper$discrete else lower$discrete
      subTypeName <- get_subtype_name(subType)

      comboAes <- add_and_overwrite_aes(plotAes, sectionAes)

      if (identical(subTypeName, "ratio")) {
        p <- ggally_ratio(data[, c(yColName, xColName)])

      } else {

        if (identical(subTypeName, "facetbar")) {
          if (!is.null(comboAes$colour)) {
            comboAes <- add_and_overwrite_aes(comboAes, aes_string(fill = comboAes$colour))
          }
        }
        p <- make_ggmatrix_plot_obj(
          wrap_fn_with_param_arg(subType, params = c()),
          mapping = comboAes
        )

      }

    } else if (type %in% c("stat_bin-num", "stat_bin-cat", "label")) {
      plotAes$y <- NULL

      if (type == "stat_bin-num" || type == "stat_bin-cat") {
        if (printInfo) {
          cat(paste(type, "\n", sep = ""))
        }

        if (type == "stat_bin-num") {
          subType <- diag$continuous
        } else if (type == "stat_bin-cat") {
          subType <- diag$discrete
        }
        subTypeName <- get_subtype_name(subType)

        comboAes <- add_and_overwrite_aes(plotAes, diag$mapping)

        if (
          ( (!identical(subTypeName, "density")) && type == "stat_bin-num") ||
          (type == "stat_bin-cat")
        ) {
          comboAes <- mapping_color_fill(comboAes)
        }

        fn_to_wrap <- subType

        p <- make_ggmatrix_plot_obj(
          wrap_fn_with_param_arg(fn_to_wrap, params = c()),
          mapping = comboAes
        )

      } else if (type == "label") {
        comboAes <- add_and_overwrite_aes(plotAes, diag$mapping)

        p <- make_ggmatrix_plot_obj(
          wrap_fn_with_param_arg("diagAxis", params = c("label" = columnLabels[posX])),
          mapping = comboAes
        )

      }
    }

    ggpairsPlots[[length(ggpairsPlots) + 1]] <- p
  }

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
    verbose = verbose,
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
mapping_color_fill <- function(current) {
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


set_to_blank_list_if_blank <- function(val, combo = TRUE, blank = "blank") {
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
  isDiag = FALSE
) {
  if (!is.list(obj)) {
    stop(str_c("'", name, "' is not a list"))
  }

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

  if (! is.null(obj$params)) {
    display_param_error()
  }

  if (! is.null(obj$aes_string)) {
    stop(str_c(
      "'aes_string' is a deprecated element for the section ", name, ".",
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
            "changing diag$", key, " from '", val, "' to '", newVal, "'",
            sep = ""
          ))
          obj[[key]] <- newVal
        }
      }
    }
  }

  obj
}


get_subtype_name <- function(subType) {
  if (inherits(subType, "ggmatrix_fn_with_params")) {
    name <- attr(subType, "fnName")
    if (str_detect(name, "^ggally_")) {
      str_replace(name, "^ggally_", "")
    } else {
      name
    }
  } else {
    if (mode(subType) == "character") {
      subType
    } else {
      "custom_function"
    }
  }
}


display_param_error <- function() {
  stop(str_c(
    "'params' is a deprecated argument.  ",
    "Please 'wrap' the function to supply arguments. ",
    "help(\"wrap\", package = \"GGally\")"
  ))
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
#  verbose = FALSE
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
