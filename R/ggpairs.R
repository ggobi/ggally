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

#' ggpairs - A GGplot2 Matrix
#'
#' Make a matrix of plots with a given data set
#'
#' upper and lower are lists that may contain the variables 'continuous',
#' 'combo' and 'discrete'. Each element of the list is a string implementing
#' the following options: continuous = exactly one of ('points', 'smooth',
#' 'density', 'cor', 'blank'); combo = exactly one of ('box', 'dot',
#' 'facethist', 'facetdensity', 'denstrip', 'blank'); discrete = exactly one
#' of ('facetbar','ratio', 'blank').
#'
#' diag is a list that may only contain the variables 'continuous' and 'discrete'.
#' Each element of the diag list is a string implmenting the following options:
#' continuous = exactly one of ('density', 'bar', 'blank'); discrete = exactly one
#' of ('bar', 'blank').
#'
#' If a list option it will be set to the function default.  If 'blank' is ever
#' chosen as an option, then ggpairs will produce a blank plot, as if nothing was
#' printed there.
#'
#' @export
#' @param data data set using.  Can have both numerical and categorical data.
#' @param columns which columns are used to make plots.  Defaults to all columns.
#' @param title title for the graph
#' @param upper see Details
#' @param lower see Details
#' @param diag see Details
#' @param params vector of parameters to be applied to geoms.  Each value must have a corresponding name, such as \code{c(binwidth = 0.1)}.
#' @param ... other parameters being supplied to geom's aes, such as color
#' @param axisLabels either "show" to display axisLabels, "internal" for labels in the diagonal plots, or "none" for no axis labels
#' @param columnLabels label names to be displayed.  Defaults to names of columns being used.
#' @param legends boolean to determine the printing of the legend in each plot. Not recommended.
#' @param verbose boolean to determine the printing of "Plot #1, Plot #2...."
#' @keywords hplot
#' @import ggplot2
#' @author Barret Schloerke \email{schloerke@@gmail.com}, Jason Crowley \email{crowley.jason.s@@gmail.com}, Di Cook \email{dicook@@iastate.edu}, Heike Hofmann \email{hofmann@@iastate.edu}, Hadley Wickham \email{h.wickham@@gmail.com}
#' @return ggpair object that if called, will print
#' @examples
#' # plotting is reduced to the first couple of examples.
#' # Feel free to print the ggpair objects created in the examples
#'
#' data(tips, package = "reshape")
#' pm <- ggpairs(tips[,1:3])
#' # pm
#' pm <- ggpairs(tips, 1:3, columnLabels = c("Total Bill", "Tip", "Sex"))
#' # pm
#' pm <- ggpairs(tips, upper = "blank")
#' # pm
#'
#'
#' # Custom Example
#' pm <- ggpairs(
#'   tips[,c(1,3,4,2)],
#'   upper = list(continuous = "density", combo = "box"),
#'   lower = list(continuous = "points", combo = "dot")
#' )
#' # pm
#'
#' # Use sample of the diamonds data
#' data(diamonds, package="ggplot2")
#' diamonds.samp <- diamonds[sample(1:dim(diamonds)[1],200),]
#'
#' # Custom Example
#' pm <- ggpairs(
#'  diamonds.samp[,1:5],
#'  upper = list(continuous = "density", combo = "box"),
#'  lower = list(continuous = "points", combo = "dot"),
#'  color = "cut",
#'  alpha = 0.4,
#'  title = "Diamonds"
#' )
#' # pm
#'
#' # Will plot four "Incorrect Plots"
#' bad_plots <- ggpairs(
#'   tips[,1:3],
#'   upper = list(continuous = "wrongType1", combo = "wrongType2"),
#'   lower = list(continuous = "IDK1", combo = "IDK2", discrete = "mosaic"),
#' )
#' # bad_plots
#'
#' # Only Variable Labels on the diagonal (no axis labels)
#' pm <- ggpairs(tips[,1:3], axisLabels="internal")
#' # pm
#' # Only Variable Labels on the outside (no axis labels)
#' pm <- ggpairs(tips[,1:3], axisLabels="none")
#' # pm
#'
#' # Custom Examples
#' custom_car <- ggpairs(mtcars[,c("mpg","wt","cyl")], upper = "blank", title = "Custom Example")
#' # ggplot example taken from example(geom_text)
#'   plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x=wt, y=mpg, label=rownames(mtcars)))
#'   plot <- plot +
#'     ggplot2::geom_text(ggplot2::aes(colour=factor(cyl)), size = 3) +
#'     ggplot2::scale_colour_discrete(l=40)
#' custom_car[1,2] <- plot
#' personal_plot <- ggally_text(
#'   "ggpairs allows you\nto put in your\nown plot.\nLike that one.\n <---"
#' )
#' custom_car[1,3] <- personal_plot
#' # custom_car
ggpairs <- function(
  data,
  columns = 1:ncol(data),
  title = "",
  upper = list(),
  lower = list(),
  diag = list(),
  params = NULL,
  ...,
  axisLabels = "show",
  columnLabels = colnames(data[,columns]),
  legends = FALSE,
  verbose = FALSE
){
  args <- list(...)
  if ("printInfo" %in% names(args)) {
    printInfo <- args[['printInfo']]
  } else {
    printInfo <- FALSE
  }

  if (! identical(class(data),"data.frame")) {
    data <- as.data.frame(data)
  }

  verbose = verbose || printInfo

  axisLabelChoices <- c("show", "internal", "none")
  axisLabelChoice <- pmatch(axisLabels, axisLabelChoices)
  if (is.na(axisLabelChoice)) {
    warning("'axisLabels' not in c('show', 'internal', 'none').  Reverting to 'show'")
    axisLabelChoice <- 1
  }
  axisLabels <- axisLabelChoices[axisLabelChoice]

  if (any(columns > ncol(data))) {
    stop(paste("Make sure your 'columns' values are less than or equal to ", ncol(data), ".\n\tcolumns = c(", paste(columns, collapse = ", "), ")", sep = ""))
  }
  if (any(columns < 1)) {
    stop(paste("Make sure your 'columns' values are positive.", "\n\tcolumns = c(", paste(columns, collapse = ", "), ")", sep = ""))
  }
  if (any((columns %% 1) != 0)) {
    stop(paste("Make sure your 'columns' values are integers.", "\n\tcolumns = c(", paste(columns, collapse = ", "), ")", sep = ""))
  }

  if (length(columnLabels) != length(columns)) {
    stop("The length of the 'columnLabels' does not match the length of the 'columns' being used.")
  }

  nameIsOnlyNumber <- ! str_detect(colnames(data[, columns]), "[^0-9]")
  if (any(nameIsOnlyNumber)) {
    badColumns <- colnames(data[,columns])[nameIsOnlyNumber]
    names(badColumns) <- paste("column =", columns[nameIsOnlyNumber])
    warning(paste(
      "Column name is numeric.  Behavior will not be as expected.\n\n",
      "c(", paste("'", names(badColumns), "' = '", badColumns, "'", collapse = "", sep = ""), ")",
      sep = ""
    ))
  }


  if (!is.list(upper) && upper == "blank") {
    upper <- list()
    upper$continuous = "blank"
    upper$combo = "blank"
    upper$discrete = "blank"
  }
  if (!is.list(lower) && lower == "blank") {
    lower <- list()
    lower$continuous = "blank"
    lower$combo = "blank"
    lower$discrete = "blank"
  }
  if (!is.list(diag) && diag == "blank") {
    diag <- list()
    diag$continuous = "blank"
    diag$discrete = "blank"
  }

  if (!is.list(upper)) {
    stop("'upper' is not a list")
  }

  if (is.null(upper$continuous)) {
    upper$continuous <- "cor"
  }
  if (is.null(upper$combo)) {
    upper$combo <- "box"
  }
  if (is.null(upper$discrete)) {
    upper$discrete <- "facetbar"
  }

  if (!is.list(lower)) {
    stop("'lower' is not a list")
  }

  if (is.null(lower$continuous)) {
    lower$continuous <- "points"
  }
  if (is.null(lower$combo)) {
    lower$combo <- "facethist"
  }
  if (is.null(lower$discrete)) {
    lower$discrete <- "facetbar"
  }

  if (!is.list(diag)) {
    stop("'diag' is not a list")
  }
  if (is.null(diag$continuous)) {
    diag$continuous <- "density"
  }
  if (is.null(diag$discrete)) {
    diag$discrete <- "bar"
  }

  data <- as.data.frame(data)
  for (i in 1:dim(data)[2] ) {
    if (is.character(data[,i])) {
      data[,i] <- as.factor(data[,i])
    }
  }

  numCol <- length(columns)
  if (printInfo) {
    cat("data col: ", numCol,"\n")
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

  if (identical(axisLabels,"internal")) {
    dataTypes$Type <- as.character(dataTypes$Type)
    dataTypes$Type[dataTypes$posx == dataTypes$posy] <- "label"
    dataTypes$Type <- as.factor(dataTypes$Type)
  }

  for (i in 1:nrow(dataTypes)) {
    p <- "blank"
    type <- dataTypes[i,"Type"]

    posX <- as.numeric(as.character(dataTypes[i,"posx"]))
    posY <- as.numeric(as.character(dataTypes[i,"posy"]))
    xColName <- as.character(dataTypes[i,"xvar"])
    yColName <- as.character(dataTypes[i,"yvar"])


    up <- posX > posY

    if (printInfo) {
      cat("Pos #", i, "\t(", posX, ",", posY, ")\t type: ")
    }

    section_aes <- section_params <- NULL

    if (type %in% c("scatterplot", "box-hori", "box-vert")) {

      if (up) {
        subType <- ifelse(isContinuous, upper$continuous, upper$combo)
        section_aes <- upper$aes_string
        section_params <- upper$params
      } else {
        subType <- ifelse(isContinuous, lower$continuous, lower$combo)
        section_aes <- lower$aes_string
        section_params <- lower$params
      }

      if (printInfo) {
        cat("scatterplot\n")
      }
      # if (type == "scatterplot") {
      #   subType <- "points"
      # } else if (type == "box-hori" || type == "box-vert") {
      #   subType <- "box"
      # }

      combo_aes <- addAndOverwriteAes(aes_string(x = xColName, y = yColName, ...), section_aes)

      if (type == "scatterplot") {
        if (subType == "density") {
          combo_aes <- addAndOverwriteAes(combo_aes, aes_string(group = combo_aes$colour))

        }
      } else if(type == "box-hori" || type == "box-vert") {
        if (subType != "dot") {
          combo_aes <- mapping_color_fill(combo_aes)
        }
      }

      combo_params <- addAndOverwriteAes(params, section_params)

      p <- make_ggpair_text(subType, combo_aes, combo_params, printInfo)

    } else if (type == "mosaic") {
      if (printInfo) {
        cat("mosaic\n")
      }
      subType <- "facetbar"

      combo_aes <- addAndOverwriteAes(aes_string(x = xColName, y = yColName, ...), section_aes)
      combo_params <- addAndOverwriteAes(params, section_params)

      if (subType == "ratio") {
        p <- ggally_ratio(data[, c(yColName, xColName)])
      } else if (subType == "facetbar") {
        if (!is.null(combo_aes$colour)) {
          combo_aes <- addAndOverwriteAes(combo_aes, aes_string(fill = combo_aes$colour))
        }
        p <- make_ggpair_text(subType, combo_aes, combo_params, printInfo)
      }

    } else if (type %in% c("stat_bin-num", "stat_bin-cat", "label")) {

      if (type == "stat_bin-num" || type == "stat_bin-cat") {
        if (printInfo) {
          cat(paste(type, "\n", sep = ""))
        }

        if (type == "stat_bin-num") {
          subType <- diag$continuous
        } else if (type == "stat_bin-cat") {
          subType <- diag$discrete
        }

        combo_aes <- addAndOverwriteAes(aes_string(x = xColName, ...), diag$aes_string)

        if (
          (subType != "density" && type == "stat_bin-num") ||
          (type == "stat_bin-num")
        ) {
          combo_aes <- mapping_color_fill(combo_aes)
        }

        combo_params <- addAndOverwriteAes(params, diag$params)

        p <- make_ggpair_text(paste(subType, "Diag", sep = "", collapse = ""), combo_aes, combo_params,printInfo)

      } else if (type == "label") {
        combo_aes <- addAndOverwriteAes(aes_string(x = xColName, ...), diag$aes_string)
        combo_params <- addAndOverwriteAes(params, diag$params)
        combo_params <- addAndOverwriteAes(combo_params, c("label" = columnLabels[posX]))

        p <- make_ggpair_text("diagAxis", combo_aes, combo_params, printInfo)
      }
    }

    ggpairsPlots[[length(ggpairsPlots)+1]] <- p
  }

  plotMatrix <- ggmatrix(
    plots = ggpairsPlots,
    byrow = TRUE,
    nrow = length(columns),
    ncol = length(columns),
    axisLabels = axisLabels,
    xAxisLabels = columnLabels,
    yAxisLabels = columnLabels,
    title = title,
    verbose = verbose,
    data = data,
    gg = NULL,
    legends = legends
  )

  plotMatrix
}

#' Generate GGally Function Text
#'
#' Generate GGally function text with data, mapping, and parameters.
#'
#' @param func identifier string in function name
#' @param mapping mapping supplied to the function
#' @param params parameters applied to the geom in the function
#' @param printInfo boolean to determine whether or not the executed function should be printed
#' @keywords internal
make_ggpair_text <- function(func, mapping, params=NULL, printInfo = FALSE){

  nonCallVals <- which(lapply(mapping, mode) == "call")
  if (length(nonCallVals) > 0) {
    nonCallNames <- names(mapping)[nonCallVals]
    stop(paste("variables: ", paste(shQuote(nonCallNames), sep = ", "), " have non standard format: ", paste(shQuote(unlist(mapping[nonCallVals])), collapse = ", "), ".  Please rename the columns and use labels instead.", sep = ""))
  }

  if (func %in% c("blank", "blankDiag")) {
    return("blank")
  }

  func_text <- paste("ggally_", func, collapse = "", sep = "")
  test_for_function <- tryCatch(
    get(func_text, mode = "function"),
    error = function(e)
      "bad_function_name"
  )

  if (identical(test_for_function, "bad_function_name")) {
    return( 'ggally_text("Incorrect\nPlot",size=6)')
  }


  text <- paste(func_text, "(ggally_data, ggplot2::aes(", paste(names(mapping), " = ", as.character(mapping), sep = "", collapse = ", "), ")", sep = "", collapse = "")

  if (!is.null(params)) {
    params[is.character(params)] <- paste("\"", params[is.character(params)], "\"", sep = "")
    text <- paste(text, ", ", paste(names(params), "=", params, sep="", collapse=", "), sep="")
  }
  text <- paste(text, ")", sep = "", collapse = "")
  if (printInfo) {
    print("")
    print(text)
    print(str(mapping))
  }
  text
}






#' Add new aes
#'
#' Add new aesthetics to a previous aes.
#'
#' @keywords internal
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @return aes_string output
#' @import ggplot2
#' @examples
#'  data(diamonds, package="ggplot2")
#'  diamonds.samp <- diamonds[sample(1:dim(diamonds)[1],1000),]
#'  ggpairs(diamonds.samp, columns = 5:7,
#'   upper = list(continuous = "cor", aes_string = ggplot2::aes_string(color = "clarity")),
#'   lower = list(continuous = "cor", aes_string = ggplot2::aes_string(color = "cut")),
#'   color = "color",
#'   title = "Diamonds Sample")
#'
addAndOverwriteAes <- function(current, new) {
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
    # current <- addAndOverwriteAes(current, aes_string(fill = col, color = NA))
    current$fill <- current$colour
    current$colour <- NULL
  }

  # if(!is.null(mapping$colour) && !is.null(mapping$fill)) {
  #   # do nothing
  # } else if(!is.null(mapping$colour)) {
  # }
  current
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
#d <- diamonds[runif(floor(nrow(diamonds)/10),0,nrow(diamonds)),]
#
#diamondMatrix <- ggpairs(
#  d,
#  columns = 8:10,
#  upper = list(continuous = "points",aes_string = aes_string(color = "clarity")),
#  lower = list(continuous = "points",aes_string = aes_string(color = "cut")),
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
#  columns = c(1,3,4),
#  upper = list(continuous = "points",aes_string = aes_string(shape = "cyl", size = 5)),
#  lower = list(continuous = "points",aes_string = aes_string(size = "cyl")),
#  diag = "blank",
#  color = "cyl",
#  title = "mtcars",
#  verbose = FALSE
#)
#
#
# carsMatrix <- ggpairs(
#   mtcars,
#   columns = c(1,3,4),
#   upper = list(aes_string = aes_string(shape = "as.factor(cyl)", size = 5)),
#   lower = list(aes_string = aes_string(size = "as.factor(cyl)")),
#   diag = "blank",
#   color = "cyl",
#   title = "Custom Cars",
# )
#
#
#}
