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
#' pm <- ggpairs(tips)
#' # pm
#' pm <- ggpairs(tips, upper = "blank")
#' # pm
#'
#'
#' # Custom Example
#' pm <- ggpairs(
#'   tips[,1:4],
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
#'  diamonds.samp[,1:3],
#'  upper = list(continuous = "density", combo = "box"),
#'  lower = list(continuous = "points", combo = "dot"),
#'  color = "cut",
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
#' custom_car <- putPlot(custom_car, plot, 1, 2)
#' personal_plot <- ggally_text(
#'   "ggpairs allows you\nto put in your\nown plot.\nLike that one.\n <---"
#' )
#' custom_car <- putPlot(custom_car, personal_plot, 1, 3)
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
  printInfo <- FALSE

  verbose = verbose || printInfo

  axisLabelChoices <- c("show", "internal", "none")
  axisLabelChoice <- pmatch(axisLabels, axisLabelChoices)
  if (is.na(axisLabelChoice)) {
    warning("axisLabels not in c('show', 'internal', 'none').  Reverting to 'show'")
    axisLabelChoice <- 1
  }
  axisLabels <- axisLabelChoices[axisLabelChoice]

  if (any(columns > ncol(data))) {
    stop(paste("Make sure your 'columns' values are less than ", ncol(data), ".\n\tcolumns = c(", paste(columns, collapse = ", "), ")", sep = ""))
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

  if(!is.list(upper) && upper == "blank"){
    upper <- list()
    upper$continuous = "blank"
    upper$combo = "blank"
    upper$discrete = "blank"
  }
  if(!is.list(lower) && lower == "blank"){
    lower <- list()
    lower$continuous = "blank"
    lower$combo = "blank"
    lower$discrete = "blank"
  }
  if(!is.list(diag) && diag == "blank"){
    diag <- list()
    diag$continuous = "blank"
    diag$discrete = "blank"
  }

  if(!is.list(upper))
    stop("upper is not a list")

  if (is.null(upper$continuous)) {
    upper$continuous <- "cor"
  }
  if (is.null(upper$combo)) {
    upper$combo <- "box"
  }
  if (is.null(upper$discrete)) {
    upper$discrete <- "facetbar"
  }

  if(!is.list(lower))
    stop("lower is not a list")

  if (is.null(lower$continuous)) {
    lower$continuous <- "points"
  }
  if (is.null(lower$combo)) {
    lower$combo <- "facethist"
  }
  if (is.null(lower$discrete)) {
    lower$discrete <- "facetbar"
  }

  if (is.null(diag$continuous)) {
    diag$continuous <- "density"
  }
  if (is.null(diag$discrete)) {
    diag$discrete <- "bar"
  }

  data <- as.data.frame(data)
  for ( i in 1:dim(data)[2] ) {
    if(is.character(data[,i])) {
      data[,i] <- as.factor(data[,i])
    }
  }

  numCol <- length(columns)
  if(printInfo)
    cat("data col: ", numCol,"\n")

  ggpairsPlots <- list()

  grid <- rev(expand.grid(y = 1:ncol(data[columns]), x = 1:ncol(data[columns])))

  all <- do.call("rbind", lapply(1:nrow(grid), function(i) {
    xcol <- grid[i, "x"]
    ycol <- grid[i, "y"]
    data.frame(xvar = names(data[columns])[ycol], yvar = names(data[columns])[xcol])
  }))

  if(printInfo){cat("\n\n\nALL\n");print(all)}

  dataTypes <- plot_types(data[columns])
  if(printInfo){cat("\n\n\nDATA TYPES\n");print(dataTypes)}

  if (identical(axisLabels,"internal")) {
    dataTypes$Type <- as.character(dataTypes$Type)
    dataTypes$Type[dataTypes$posx == dataTypes$posy] <- "label"
    dataTypes$Type <- as.factor(dataTypes$Type)
  }

  for(i in 1:nrow(dataTypes)){
    p <- "blank"
    type <- dataTypes[i,"Type"]

    posX <- as.numeric(as.character(dataTypes[i,"posx"]))
    posY <- as.numeric(as.character(dataTypes[i,"posy"]))
    xColName <- as.character(dataTypes[i,"xvar"])
    yColName <- as.character(dataTypes[i,"yvar"])


    up <- posX > posY

    if(printInfo) cat("Pos #", i, "\t(", posX, ",", posY, ")\t type: ")

    section_aes <- section_params <- NULL

    if(type == "scatterplot"){
      if(printInfo) cat("scatterplot\n")

      subType <- "points"
      if(up){
        subType <- upper$continuous
        section_aes <- upper$aes_string
        section_params <- upper$params
      } else {
        subType <- lower$continuous
        section_aes <- lower$aes_string
        section_params <- lower$params
      }

      combo_aes <- addAndOverwriteAes(aes_string(x = xColName, y = yColName, ...), section_aes)
      if(subType == "density") {
        combo_aes <- addAndOverwriteAes(combo_aes, aes_string(group = combo_aes$colour))
        combo_aes
      }

      combo_params <- addAndOverwriteAes(params, section_params)

      p <- make_ggpair_text(subType, combo_aes, combo_params, printInfo)
#      else if(subType == "smooth")
#        p <- ggally_smooth(data, combo_aes, params)
#      else if(subType == "density")
#          p <- ggally_density(data, combo_aes, params )
#      else if(subType == "cor")
#        p <- ggally_cor(data, combo_aes, params)
#      else if(subType == "blank")
#        p <- ggally_blank()

    } else if(type == "box-hori" || type == "box-vert"){
      if(printInfo)cat("box-hori-vert\n")

      subType <- "box"
      section_aes <- NULL
      if(up){
        subType <- upper$combo
        section_aes <- upper$aes_string
        section_params <- upper$params
      } else {
        subType <- lower$combo
        section_aes <- lower$aes_string
        section_params <- lower$params
      }
      combo_aes <- addAndOverwriteAes(aes_string(x = xColName, y = yColName, ...), section_aes)
      if(subType != "dot")
        combo_aes <- mapping_color_fill(combo_aes)
      combo_params <- addAndOverwriteAes(params, section_params)

      p <- make_ggpair_text(subType, combo_aes, combo_params, printInfo)
#      if(subType == "box")
#        p <- ggally_box(data, combo_aes, params)
#      else if(subType == "dot")
#        p <- ggally_dot(data, combo_aes, params)
#      else if(subType == "facethist")
#        p <- ggally_facethist(data, combo_aes, params)
#      else if(subType == "facetdensity")
#        p <- ggally_facetdensity(data, combo_aes, params)
#      else if(subType == "denstrip")
#        p <- ggally_denstrip(data, combo_aes, params)
#      else if(subType == "blank")
#        p <- ggally_blank()

    } else if(type == "mosaic"){
      if(printInfo)cat("mosaic\n")

      subType <- "facetbar"
      section_aes <- NULL
      if(up){
        subType <- upper$discrete
        section_aes <- upper$aes_string
        section_params <- upper$params
      } else {
        subType <- lower$discrete
        section_aes <- lower$aes_string
        section_params <- lower$params
      }

      combo_aes <- addAndOverwriteAes(aes_string(x = xColName, y = yColName, ...), section_aes)
      combo_params <- addAndOverwriteAes(params, section_params)

      if(subType == "ratio") {
        p <- ggally_ratio(data[, c(yColName, xColName)])
      } else if(subType == "facetbar") {
        if(!is.null(combo_aes$colour)) {
          combo_aes <- addAndOverwriteAes(combo_aes, aes_string(fill = combo_aes$colour))
        }
        p <- make_ggpair_text(subType, combo_aes, combo_params, printInfo)
      }
      else if(subType == "blank") {
        p <- "ggally_blank('blank')"
      } else {
        p <- ggally_text("Incorrect\nPlot",size=6)
      }

    } else if(type == "stat_bin-num") {
      if(printInfo)cat("stat_bin-num\n")

      subType <- diag$continuous

      combo_aes <- addAndOverwriteAes(aes_string(x = xColName, ...), diag$aes_string)
      if(subType != "density")
        combo_aes <- mapping_color_fill(combo_aes)

      combo_params <- addAndOverwriteAes(params, diag$params)

      if(subType != "blank") {
        p <- make_ggpair_text(paste(subType, "Diag", sep = "", collapse = ""), combo_aes, combo_params,printInfo)
      } else {
        p <- "blank"
      }
#
#        p <- ggally_densityDiag(data, combo_aes, params)
#      else if(subType == "bar")
#        p <- ggally_barDiag(data, combo_aes, params)
#      else if(subType == "blank")
#        p <- ggally_blank()

    } else if(type == "stat_bin-cat"){
      if(printInfo)cat("stat_bin-cat\n")

      subType <- diag$discrete
      combo_aes <- addAndOverwriteAes(aes_string(x = xColName, ...), diag$aes_string)
      combo_aes <- mapping_color_fill(combo_aes)

      combo_params <- addAndOverwriteAes(params, diag$params)

      p <- make_ggpair_text(paste(subType, "Diag", sep = "", collapse = ""), combo_aes, combo_params, printInfo)
#      if(subType == "bar")
#        p <- ggally_barDiag(data, combo_aes, params)
#      #else if(subType == "ratio")
#      #  p <- ggally_ratio(dataSelect)
#      else if(subType == "blank")
#        p <- ggally_blank()
    } else if(type == "label") {
      combo_aes <- addAndOverwriteAes(aes_string(x = xColName, ...), diag$aes_string)
      combo_params <- addAndOverwriteAes(params, diag$params)
      combo_params <- addAndOverwriteAes(combo_params, c("label" = columnLabels[posX]))

      p <- make_ggpair_text("diagAxis", combo_aes, combo_params, printInfo)
    }

    ggpairsPlots[[length(ggpairsPlots)+1]] <- p
  }

  plotMatrix <- list(
    data = data,
    columns = columns,
    plots = ggpairsPlots,
    title = title,
    verbose = verbose,
    printInfo = printInfo,
    axisLabels = axisLabels,
    columnLabels = columnLabels,
    legends = legends
  )

  attributes(plotMatrix)$class <- "ggpairs"

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

  nonSymbolLocs <- which(lapply(mapping, typeof) != "symbol")
  if (length(nonSymbolLocs) > 0) {
    nonSymbolNames <- names(mapping)[nonSymbolLocs]
    stop(paste("variables: ", paste(shQuote(nonSymbolNames), sep = ", "), " have non standard format.  Please rename the columns and use labels instead.", sep = ""))
  }

  func_text <- paste("ggally_", func, collapse = "", sep = "")
  test_for_function <- tryCatch(
    get(func_text, mode = "function"),
    error = function(e)
      "bad_function_name"
  )

  if(identical(test_for_function, "bad_function_name")) {
    return( ggally_text("Incorrect\nPlot",size=6))
  }


  text <- paste(func_text, "(ggally_data, ggplot2::aes(", paste(names(mapping), " = ", as.character(mapping), sep = "", collapse = ", "), ")", sep = "", collapse = "")

  if(!is.null(params)){
    params[is.character(params)] <- paste("\"", params[is.character(params)], "\"", sep = "")
    text <- paste(text, ", ", paste(names(params), "=", params, sep="", collapse=", "), sep="")
  }
  text <- paste(text, ")", sep = "", collapse = "")
  if(printInfo){
    print("")
    print(text)
    print(str(mapping))
  }
  text
}

#' Evaluate a GGally Function
#'
#' Evaluate and GGally function with data, mapping, and parameters.
#'
#' @param txt text that should be evaluated to create a plot
#' @param ggally_data data that should be used when evaluating the text
#' @keywords internal
eval_ggpair <- function(txt, ggally_data) {
  con <- textConnection(txt)
  on.exit(close(con))
  output <- eval(parse(con))
  output
}


#' Viewport Layout Wrapper
#'
#' A wrapper function to set the viewport.
#'
#' @param x row position
#' @param y coloumn position
#' @importFrom grid viewport
#' @keywords internal
#' @author Hadley Wickham \email{h.wickham@@gmail.com}
# '
vplayout <- function(x, y) {
  viewport(layout.pos.row = x, layout.pos.col = y)
}


#' Put Plot
#'
#' Function to place your own plot in the layout.
#'
#' @param plotMatrix ggally object to be altered
#' @param plotObj ggplot object to be placed
#' @param rowFromTop row from the top
#' @param columnFromLeft column from the left
#' @keywords hplot
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @export
#' @examples
#' custom_car <- ggpairs(mtcars[,c("mpg","wt","cyl")], upper = "blank", title = "Custom Example")
#' # ggplot example taken from example(geom_text)
#'   plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x=wt, y=mpg, label=rownames(mtcars)))
#'   plot <- plot +
#'     ggplot2::geom_text(ggplot2::aes(colour=factor(cyl)), size = 3) +
#'     ggplot2::scale_colour_discrete(l=40)
#' custom_car <- putPlot(custom_car, plot, 1, 2)
#' personal_plot <- ggally_text(
#'   "ggpairs allows you\nto put in your\nown plot.\nLike that one.\n <---"
#' )
#' custom_car <- putPlot(custom_car, personal_plot, 1, 3)
#' # custom_car
putPlot <- function(plotMatrix, plotObj, rowFromTop, columnFromLeft){

  pos <- columnFromLeft + (length(plotMatrix$columns)) * (rowFromTop - 1)
  plotMatrix$plots[[pos]] <- plotObj

  if(plotMatrix$printInfo)
    cat("\n\nDone placing plot: ",pos,"\n")

  plotMatrix
}

#' getPlot
#'
#' Retrieves the ggplot object at the desired location.
#'
#' @param plotMatrix ggpair object to select from
#' @param rowFromTop row from the top
#' @param columnFromLeft column from the left
#' @keywords hplot
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @export
#' @examples
#'  data(tips, package = "reshape")
#'  plotMatrix2 <- ggpairs(tips[,3:2], upper = list(combo = "denstrip"))
#'  getPlot(plotMatrix2, 1, 2)
getPlot <- function(plotMatrix, rowFromTop, columnFromLeft){
  if(plotMatrix$printInfo)
    cat("rowFromTop: ",rowFromTop," columnFromLeft: ",columnFromLeft,"\n")

  pos <- columnFromLeft + (length(plotMatrix$columns)) * (rowFromTop - 1)

  if(plotMatrix$printInfo) cat("Plot List Spot: ",pos,"\n")

  plot_text <- plotMatrix$plots[[pos]]
  if (is.character(plot_text)) {
    if (plot_text != "blank") {
      p <- eval_ggpair(plot_text, plotMatrix$data)
      # attributes( p)$class <- "ggplot"
    } else {
      p <- ggally_blank()
    }
  } else {
    p <- plot_text
  }

  if(plotMatrix$printInfo || plotMatrix$verbose){
    cat("Plot #",pos)
    if (is.character(plot_text) ) { if (plot_text == "blank") cat(" - Blank")}
    cat("\n")
  }

  p
}

#' Print ggpair object
#'
#' Specialized method to print the ggpair object-
#'
#' @param x ggpair object to be plotted
#' @param ... not used
#' @method print ggpairs
#' @keywords internal
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @importFrom grid gpar grid.layout grid.newpage grid.text grid.rect popViewport pushViewport unit viewport grid.draw
#' @export
#' @examples
#'  data(tips, package = "reshape")
#'  pMat <- ggpairs(tips, 1:3, color = "sex")
#'  pMat
#'  print(pMat, leftWidthProportion = 3, spacingProportion = 1, bottomHeightProportion = 1, showStrips = TRUE)
print.ggpairs <- function(
  x,
  leftWidthProportion = 0.2,
  bottomHeightProportion = 0.1,
  spacingProportion = 0.03,
  showStrips = FALSE,
  ...
) {

  plotObj <- x

  # If using internal axis labels, extend the plotting region out since
  # variable names on the margins will not be used
  if(identical(plotObj$axisLabels,"internal")) {
    v1 <- viewport(
      y = unit(0.5, "npc") - unit(0.5,"lines"),
      width=unit(1, "npc") - unit(1,"lines"),
      height=unit(1, "npc") - unit(2, "lines")
    )
  } else {
    v1 <- viewport(
      width=unit(1, "npc") - unit(3,"lines"),
      height=unit(1, "npc") - unit(3, "lines")
    )
  }

  numCol <- length(plotObj$columns)

  if (identical(plotObj$axisLabels,"show")) {
    showLabels <- TRUE
    viewPortWidths <- c(leftWidthProportion, 1, rep(c(spacingProportion,1), numCol - 1))
    viewPortHeights <- c(rep(c(1,spacingProportion), numCol - 1), 1, bottomHeightProportion)
  } else {
    showLabels <- FALSE
    viewPortWidths <- c(1, rep(c(spacingProportion,1), numCol - 1))
    viewPortHeights <- c(rep(c(1,spacingProportion), numCol - 1), 1)
  }
  viewPortCount <- length(viewPortWidths)

  v2 <- viewport(
    layout = grid.layout(
      viewPortCount,
      viewPortCount,
      ## added left and bottom spacers for axis labels
      widths = viewPortWidths,
      heights = viewPortHeights
  ))

  grid.newpage()

  if(plotObj$title != ""){
    pushViewport(viewport(height = unit(1,"npc") - unit(.4,"lines")))
    grid.text(plotObj$title,x = .5, y = 1, just = c(.5,1),gp=gpar(fontsize=15))
    popViewport()
  }

  # This plots the variable names on the margins, which is not needed if using internal
  # axis labels
  if(!identical(plotObj$axisLabels,"internal")) {
    # viewport for Left Names
    pushViewport(viewport(width=unit(1, "npc") - unit(2,"lines"), height=unit(1, "npc") - unit(3, "lines")))

    ## new for axis spacingProportion
    pushViewport(viewport(layout = grid.layout(
      viewPortCount, viewPortCount,
      widths = viewPortWidths, heights = viewPortHeights
    )))

    # Left Side
    for(i in 1:numCol){
      grid.text(plotObj$columnLabels[i],0,0.5,rot=90,just=c("centre","centre"), vp = vplayout(as.numeric(i) * 2 - 1 ,1))
    }

    popViewport()# layout
    popViewport()# spacing

    # viewport for Bottom Names
    pushViewport(viewport(width=unit(1, "npc") - unit(3,"lines"), height=unit(1, "npc") - unit(2, "lines")))

    ## new for axis spacing
    pushViewport(viewport(layout = grid.layout(
      viewPortCount, viewPortCount,
      widths = viewPortWidths, heights = viewPortHeights
    )))


    # Bottom Side
    for(i in 1:numCol){
      grid.text(
        plotObj$columnLabels[i],
        0.5,
        0,
        just = c("centre","centre"),
        vp = vplayout(
          ifelse(showLabels, 2*numCol, 2*numCol - 1),
          ifelse(showLabels, 2*i, 2*i - 1)
        )
      )
    }

    popViewport() #layout
    popViewport() #spacing
  }

##############################################################
####################  End Viewports  #########################
##############################################################

#####################  Plot Objects  #########################

  pushViewport(v1) # labels on outside
  pushViewport(v2) # layout of plots

  for(rowPos in 1:numCol){
    for(columnPos in 1:numCol){
      p <- getPlot(plotObj, rowPos, columnPos)
      pGtable <- ggplot_gtable(ggplot_build(p))

      ## New axis labels

      # left axis
      if (columnPos == 1 && showLabels) {
        if (identical(plotObj$verbose, TRUE)) {
          print("trying left axis")
        }
        pAxisLabels <- gtable_filter(pGtable, "axis-l")

        # make a viewport that is chopped into numFacets parts vertically
        grobLength <- length(pAxisLabels$grobs)
        vpLAxis <- viewport(
          layout = grid.layout(
            grobLength, 1,
            widths  = unit(1, "null"),
            heights = unit(rep(1, grobLength), rep("null", grobLength))
          )
        )

        pushViewport(vplayout(rowPos * 2 - 1, 1))
        pushViewport(vpLAxis)
          for (lAxisPos in 1:grobLength) {
            pushViewport(vplayout(lAxisPos, 1))
            grid.draw(pAxisLabels$grobs[[lAxisPos]])
            popViewport()
          }
        popViewport() # vpLAxis
        popViewport() # left Axis 'plot' area
      }

      ## bottom axis
      if (rowPos == numCol && showLabels) {
        if (identical(plotObj$verbose, TRUE)) {
          print("trying bottom axis")
        }
        pAxisLabels <- gtable_filter(pGtable, "axis-b")

        grobLength <- length(pAxisLabels$grobs)
        vpBAxis <- viewport(
          layout = grid.layout(
            grobLength, 1,
            widths  = unit(rep(1, grobLength), rep("null", grobLength)),
            heights = unit(1, "null")
          )
        )

        pushViewport(vplayout(numCol + numCol, 2 * columnPos))
        pushViewport(vpLAxis)
          for (bAxisPos in 1:grobLength) {
            pushViewport(vplayout(1, bAxisPos))
            grid.draw(pAxisLabels$grobs[[bAxisPos]])
            popViewport()
          }
        popViewport() # vpBAxis
        popViewport() # bottom Axis 'plot' area

      }

      ## get 'plot panel' grob to draw
      if (showStrips) {
        layoutRows <- (pGtable$layout$name %in% c("panel", "strip-top", "strip-right"))
      } else {
        layoutRows <- (pGtable$layout$name %in% c("panel"))
      }

      layoutInfo <- pGtable$layout[layoutRows, ]
      layoutTB <- layoutInfo[,c("t", "b")]
      layoutLR <- layoutInfo[,c("l", "r")]

      pPanel <- pGtable[
        min(layoutTB):max(layoutTB),
        min(layoutLR):max(layoutLR)
      ]

      ## Draw 'plot panel'
      pushViewport(vplayout(2 * rowPos - 1, ifelse(showLabels, 2 * columnPos, 2*columnPos - 1)))
        suppressMessages(suppressWarnings(
          grid.draw(pPanel)
        ))
      popViewport() # 'plot panel' area

    } # end cols
  } # end rows

  popViewport() #layout
  popViewport() #spacing
}



#' Is Blank Plot?
#' Find out if the plot equals a blank plot
#'
#' @keywords internal
#' @examples
#'  GGally:::is_blank_plot(ggally_blank())
#'  GGally:::is_blank_plot(ggally_points(mtcars, ggplot2::aes_string(x = "disp", y = "hp")))
#'
is_blank_plot <- function(p){
  if( !is.null(p$subType) && !is.null(p$type))
    p$subType == "blank" && p$type == "blank"
  else
    FALSE
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

  if(any(color %in% currentNames) && "fill" %in% currentNames) {
    # do nothing
  } else if(any(color %in% currentNames)) {
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
