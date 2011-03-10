#make sure it all works!!!!


# continuous
#	  points
#	  smooth
#	  density
#	  cor
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


#' ggpairs - A GGplot2 Matrix
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
#' @param data data set using.  Can have both numerical and categorical data.
#' @param columns which columns are used to make plots.  Defaults to all columns.
#' @param title title for the graph
#' @param upper see Details
#' @param lower see Details
#' @param diag see Details
#' @param params vector of parameters to be applied to geoms.  Each value must have a corresponding name, such as \code{c(binwidth = 0.1)}.
#' @param ... other parameters being supplied to geom's aes, such as color
#' @param verbose boolean to determine the printing of "Plot #1, Plot #2...."
#' @param removeTicks boolean to determine if the ticks/labels are removed from the border areas
#' @keywords hplot
#' @author Barret Schloerke \email{schloerke@@gmail.com}, Di Cook \email{dicook@@iastate.edu}, Heike Hofmann \email{hofmann@@iastate.edu}, Hadley Wickham \email{h.wickham@@gmail.com}
#' @return ggpair object that if called, will print
#' @examples
#' # plotting is reduced to the first couple of examples.  
#' # Feel free to print the ggpair objects created in the examples
#' 
#' #ggpairs(iris)
#' #ggpairs(iris, upper = "blank")
#' ggpairs(iris[,3:5])
#'
#' # Custom Example
#' ggpairs(
#' 	iris[,3:5], 
#' 	upper = list(continuous = "density", combo = "box"), 
#' 	lower = list(continuous = "points", combo = "dot"), 
#' 	diag = list(continuous = "bar", discrete = "bar")
#' )
#'
#'
#'
#' # Custom Example
#' diamondMatrix <- ggpairs(	
#'  diamonds[,1:3], 	
#'  upper = list(continuous = "density", combo = "box"), 	
#'  lower = list(continuous = "points", combo = "dot"), 	
#'  diag = list(continuous = "bar", discrete = "bar"), 
#'  color = "cut", 
#'  title = "Diamonds"
#' )
#' #diamondMatrix
#' 
#'
#' # Will plot four "Incorrect Plots"
#' bad_plots <- ggpairs(
#' 	iris[,3:4],
#' 	upper = list(continuous = "wrongType1", combo = "wrongType2"),
#' 	lower = list(continuous = "IDK1", combo = "IDK2", discrete = "mosaic"),
#' 	diag = list(continuous = "points", discrete = "box")
#' )
#' #bad_plots
#'
#'
#' # Custom Examples
#' custom_car <- ggpairs(mtcars[,c("mpg","wt","cyl")], upper = "blank", title = "Custom Example")
#' # ggplot example taken from example(geom_text)
#'   plot <- ggplot(mtcars, aes(x=wt, y=mpg, label=rownames(mtcars)))
#'   plot <- plot + geom_text(aes(colour=factor(cyl)), size = 3) + scale_colour_discrete(l=40)
#' custom_car <- putPlot(custom_car, plot, 1, 2)
#' custom_car <- putPlot(custom_car, ggally_text("ggpairs allows you\nto put in your\nown plot.\nLike that one.\n <---"), 1, 3)
#' #custom_car
#' 
#'
#' # Custom plot with different scale fill gradient
#' custom_fill <- ggpairs(iris[,5:4], upper = list(combo = "denstrip"))
#' custom_fill <- putPlot(
#'  custom_fill, 
#'  ggally_text("ggpairs allows you\nto retrieve a plot.\nWe will grab this one,\n-->\nwith the legend\nand axis labels!"),
#'  1, 1)
#' #custom_fill
#' plot <- getPlot(custom_fill, 1, 2)
#' #plot
#' plotNew <- plot + scale_fill_gradient(low = "grey80", high = "black")
#' #plotNew
#' custom_fill <- putPlot(custom_fill, plotNew, 1, 2)
#' #custom_fill
#'
#'
#' #Sequence to show how to change label size
#' make_small_strip <- function(plot_matrix, from_top, from_left, new_size = 7){
#'   up <- from_left > from_top
#'   p <- getPlot(plot_matrix, from_top, from_left)
#'   if(up)
#'     p <- p + opts(strip.text.x = theme_text(size = new_size))
#'   else 
#'     p <- p + opts(strip.text.y = theme_text(angle = -90, size = new_size))
#'
#'   putPlot(plot_matrix, p, from_top, from_left)
#' }
#' small_label_diamond <- make_small_strip(diamondMatrix, 2, 1)
#' small_label_diamond <- make_small_strip(small_label_diamond, 1, 2)
#' small_label_diamond <- make_small_strip(small_label_diamond, 2, 2)
#' #small_label_diamond # now with much smaller strip text
#' 
#' 
#' 
#' special_diamond <- ggpairs(
#'   diamonds,
#'   columns = 8:10, 
#'   upper = list(continuous = "points",aes_string = aes_string(color = "clarity")), 
#'   lower = list(continuous = "points",aes_string = aes_string(color = "cut")), 
#'   diag = "blank", 
#'   title = "Diamonds"
#' )
#' #special_diamond
#'
#'
#' 
#' ## prints
#' #   data =    mtcars
#' #   columns = c(1,3,4) # (mpg, disp, hp)
#' #   upper =   contains scatter plots with the shape defined by the cyl and size constant at 5
#' #   lower =   contains scatter plots with the size defined by the cyl
#' #   diag =    contains 'blank' plots
#' #   color =   defined by cyl and is applied to botht he upper and lower plots.  It would be applied to diag if it existed
#' #   title =   "Custom Cars"
#' #   verbose = FALSE makes the "Plot #1, Plot #2...." not print
#' carsMatrix <- ggpairs(
#'   mtcars,
#'   columns = c(1,3,4), 
#'   upper = list(continuous = "points",aes_string = aes_string(shape = "cyl", size = 5)), 
#'   lower = list(continuous = "points",aes_string = aes_string(size = "cyl")), 
#'   diag = "blank", 
#'   color = "cyl", 
#'   title = "Custom Cars",
#'   verbose = FALSE
#' )
#' #carsMatrix
#'
#'
#' ## Each list is custom to it's own area
#' #  params are the parameters applied to the geoms.  Parameters can not be considered aesthetics.
#' iris_with_params <- ggpairs(
#'   iris, 
#'   upper = list(continuous = "density", combo = "dot", aes_string = aes_string(color = "Species")), 
#'   lower = list(continuous = "smooth", combo = "denstrip", aes_string = aes_string(color = "Species", fill = "Species"), params = c(binwidth=0.1)), 
#'   diag = list(continuous = "bar", discrete = "bar", aes_string = aes_string(fill = "Species"), params = c(binwidth = 0.25)),
#'   title = "Complex Iris Data"
#'  )
#' #iris_with_params
ggpairs <- function(
  data, 
  columns = 1:ncol(data),
  title = "",
  upper = list(), 
  lower = list(), 
  diag = list(),
  params = NULL,
  ...,
  verbose = FALSE,
	removeTicks = FALSE
){
  require(ggplot2)
  printInfo <- FALSE

	verbose = verbose || printInfo
	
	
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
		upper$combo <- "facethist"
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
		lower$combo <- "box"
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
	numCol <- length(columns)
	if(printInfo)
    cat("data col: ", numCol,"\n")

	

	ggpairsPlots <- list()
	

	grid <- expand.grid(x = 1:ncol(data[columns]), y = 1:ncol(data[columns]))

	all <- do.call("rbind", lapply(1:nrow(grid), function(i) {
		xcol <- grid[i, "x"]
		ycol <- grid[i, "y"]
		data.frame(xvar = names(data[columns])[ycol], yvar = names(data[columns])[xcol])
	}))

  if(printInfo){cat("\n\n\nALL\n");print(all)}

	dataTypes <- plot_types(data[columns])
  if(printInfo){cat("\n\n\nDATA TYPES\n");print(dataTypes)}

	
	for(i in 1:nrow(dataTypes)){
		p <- "blank"
		type <- dataTypes[i,"Type"]

		posX <- as.numeric(dataTypes[i,"posx"])
		posY <- as.numeric(dataTypes[i,"posy"])
		xColName <- as.character(dataTypes[i,"xvar"])
		yColName <- as.character(dataTypes[i,"yvar"])
		

		up <- posX < posY
		
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
			
			combo_aes <- addAndOverwriteAes(aes_string(x = yColName, y = xColName, ...), section_aes)
			if(subType == "density") {
				combo_aes <- addAndOverwriteAes(combo_aes, aes_string(group = combo_aes$colour))
				combo_aes
			}
				
			combo_params <- addAndOverwriteAes(params, section_params)
				
				
				p <- make_ggpair_text(subType, combo_aes, combo_params, printInfo)
#			else if(subType == "smooth")
#				p <- ggally_smooth(data, combo_aes, params)
#			else if(subType == "density")
#  				p <- ggally_density(data, combo_aes, params )
#			else if(subType == "cor")
#				p <- ggally_cor(data, combo_aes, params)
#			else if(subType == "blank")
#				p <- ggally_blank()
		
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
			combo_aes <- addAndOverwriteAes(aes_string(x = yColName, y = xColName, ...), section_aes)
			if(subType != "dot")
				combo_aes <- mapping_color_fill(combo_aes)
			combo_params <- addAndOverwriteAes(params, section_params)

			p <- make_ggpair_text(subType, combo_aes, combo_params, printInfo)
#			if(subType == "box")
#				p <- ggally_box(data, combo_aes, params)
#			else if(subType == "dot")
#				p <- ggally_dot(data, combo_aes, params)
#			else if(subType == "facethist")
#				p <- ggally_facethist(data, combo_aes, params)
#			else if(subType == "facetdensity")
#				p <- ggally_facetdensity(data, combo_aes, params)
#			else if(subType == "denstrip")
#				p <- ggally_denstrip(data, combo_aes, params)
#			else if(subType == "blank")
#				p <- ggally_blank()
				
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
			
			combo_aes <- addAndOverwriteAes(aes_string(x = yColName, y = xColName, ...), section_aes)
			combo_params <- addAndOverwriteAes(params, section_params)
			
			if(subType == "ratio")
				p <- ggally_ratio(data[, c(yColName, xColName)])
			else if(subType == "facetbar"){
				if(!is.null(combo_aes$colour)){
					combo_aes <- addAndOverwriteAes(combo_aes, aes_string(fill = combo_aes$colour))
				}
				p <- make_ggpair_text(subType, combo_aes, combo_params, printInfo)
			}
			# else if(subType == "blank")
			# 	p <- "ggally_blank('blank')"
                  else p <- ggally_text("Incorrect\nPlot",size=6)

		} else if(type == "stat_bin-num"){
  		if(printInfo)cat("stat_bin-num\n")
		  
			subType <- diag$continuous
			
			combo_aes <- addAndOverwriteAes(aes_string(x = xColName, ...), diag$aes_string)
			if(subType != "density")
				combo_aes <- mapping_color_fill(combo_aes)

			combo_params <- addAndOverwriteAes(params, diag$params)
		
			if(subType != "blank")
        p <- make_ggpair_text(paste(subType, "Diag", sep = "", collapse = ""), combo_aes, combo_params,printInfo)
		  else
		    p <- "blank"
#			
#				p <- ggally_densityDiag(data, combo_aes, params)
#			else if(subType == "bar")
#				p <- ggally_barDiag(data, combo_aes, params)
#			else if(subType == "blank")
#				p <- ggally_blank()

		} else if(type == "stat_bin-cat"){
  		if(printInfo)cat("stat_bin-cat\n")
  		
			subType <- diag$discrete
			combo_aes <- addAndOverwriteAes(aes_string(x = xColName, ...), diag$aes_string)
			combo_aes <- mapping_color_fill(combo_aes)
			
			combo_params <- addAndOverwriteAes(params, diag$params)

		
			p <- make_ggpair_text(paste(subType, "Diag", sep = "", collapse = ""), combo_aes, combo_params, printInfo)
#			if(subType == "bar")
#				p <- ggally_barDiag(data, combo_aes, params)
#			#else if(subType == "ratio")
#			#	p <- ggally_ratio(dataSelect)
#			else if(subType == "blank")
#				p <- ggally_blank()
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
		removeTicks = removeTicks
  )
	
	attributes(plotMatrix)$class <- "ggpairs"
	
	plotMatrix
	
}

#' Generate GGally Function Text
#'
#' Generate GGally function text with data, mapping, and parameters
#'
#' @param func identifier string in function name
#' @param mapping mapping supplied to the function
#' @param params parameters applied to the geom in the function
#' @param printInfo boolean to determine whether or not the executed function should be printed
#' @keywords internal
make_ggpair_text <- function(func, mapping, params=NULL, printInfo = FALSE){
  
  func_text <- paste("ggally_", func, collapse = "", sep = "")
  test_for_function <- tryCatch(
    get(func_text, mode = "function"),
    error = function(e)
      "bad_function_name"
  )
  
  if(identical(test_for_function, "bad_function_name")) return( ggally_text("Incorrect\nPlot",size=6))

  
  text <- paste(func_text, "(ggally_data, aes(", paste(names(mapping), " = ", as.character(mapping), sep = "", collapse = ", "), ")", sep = "", collapse = "")
  
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
#' Evaluate and GGally function with data, mapping, and parameters
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
#' A wrapper function to set the viewport
#' 
#' @param x row position
#' @param y coloumn position
#' @keywords internal
#' @author Hadley Wickham \email{h.wickham@@gmail.com}
# '
vplayout <- function(x, y) {
	viewport(layout.pos.row = x, layout.pos.col = y) 
}


#' Put Plot
#' Function to place your own plot in the layout
#' 
#' @param plotMatrix ggally object to be altered
#' @param plotObj ggplot object to be placed
#' @param rowFromTop row from the top
#' @param columnFromLeft column from the left
#' @keywords hplot
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @examples
#' plotMatrix <- ggpairs(mtcars[,c("mpg","wt","cyl")], upper = "blank", title = "Custom Example")
#' # ggplot example taken from example(geom_text)
#'   plot <- ggplot(mtcars, aes(x=wt, y=mpg, label=rownames(mtcars)))
#'   plot <- plot + geom_text(aes(colour=factor(cyl)), size = 3) + scale_colour_discrete(l=40)
#' plotMatrix <- putPlot(plotMatrix, plot, 1, 2)
#' plotMatrix <- putPlot(plotMatrix, ggally_text("ggpairs allows you\nto put in your\nown plot.\nLike that one.\n <---"), 1, 3)
#' plotMatrix
putPlot <- function(plotMatrix, plotObj, rowFromTop, columnFromLeft){

	pos <- columnFromLeft + (length(plotMatrix$columns)) * (rowFromTop - 1)
	plotMatrix$plots[[pos]] <- plotObj
	
	if(plotMatrix$printInfo)
    cat("\n\nDone placing plot: ",pos,"\n")
    
	plotMatrix
}

#' getPlot
#' Retrieves the ggplot object at the desired location
#' 
#' @param plotMatrix ggpair object to select from
#' @param rowFromTop row from the top
#' @param columnFromLeft column from the left
#' @keywords hplot
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @examples
#' plotMatrix2 <- ggpairs(iris[,5:4], upper = list(combo = "denstrip"))
#' getPlot(plotMatrix2, 1, 2)
getPlot <- function(plotMatrix, rowFromTop, columnFromLeft){
  if(plotMatrix$printInfo)
    cat("rowFromTop: ",rowFromTop," columnFromLeft: ",columnFromLeft,"\n")
  
	pos <- columnFromLeft + (length(plotMatrix$columns)) * (rowFromTop - 1)
	
  if(plotMatrix$printInfo) cat("Plot List Spot: ",pos,"\n")
  
  plot_text <- plotMatrix$plots[[pos]]
  if (is.character(plot_text)) {
  	if (plot_text != "blank") {
    	p <- eval_ggpair(plot_text, plotMatrix$data)
      attributes( p)$class <- "ggplot"
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
#' Specialized method to print the ggapir object
#'
#' @param x ggpair object to be plotted
#' @param ... not used
#' @method print ggpairs
#' @keywords internal
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @examples
#'  ggpairs(iris[,3:5])
print.ggpairs <- function(x, ...){
  plotObj <- x
  
  v1 <- viewport(
#		x = unit(0.5, "npc") + unit(1,"lines"), 
#		y = unit(0.5, "npc") + unit(1,"lines"), 
		width=unit(1, "npc") - unit(3,"lines"), 
		height=unit(1, "npc") - unit(3, "lines")
	)
	numCol <- length(plotObj$columns)

	v2 <- viewport(
	     layout = grid.layout(
	             numCol, 
	             numCol, 
	             widths = rep(1,numCol), 
	             heights = rep(1,numCol) 
	   ))

	grid.newpage()
	
	if(plotObj$title != ""){
		pushViewport(viewport(height = unit(1,"npc") - unit(.4,"lines")))
		grid.text(plotObj$title,x = .5, y = 1, just = c(.5,1),gp=gpar(fontsize=20))
		popViewport()
	}

	# viewport for Left Names
	pushViewport(viewport(width=unit(1, "npc") - unit(2,"lines"), height=unit(1, "npc") - unit(3, "lines")))
	
	pushViewport(viewport(layout = grid.layout(numCol, numCol, widths = rep(1,numCol), heights = rep(1,numCol) )))

	# Left Side
	for(i in 1:numCol){
		grid.text(names(plotObj$data[,plotObj$columns])[i],0,0.5,rot=90,just=c("centre","centre"), vp = vplayout(as.numeric(i),1))
	}


	popViewport()# layout
	popViewport()# spacing

	# viewport for Bottom Names
	pushViewport(viewport(width=unit(1, "npc") - unit(3,"lines"), height=unit(1, "npc") - unit(2, "lines")))

	pushViewport(viewport(layout = grid.layout(numCol, numCol, widths = rep(1,numCol), heights = rep(1,numCol) )))


	# Bottom Side
	for(i in 1:numCol){
		grid.text(names(plotObj$data[,plotObj$columns])[i],0.5,0,just=c("centre","centre"), vp = vplayout(numCol, i))
	}

	popViewport() #layout
	popViewport() #spacing

##############################################################  
####################  End Viewports  #########################
##############################################################  

#####################  Plot Objects  #########################

	pushViewport(v1) # labels on outside
	pushViewport(v2) # layout of plots
  
  for(rowPos in 1:numCol){
    for(columnPos in 1:numCol){
      p <- getPlot(plotObj, rowPos, columnPos)
      if(!is_blank_plot(p)){
        
      	pos <- columnPos + (rowPos - 1) * numCol
      	type <- p$type
      	subType <- p$subType
      	if(plotObj$printInfo) {
          cat("Pos #", pos)
          if(!is.null(type)) cat(": type = ", type)
          if(!is.null(subType)) cat(": subType = ", subType)
          cat("\n")
      	}
          
        # hack because ggplot2 is annoying
        if(!is.null(subType)){
          if(subType == "facethist"){
            p <- p + scale_x_continuous(NULL) + scale_y_continuous(NULL)
          } else if(subType == "box" || subType == "dot"){
            p <- p + scale_x_continuous(NULL, labels="", breaks=1)
          } else if(subType == "ratio"){
            p <- p + 
          		scale_x_continuous(
          			NULL, 
          			limits=c(1,length(p$x_names) + 1), 
          			breaks=1:(length(p$x_names) + 1), 
          			labels=c(p$x_names,""), 
          			minor_breaks=FALSE
          		) + 
          		scale_y_continuous(
          			NULL, 
          			limits=c(1,length(p$y_names) + 1), 
          			breaks=1:(length(p$y_names) + 1), 
          			labels=c(p$y_names,""), 
          			minor_breaks=FALSE
          		)          		

          }
        }

				removeTicks <- identical(plotObj$removeTicks, TRUE)

  			if( columnPos != 1 || removeTicks){
  				p <- p + opts(axis.text.y = theme_blank(), axis.title.y = theme_blank() )
  			}

  			if( rowPos != numCol || removeTicks){
  				p <- p + opts(axis.text.x = theme_blank(), axis.title.x = theme_blank() )
  			}

				if(removeTicks) {
					p <- p + opts(
						strip.background = theme_blank(),
						strip.text.x = theme_blank(),
						strip.text.y = theme_blank(),
                                    axis.ticks = theme_blank()
					)
		  		
				}
    		
                # Adjusts for the blank space left by faceting, and manually
                # sets the limits for numeric axes to 1% of the variable's
                # range below the min and above the max.
                if (identical(p$type,"combo")) {
                  # Scale the numeric variable; the numeric variable is
                  # mapped to the y variable for dot and box plots, but to the
                  # x variable for the others
                  if (p$subType %in% c("dot","box")) {
                    ymin <- min(p$data[,as.character(p$mapping$y)])
                    ymax <- max(p$data[,as.character(p$mapping$y)])
                    p <- p + labs(x = NULL, y = NULL) +
                      scale_y_continuous(limits=c(ymin-.01*(ymax-ymin),ymax+.01*(ymax-ymin)))
                  } 
                  else {
                    xmin <- min(p$data[,as.character(p$mapping$x)])
                    xmax <- max(p$data[,as.character(p$mapping$x)])
                    p <- p + labs(x = NULL, y = NULL) +
                      scale_x_continuous(limits=c(xmin-.01*(xmax-xmin),xmax+.01*(xmax-xmin)))
                  }
                  # Adjust for blank space left by faceting
                  if (p$horizontal) {
                    p <- p + opts(plot.margin = unit(c(0,-0.5,0,0), "lines"),
                      legend.position = "none")
                  } 
                  else {
                    p <- p + opts(plot.margin = unit(c(-0.5,0,0,0), "lines"),
                      legend.position = "none")
                  }                 
                }
                # Need to scale both variables for continuous plots
                else if (identical(p$type,"continuous") && !identical(p$subType,"cor")) {
                  xmin <- min(p$data[,as.character(p$mapping$x)])
                  xmax <- max(p$data[,as.character(p$mapping$x)])
                  ymin <- min(p$data[,as.character(p$mapping$y)])
                  ymax <- max(p$data[,as.character(p$mapping$y)])
                  p <- p + labs(x = NULL, y = NULL) + opts(plot.margin = unit(rep(0, 
                    4), "lines"), legend.position = "none") +
                    scale_x_continuous(limits=c(xmin-.01*(xmax-xmin),xmax+.01*(xmax-xmin))) +
                    scale_y_continuous(limits=c(ymin-.01*(ymax-ymin),ymax+.01*(ymax-ymin)))
                }
                # Scale the variable for numeric diagonal plots
                else if (identical(p$type,"diag") && is.numeric(p$data[,as.character(p$mapping$x)])) {
                  xmin <- min(p$data[,as.character(p$mapping$x)])
                  xmax <- max(p$data[,as.character(p$mapping$x)])
                  p <- p + labs(x = NULL, y = NULL) + opts(plot.margin = unit(rep(0,
                    4), "lines"), legend.position = "none") +
                    scale_x_continuous(limits=c(xmin-.01*(xmax-xmin),xmax+.01*(xmax-xmin)))
                }
                else {
                  p <- p + labs(x = NULL, y = NULL) + opts(plot.margin = unit(rep(0, 
                    4), "lines"), legend.position = "none")
                }
        
        
  			grid.rect(
  			  gp=gpar(fill="white",lty = "blank"),
  			  vp = vplayout(rowPos, columnPos)
    	  )
    	  if(identical(plotObj$verbose, TRUE)) {
					print(p, vp = vplayout(rowPos, columnPos))
				} else {
					suppressMessages(suppressWarnings(print(p, vp = vplayout(rowPos, columnPos))))
				}
      	
      }# end plot alterations
    }# end cols
  }# end rows
  
	popViewport() #layout
	popViewport() #spacing
}

#' Is Blank Plot?
#' Find out if the plot equals a blank plot
#'
#' @keywords internal
#' @examples
#'  is_blank_plot(ggally_blank())
#'  is_blank_plot(ggally_points(mtcars, aes_string(x = "disp", y = "hp")))
#'
is_blank_plot <- function(p){  
  if( !is.null(p$subType) && !is.null(p$type))
    p$subType == "blank" && p$type == "blank"
  else
    FALSE
}


#' Add new aes
#' Add new aesthetics to a previous aes
#' 
#' @keywords internal
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @return aes_string output
#' @examples
#' diamondMatrix <- ggpairs(	
#'  diamonds,
#'  columns = 1:3, 	
#'  upper = list(points = "scatterplot", aes_string = aes_string(color = "clarity")), 	
#'  lower = list(points = "scatterplot", aes_string = aes_string(color = "cut")), 	
#'  diag = "blank", 
#'  color = "color", 
#'  title = "Diamonds"
#' )
#'   
addAndOverwriteAes <- function(current, new)
{
  if(length(new) >= 1)
    for(i in 1:length(new)){
      current[names(new)[i]] <- new[i]
    }
  
  current
}


#' Aesthetic Mapping Color Fill
#' Replace the fill with the color and make color NULL
#'
#' @param current the current aesthetics
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
	# 	# do nothing
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