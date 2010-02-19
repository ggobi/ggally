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
#   blank



#' ggpairs - A GGplot2 Matrix
#' Make a matrix of plots with a given data set
#' 
#' upper and lower are lists that may contain the variables 'continuous', 
#' 'combo' and 'discrete'. Each element of the list is a string implementing
#' the following options: continuous = exactly one of ('points', 'smooth', 
#' 'density', 'cor', 'blank'); combo = exactly one of ('box', 'dot', 
#' 'facethist', 'facetdensity', 'denstrip', 'blank'); discrete = exactly one 
#' of ('ratio', 'blank').
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
#' @param colour colour of the points
#' @param title title for the graph
#' @param upper see Details
#' @param lower see Details
#' @param diag see Details
#' @param ... other parameters being supplied to geom's aes, such as color
#' @keywords hplot
#' @author Barret Schloerke \email{bigbear@@iastate.edu}, Di Cook \email{dicook@@iastate.edu}, Heike Hofmann \email{hofmann@@iastate.edu}, Hadley Wickham \email{h.wickham@@gmail.com}
#' @return ggpair object that if called, will reprint
#' @examples
#' 
#' # plotting is reduced to the first couple of examples.  
#' # Feel free to print the ggpair objects created
#' 
#' #ggpairs(iris)
#' ggpairs(iris[,3:5])
#'
#' # Custom Example
#' ggpairs(
#' 	iris[,3:5], 
#' 	upper = list(continuous = "density", combo = "box"), 
#' 	lower = list(continuous = "points", combo = "dot"), 
#' 	diag = list(continuous = "bar", discrete = "bar")
#' )
#
#'
#' # Custom Example
#' #ggpairs(
#' #	iris[,3:5],
#' #	upper = list(continuous = "density", combo = "facetdensity"),
#' #	lower = "blank",
#' #	diag = list(continuous = "bar", discrete = "blank")
#' #)
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
#; #bad_plots
#'
#'
#' # Custom Examples
#' plotMatrix <- ggpairs(mtcars[,c("mpg","wt","cyl")], upper = "blank", title = "Custom Example")
#' # ggplot example taken from example(geom_text)
#'   plot <- ggplot(mtcars, aes(x=wt, y=mpg, label=rownames(mtcars)))
#'   plot <- plot + geom_text(aes(colour=factor(cyl)), size = 3) + scale_colour_discrete(l=40)
#' plotMatrix <- putPlot(plotMatrix, plot, 1, 2)
#' plotMatrix <- putPlot(plotMatrix, ggally_text("ggpairs allows you\nto put in your\nown plot.\nLike that one.\n <---"), 1, 3)
#' #plotMatrix
#' 
#'
#' # Custom plot with different scale fill gradient
#' plotMatrix2 <- ggpairs(iris[,5:4], upper = list(combo = "denstrip"))
#' plotMatrix2 <- putPlot(plotMatrix2, ggally_text("ggpairs allows you\nto retrieve a plot.\nWe will grab this one,\n-->\nwith the legend\nand axis labels!"), 1, 1)
#' #plotMatrix2
#' getPlot(plotMatrix2, 1, 2) -> plot
#' #plot
#' plotNew <- plot + scale_fill_gradient(low = "grey80", high = "black")
#' #plotNew
#' plotMatrix2 <- putPlot(plotMatrix2, plotNew, 1, 2)
#' #plotMatrix2
#'
#'
#' #Sequence to show how to change label size
#' plotWithBigLabels <- getPlot(diamondMatrix, 2, 1) 
#' plotWithBigLabels <- plotWithBigLabels + 
#'  opts(axis.text.y = 
#'    theme_text(
#'      angle = 90, 
#'      vjust = 0, 
#'      colour = "grey50",
#'      size = 4
#'    )
#'  )
#' diamondMatrix <- putPlot(
#'   diamondMatrix,
#'   plotWithBigLabels,
#'   2,
#'   1
#' )
#' plotWithBigLabels2 <- getPlot(diamondMatrix, 3, 2) 
#' plotWithBigLabels2 <- plotWithBigLabels2 + 
#'  opts(axis.text.x = 
#'    theme_text(
#'      vjust = 1, 
#'      hjust = 0, 
#'      colour = "grey50",
#'      size = 4
#'    )
#'  )
#' diamondMatrix <- putPlot(
#'   diamondMatrix,
#'   plotWithBigLabels2,
#'   3,
#'   2
#' )
#' #diamondMatrix # now with much smaller labels
ggpairs <- function (
	data, 
	colour = "black", 
	upper = list(continuous = "cor", combo = "facethist", discrete = "ratio"), 
	lower = list(continuous = "points", combo = "box", discrete = "ratio"), 
	diag = list(continuous = "density", discrete = "bar"),
	title = "",
	...) 
{
  library(ggplot2)
	
	
	if(!is.list(upper) && upper == "blank")
	{
		upper <- list()
		upper$continuous = "blank"
		upper$combo = "blank"
		upper$discrete = "blank"
	}
	if(!is.list(lower) && lower == "blank")
	{
		lower <- list()
		lower$continuous = "blank"
		lower$combo = "blank"
		lower$discrete = "blank"
	}
	if(!is.list(diag) && diag == "blank")
	{
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
		upper$discrete <- "ratio"
	}

	if(!is.list(lower))
		stop("lower is not a list")

	if (is.null(lower$continuous)) {
		lower$continuous <- "cor"
	}
	if (is.null(lower$combo)) {
		lower$combo <- "facethist"
	}
	if (is.null(lower$discrete)) {
		lower$discrete <- "ratio"
	}

	if (is.null(diag$continuous)) {
		diag$continuous <- "density"
	}
	if (is.null(diag$discrete)) {
		diag$discrete <- "bar"
	}


	data <- as.data.frame(data)
	numCol <- ncol(data)
	ggpairsnumCol <- numCol

	

	ggpairsPlots <- NULL
	subTypeList <- NULL
	typeList <- NULL
	posXList <- NULL
	posYList <- NULL
	

	grid <- expand.grid(x = 1:ncol(data), y = 1:ncol(data))


	all <- do.call("rbind", lapply(1:nrow(grid), function(i) {
		xcol <- grid[i, "x"]
		ycol <- grid[i, "y"]
		data.frame(xvar = names(data)[ycol], yvar = names(data)[xcol])
	}))

  #cat("\n\n\nALL\n");print(all)

	dataTypes <- .plot_types(data)
  #cat("\n\n\nDATA TYPES\n");print(dataTypes)


	blank <- ggally_text("Incorrect\nPlot",size=6)
	
	
	for(i in 1:nrow(dataTypes))
	{
		p <- blank
		type <- dataTypes[i,"Type"]

		posX <- as.numeric(dataTypes[i,"posx"])
		posY <- as.numeric(dataTypes[i,"posy"])
		xColName <- as.character(dataTypes[i,"xvar"])
		yColName <- as.character(dataTypes[i,"yvar"])
		

		up <- posX > posY
		#	print(up)


		if(type == "scatterplot")
		{
			#cat("\nScatterplot")
			subType <- "points"
			if(up)
				subType <- upper$continuous
			else
				subType <- lower$continuous
				
			if(subType == "points")
				p <- ggally_points(data, aes_string(x = xColName, y = yColName, ...))
			else if(subType == "smooth")
				p <- ggally_smooth(data, aes_string(x = xColName, y = yColName, ...))
			else if(subType == "density")
			 {
			     
#			  if(filledP)
  				p <- ggally_density(data, aes_string(x = xColName, y = yColName, ...) )
#				else
#  				p <- ggally_density(data, aes_string(x = xColName, y = yColName, ...)

			 }
			else if(subType == "cor")
			{
			###### Get size
				p <- ggally_cor(data, aes_string(x = xColName, y = yColName, ...))
			}
			else if(subType == "blank")
				p <- ggally_blank()

		
		}
		else if(type == "box-hori" || type == "box-vert")
		{
			#cat("\nbox-hori")
			subType <- "box"
			if(up)
				subType <- upper$combo
			else
				subType <- lower$combo

			if(subType == "box")
				p <- ggally_box(data, aes_string(x = xColName, y = yColName, ...))
			else if(subType == "dot")
				p <- ggally_dot(data, aes_string(x = xColName, y = yColName, ...))
			else if(subType == "facethist")
				p <- ggally_facethist(data, aes_string(x = xColName, y = yColName, ...))
			else if(subType == "facetdensity")
				p <- ggally_facetdensity(data, aes_string(x = xColName, y = yColName, ...))
			else if(subType == "denstrip")
				p <- ggally_denstrip(data, aes_string(x = xColName, y = yColName, ...))
			else if(subType == "blank")
				p <- ggally_blank()
		}
		else if(type == "mosaic")
		{
			subType <- "ratio"
			if(up)
				subType <- upper$discrete
			else
				subType <- lower$discrete

			if(subType == "ratio")
				p <- ggally_ratio(data[, c(xColName, yColName)])
			else if(subType == "blank")
				p <- ggally_blank()

		}
		else if(type == "stat_bin-num")
		{
			subType <- diag$continuous
		
			if(subType == "density")
				p <- ggally_densityDiag(data, aes_string(x = xColName, ...))
			else if(subType == "bar")
				p <- ggally_barDiag(data, aes_string(x = xColName, ...))
			else if(subType == "blank")
				p <- ggally_blank()

		}
		else if(type == "stat_bin-cat")
		{
			subType <- diag$discrete
		
			if(subType == "bar")
				p <- ggally_barDiag(data, aes_string(x = xColName,...))
			#else if(subType == "ratio")
			#	p <- ggally_ratio(dataSelect)
			else if(subType == "blank")
				p <- ggally_blank()
		}
		
		ggpairsPlots <- c( ggpairsPlots, p)
		subTypeList <- c(subTypeList, as.character(subType))
		typeList <- c(typeList, as.character(type))
		posYList <- c(posYList, posY)
		posXList <- c(posXList, posX)		
		
	}
	
	plotMatrix <- list(data = data, plots = ggpairsPlots, subType = subTypeList, type = typeList, positionY = posYList, positionX = posXList, title = title)
	
	attributes(plotMatrix)$class <- "ggpairs"
	
	plotMatrix
	
}


#' Viewport Layout Wrapper
#' A wrapper function to set the viewport
#' 
#' @param x row position
#' @param y coloumn position
#' @keywords internal
#' @author Hadley Wickham \email{h.wickham@@gmail.com}
.vplayout <- function(x, y) 
	viewport(layout.pos.row = x, layout.pos.col = y) 


#' Put Plot
#' Function to place your own plot in the layout
#' 
#' @param plotMatrix ggally object to be altered
#' @param plotObj ggplot object to be placed
#' @param rowFromTop row from the top
#' @param columnFromLeft column from the left
#' @keywords hplot
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#' @examples
#' example(ggpairs)
putPlot <- function(plotMatrix, plotObj, rowFromTop, columnFromLeft)
{

	pos <- columnFromLeft + (ncol(plotMatrix$data)) * (rowFromTop - 1)
#print(pos)
	pos <- (pos - 1) * 8 + 1

	plotMatrix$plots[pos:(pos + 7)] <- plotObj
	
	plotMatrix
	#cat("\n\nDone")
}

#' getPlot
#' Retrieves the ggplot object at the desired location
#' 
#' @param plotMatrix ggpair object to select from
#' @param rowFromTop row from the top
#' @param columnFromLeft column from the left
#' @keywords hplot
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#' @examples
#' example(ggpairs)
getPlot <- function(plotMatrix, rowFromTop, columnFromLeft)
{
  
	pos <- columnFromLeft + (ncol(plotMatrix$data)) * (rowFromTop - 1)
	
	shiftedPos <- (pos - 1) * 8 + 1
#	cat("Plot List Spot: ",pos,"\n")
	plot <- plotMatrix$plots[shiftedPos:(shiftedPos + 7)]
	attributes(plot)$class <- "ggplot"

	cat("Plot #",pos); if(.is_blank_plot(plot)) cat(" - Blank"); cat("\n")

	plot
}



#######################################################
#######################################################
#######################################################
#######################################################


#' Print ggpair object
#' Specialized method to print the ggapir object
#'
#' @param x ggpair object to be plotted
#' @param ... not used
#' @method print ggpairs
#' @keywords internal
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#' @examples
#' example(ggpairs)
print.ggpairs <- function(x, ...)
{
  plotObj <- x
  
  v1 <- viewport(
#		x = unit(0.5, "npc") + unit(1,"lines"), 
#		y = unit(0.5, "npc") + unit(1,"lines"), 
		width=unit(1, "npc") - unit(3,"lines"), 
		height=unit(1, "npc") - unit(3, "lines")
	)
	numCol <- ncol(plotObj$data)

	v2 <- viewport(
	     layout = grid.layout(
	             numCol, 
	             numCol, 
	             widths = rep(1,numCol), 
	             heights = rep(1,numCol) 
	   ))

	grid.newpage()
	
	if(plotObj$title != "")
	{
		pushViewport(viewport(height = unit(1,"npc") - unit(.4,"lines")))
		grid.text(plotObj$title,x = .5, y = 1, just = c(.5,1),gp=gpar(fontsize=20, fonttpye = "bold"))
		popViewport()
	}

	#		
	## viewport for Left Names
	pushViewport(viewport(width=unit(1, "npc") - unit(2,"lines"), height=unit(1, "npc") - unit(3, "lines")))
	
	pushViewport(viewport(layout = grid.layout(numCol, numCol, widths = rep(1,numCol), heights = rep(1,numCol) )))

	# Left Side
	for(i in 1:numCol)
	{
		grid.text(names(plotObj$data)[i],0,0.5,rot=90,just=c("centre","centre"), vp = .vplayout(as.numeric(i),1))
	}


	popViewport()# layout
	popViewport()# spacing

	#		
	## viewport for Bottom Names
	pushViewport(viewport(width=unit(1, "npc") - unit(3,"lines"), height=unit(1, "npc") - unit(2, "lines")))

	pushViewport(viewport(layout = grid.layout(numCol, numCol, widths = rep(1,numCol), heights = rep(1,numCol) )))


	# Bottom Side
	for(i in 1:numCol)
	{
		grid.text(names(plotObj$data)[i],0.5,0,just=c("centre","centre"), vp = .vplayout(numCol, i))
	}

	popViewport() #layout
	popViewport() #spacing

##############################################################  
####################  End Viewports  #########################
##############################################################  

#####################  Plot Objects  #########################

	pushViewport(v1) # labels on outside
	pushViewport(v2) # layout of plots
  
  for(rowPos in 1:numCol)
  {
    for(columnPos in 1:numCol)
    {      
      p <- getPlot(plotObj, rowPos, columnPos)
      
      isBlank <- .is_blank_plot(p)
      
    	pos <- columnPos + (rowPos - 1) * numCol

  
      if(plotObj$type[pos] == "box-hori")
  		{
  			if( columnPos != 1)
  				p <- p + opts(axis.text.y = theme_blank() )
  			if( rowPos != numCol)
  				p <- p + opts(axis.text.x = theme_blank() )
  		}
  		else
  		{
  			if( columnPos != 1)
  				p <- p + opts(axis.text.y = theme_blank() )
  			if( rowPos != numCol)
  				p <- p + opts(axis.text.x = theme_blank() )
  		}
  		
  		if(plotObj$type[pos] == "stat_bin-num")
  		{
  		  #cat("stat_bin-num - Plot = ", pos,"\n")
  		  p <- p + scale_y_continuous("")
  		}
  		
  		
  		p <- p + 
  			labs(x = NULL, y = NULL) + 
  			opts(
  				plot.margin = unit(rep(0,4), "lines"),
  				legend.position = "none"
  			)
  
  		
  			grid.rect(
  			  gp=gpar(fill="white",lty = "blank"),
  			  vp = .vplayout(rowPos, columnPos)
  	  )
  	  
  	  if(!isBlank)
      	print(p, vp = .vplayout(rowPos, columnPos))
    }
  }
	popViewport() #layout
	popViewport() #spacing

  
}

#' Is Blank Plot?
#' Find out if the plot equals a blank plot
#'
#' @keywords internal
#' @examples
#'  .is_blank_plot(ggally_blank())
#'  .is_blank_plot(ggally_points(mtcars, x = "disp", y = "hp"))
.is_blank_plot <- function(p)
{  
  identical(p, ggally_blank())
}


