# scatter
#	points
#	smooth
#	density
#	cor
#   blank

# conditional
#   box
#   dot plot
#   facethist
#   facetdensity
#   denstrip
#   blank

# mosaic
#   rata
#   blank


#' GGpairs - GGplot2 Matrix
#' Make a matrix of plots with a given data set
#' 
#' @param data data set using.  Can have both numerical and categorical data.
#' @param colour colour of the points
#' @param title title for the graph
#' @param upper see Details
#' @param lower see Details
#' @param identity see Details
#' @param ... other parameters being supplied to geoms, such as binwidth
#' @keyword hplot
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#' @examples
#' ggpairs(iris)
#' ggpairs(iris[,3:5])
#' ggpairs(
#' 	iris[,3:5], 
#' 	upper = list(scatter = "density", conditional = "box"), 
#' 	lower = list(scatter = "points", conditional = "dot"), 
#' 	iden = list(scatter = "bar", conditional = "bar")
#' )
#
#' ggpairs(
#' 	iris[,3:5],
#' 	upper = list(scatter = "density", conditional = "facetdensity"),
#' 	lower = "blank",
#' 	identity = list(scatter = "bar", conditional = "blank")
#' )
#' 	
#' ggpairs(
#' 	iris[,3:4],
#' 	upper = list(scatter = "wrongType", conditional = "wrongType"),
#' 	lower = list(scatter = "IDK", conditional = "5:1", mosaic = "mosaic"),
#' 	identity = list(scatter = "points", conditional = "box")
#' )
#'
#' ggpairs(mtcars[,c("mpg","wt","cyl")], upper = "blank")
#' plot <- ggplot(mtcars, aes(x=wt, y=mpg, label=rownames(mtcars))) + geom_text(aes(colour=factor(cyl)), size = 3) + scale_colour_discrete(l=40)
#' putPlot(1,2,plot)
#' putPlot(1,2,plot,axes = FALSE)
#' putPlot(1,3,ggplot_text("ggpairs allows you\nto put in your\nown plot.\nLike that one.\n <---"),axes = FALSE)
#' 
#' ggpairs(iris[,5:4], upper = list(conditional = "denstrip"))
#' putPlot(1,1,ggplot_text("ggpairs allows you\nto retrieve a plot.\nWe will grab this one,\n-->\nwith the legend!"),axes = FALSE)
#' getPlot(1,2) -> plot
#' plot
#' plotNew <- plot + scale_fill_gradient(low = "grey80", high = "black")
#' plotNew
#' ggpairs(iris[,5:4], upper = list(conditional = "denstrip"))
#' putPlot(1,2,plotNew,axes = FALSE)

ggpairs <- function (
	data, 
	colour = "black", 
	upper = list(scatter = "cor", conditional = "facethist", mosaic = "rata"), 
	lower = list(scatter = "points", conditional = "box", mosaic = "rata"), 
	identity = list(scatter = "density", conditional = "bar"),
	title = "",
	...) 
{
	
	makeTitle <- title != ""
	
	if(!is.list(upper) && upper == "blank")
	{
		upper <- list()
		upper$scatter = "blank"
		upper$conditional = "blank"
		upper$mosaic = "blank"
	}
	if(!is.list(lower) && lower == "blank")
	{
		lower <- list()
		lower$scatter = "blank"
		lower$conditional = "blank"
		lower$mosaic = "blank"
	}
	if(!is.list(identity) && identity == "blank")
	{
		identity <- list()
		identity$scatter = "blank"
		identity$conditional = "blank"
	}

	if(!is.list(upper))
		stop("upper is not a list")

	if (is.null(upper$scatter)) {
		upper$scatter <- "cor"
	}
	if (is.null(upper$conditional)) {
		upper$conditional <- "facethist"
	}
	if (is.null(upper$mosaic)) {
		upper$mosaic <- "rata"
	}

	if(!is.list(lower))
		stop("lower is not a list")

	if (is.null(lower$scatter)) {
		lower$scatter <- "cor"
	}
	if (is.null(lower$conditional)) {
		lower$conditional <- "facethist"
	}
	if (is.null(lower$mosaic)) {
		lower$mosaic <- "rata"
	}

	if (is.null(identity$scatter)) {
		identity$scatter <- "density"
	}
	if (is.null(identity$conditional)) {
		identity$conditional <- "bar"
	}


	data <- as.data.frame(data)
	numCol <- ncol(data)
	.ggpairsnumCol <<- numCol

	grid.newpage()
	if(makeTitle)
	{
		pushViewport(viewport(height = unit(1,"npc") - unit(.4,"lines")))
		grid.text(title,x = .5, y = 1, just = c(.5,1),gp=gpar(fontsize=20, fonttpye = "bold"))
		popViewport()
	}
	

	.v1 <<- viewport(
#		x = unit(0.5, "npc") + unit(1,"lines"), 
#		y = unit(0.5, "npc") + unit(1,"lines"), 
		width=unit(1, "npc") - unit(3,"lines"), 
		height=unit(1, "npc") - unit(3, "lines")
	)
	.v2 <<- viewport(layout = grid.layout(numCol, numCol, widths = rep(1,numCol), heights = rep(1,numCol) ))
	.ggpairsPlots <<- NULL
	

	grid <- expand.grid(x = 1:ncol(data), y = 1:ncol(data))

#cat("\nGRID\n");print(grid)
#print(dataTypes)


	all <- do.call("rbind", lapply(1:nrow(grid), function(i) {
		xcol <- grid[i, "x"]
		ycol <- grid[i, "y"]
		data.frame(xvar = names(data)[ycol], yvar = names(data)[xcol])
	}))

#cat("\n\n\nALL\n");print(all)

	dataTypes <- plot_types(data)
#cat("\n\n\nDATA TYPES\n");print(dataTypes)

	blank <- ggplot_text("Incorrect\nPlot",size=6)


	#		
	## viewport for Left Names
	pushViewport(viewport(width=unit(1, "npc") - unit(2,"lines"), height=unit(1, "npc") - unit(3, "lines")))
	
	pushViewport(viewport(layout = grid.layout(numCol, numCol, widths = rep(1,numCol), heights = rep(1,numCol) )))

	# Left Side
	for(i in 1:numCol)
	{
		grid.text(names(data)[i],0,0.5,rot=90,just=c("centre","centre"), vp = .vplayout(as.numeric(i),1))
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
		grid.text(names(data)[i],0.5,0,just=c("centre","centre"), vp = .vplayout(numCol, i))
	}

	popViewport() #layout
	popViewport() #spacing
	

	for(i in 1:nrow(dataTypes))
	{
		p <- blank
		type <- dataTypes[i,"Type"]

		posX <- as.numeric(dataTypes[i,"posx"])
		posY <- as.numeric(dataTypes[i,"posy"])

		up <- posX > posY
		
	#	print(up)

		dataSelect <- data[,c(posX,posY)]
		#colnames(dataSelect) <- c("X","Y")

#print(head(dataSelect))
		if(type == "scatterplot")
		{
			#cat("\nScatterplot")
			subType <- "points"
			if(up)
				subType <- upper$scatter
			else
				subType <- lower$scatter
				
			if(subType == "points")
				p <- ggplot_points(dataSelect, colour = colour, ...)
			else if(subType == "smooth")
				p <- ggplot_smooth(dataSelect, colour = colour, ...)
			else if(subType == "density")
				p <- ggplot_density(dataSelect)
			else if(subType == "cor")
			{
			###### Get size
				p <- ggplot_cor(dataSelect)
			}
			else if(subType == "blank")
				p <- ggplot_blank()

		
		}
		else if(type == "box-hori" || type == "box-vert")
		{
			#cat("\nbox-hori")
			subType <- "box"
			if(up)
				subType <- upper$conditional
			else
				subType <- lower$conditional

			if(subType == "box")
				p <- ggplot_box(dataSelect)
			else if(subType == "dot")
				p <- ggplot_dot(dataSelect)
			else if(subType == "facethist")
				p <- ggplot_facethist(dataSelect)
			else if(subType == "facetdensity")
				p <- ggplot_facetdensity(dataSelect)
			else if(subType == "denstrip")
				p <- ggplot_denstrip(dataSelect[,2:1],...)
			else if(subType == "blank")
				p <- ggplot_blank()
		}
		else if(type == "mosaic")
		{
			subType <- "rata"
			if(up)
				subType <- upper$mosaic
			else
				subType <- lower$mosaic

			if(subType == "rata")
				p <- ggplot_rata(dataSelect)
			else if(subType == "blank")
				p <- ggplot_blank()

		}
		else if(type == "stat_bin-num")
		{
			subType <- identity$scatter
		
			if(subType == "density")
				p <- ggplot_densityI(dataSelect)
			else if(subType == "bar")
				p <- ggplot_bar(dataSelect)
			else if(subType == "blank")
				p <- ggplot_blank()

		}
		else if(type == "stat_bin-cat")
		{
			subType <- identity$conditional
		
			if(subType == "bar")
				p <- ggplot_bar(dataSelect)
			#else if(subType == "rata")
			#	p <- ggplot_rata(dataSelect)
			else if(subType == "blank")
				p <- ggplot_blank()
		}
		
		print(posY)
		print(posX)
		#print(.ggpairsPlots)
		.ggpairsPlots <<- c( .ggpairsPlots, p)
		
		if(type == "box-hori")
		{
			if( posX != 1)
				p <- p + opts(axis.text.y = theme_blank() )
			if( posY != numCol)
				p <- p + opts(axis.text.x = theme_blank() )
		}
		else
		{
			if( posX != 1)
				p <- p + opts(axis.text.y = theme_blank() )
			if( posY != numCol)
				p <- p + opts(axis.text.x = theme_blank() )
		}
		
		p <- p + 
			labs(x = NULL, y = NULL) + 
			opts(
				plot.margin = unit(rep(0,4), "lines"),
				legend.position = "none"
			)

		
		putPlot(as.numeric(posY),as.numeric(posX),p)
		
		
	}
	
}

.vplayout <- function(x, y) 
	viewport(layout.pos.row = x, layout.pos.col = y) 



putPlot <- function(rowFromTop, columnFromLeft, p, axes = TRUE)
{
##	print(.vps$first)
#print(str(.v1))
#print(str(.v2))

	pos <- columnFromLeft + (.ggpairsnumCol) * (rowFromTop - 1)
	print(pos)
	pos <- (pos - 1) * 8 + 1

	.ggpairsPlots[pos:(pos + 7)] <<- p

	pushViewport(.v1) # labels on outside
	pushViewport(.v2) # layout of plots

	grid.rect(
		gp=gpar(fill="white",lty = "blank"),
		vp = .vplayout(rowFromTop, columnFromLeft)
	)
	
	if(!axes)
	{
		p <- p +
			labs(x=NULL,y=NULL) +
			opts(
				axis.text.x = theme_blank(), 
				axis.text.y = theme_blank(), 
				plot.margin = unit(rep(0,4), "lines"),
				legend.position = "none"
			)
	}
	
	
	print(p, vp = .vplayout(rowFromTop, columnFromLeft))
	
	popViewport()
	popViewport()
	
	
	#cat("\n\nDone")
}

getPlot <- function(x, y)
{
	pos <- y + (.ggpairsnumCol) * (x - 1)
	print(pos)
	pos <- (pos - 1) * 8 + 1
	print(pos)
	plot <- .ggpairsPlots[pos:(pos + 7)]
	attributes(plot)$class <- "ggplot"
	plot
}




.getTypes <- function(section)
{
	if(!is.list(section))
		stop("Either upper or lower is not a list")

	if (is.null(section$scatter)) {
		section$scatter <- "points"
	}
	if (is.null(section$conditional)) {
		section$conditional <- "box"
	}
	if (is.null(section$mosaic)) {
		section$mosaic <- "rata"
	}
	
	section
}


