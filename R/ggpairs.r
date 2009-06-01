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
#   fluc
#   blank

ggpairs <- function (
	data, 
	colour = "black", 
	upper = list(scatter = "cor", conditional = "denstrip", mosaic = "fluc"), 
	lower = list(scatter = "smooth", conditional = "denstrip", mosaic = "fluc"), 
	identity = list(scatter = "density", conditional = "fluc"),
	title = "",
	...) 
{
	
	makeTitle <- title != ""
	
	
	upper <- .getTypes(upper)
	lower <- .getTypes(lower)
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

	blank <- ggplot_Text(.5,.5,"Blank\nPlot",size=6)


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
				p <- ggscatter_points(dataSelect,...)
			else if(subType == "smooth")
				p <- ggscatter_smooth(dataSelect,...)
			else if(subType == "density")
				p <- ggscatter_density(dataSelect)
			else if(subType == "cor")
			{
			###### Get size
				p <- ggscatter_cor(dataSelect)
			}
			else if(subType == "blank")
				p <- gg_blank_plot()

		
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
				p <- ggbox_reg(dataSelect)
			else if(subType == "dot")
				p <- ggbox_dot(dataSelect)
			else if(subType == "facethist")
				p <- ggbox_facethist(dataSelect)
			else if(subType == "facetdensity")
				p <- ggbox_facetdensity(dataSelect)
			else if(subType == "denstrip")
				p <- ggbox_denstrip(dataSelect[,2:1],...)
			else if(subType == "blank")
				p <- gg_blank_plot()
		}
		else if(type == "mosaic")
		{
			subType <- "fluc"
			if(up)
				subType <- upper$mosaic
			else
				subType <- lower$mosaic

			if(subType == "fluc")
				p <- ggmosaic_fluc(dataSelect)
			else if(subType == "blank")
				p <- gg_blank_plot()

		}
		else if(type == "stat_bin-num")
		{
			if(identity$scatter == "density")
				p <- ggiden_density(dataSelect)
			else if(identity$scatter == "bar")
				p <- ggiden_bar(dataSelect)
			else if(subType == "blank")
				p <- gg_blank_plot()

		}
		else if(type == "stat_bin-cat")
		{
			if(identity$conditional == "bar")
				p <- ggiden_bar(dataSelect)
			else if(identity$conditional == "fluc")
				p <- ggiden_fluc(dataSelect)
			else if(subType == "blank")
				p <- gg_blank_plot()
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
				plot.margin = unit(rep(0,4), "lines")
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
	if (is.null(section$scatter)) {
		section$scatter <- "points"
	}
	if (is.null(section$conditional)) {
		section$conditional <- "box"
	}
	if (is.null(section$mosaic)) {
		section$mosaic <- "fluc"
	}
	
	section
}


