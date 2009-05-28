

ggpairs <- function (data, mapping = aes(), colour = "black", ...) 
{
	data <- as.data.frame(data)
	numCol <- ncol(data)

	grid.newpage()
	
	.v1 <<- viewport(
#		x = unit(0.5, "npc") + unit(1,"lines"), 
#		y = unit(0.5, "npc") + unit(1,"lines"), 
		width=unit(1, "npc") - unit(3,"lines"), 
		height=unit(1, "npc") - unit(3, "lines")
	)
	
	.v2 <<- viewport(layout = grid.layout(numCol, numCol, widths = rep(1,numCol), heights = rep(1,numCol) ))

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

	for(i in 1:nrow(dataTypes))
	{
		type <- dataTypes[i,"Type"]

		posX <- dataTypes[i,"posx"]
		posY <- dataTypes[i,"posy"]

		dataSelect <- data[,c(posX,posY)]
		colnames(dataSelect) <- c("X","Y")

#print(head(dataSelect))
		if(type == "scatterplot")
		{
		#reg	p <- qplot(dataSelect[,1], dataSelect[,2]) + geom_point(colour = colour)

		#contour
			p <- qplot(dataSelect[,1], dataSelect[,2], geom = "density2d", colour = I("black"))
		
		#filled contour	p <- ggplot(data = dataSelect, aes(x = X, y = Y)) + stat_density2d(aes(fill = ..level..), geom="polygon")
		}
		else if(type == "box-hori")
		{
			p <- qplot(dataSelect[,2], dataSelect[,1], geom="boxplot") + coord_flip() +  
				opts(
					axis.text.y = theme_text(
						angle = 90, 
						vjust = 0, 
						colour = "grey50")
				)
		}
		else if(type == "box-vert")
			p <- qplot(dataSelect[,1], dataSelect[,2], geom="boxplot")
		else if(type == "mosaic")
		{
			p <- ggfluctuation(table(dataSelect[,2], dataSelect[,1])) +  
				opts(
					axis.text.y = theme_text(
						angle = 90, 
						vjust = 0, 
						colour = "grey50")
				)
		}
		else if(type == "stat_bin-num")
		{
			colnames(dataSelect) <- c("X","Y")
			p <- ggplot(dataSelect, aes(x = X)) + 
				scale_x_continuous() + 
				scale_y_continuous() + 
				stat_density(
					aes(
						x = X, 
						y = ..scaled.. * diff(range(x)) + min(x)
					),
					position = "identity", 
					colour = "grey20", 
					geom = "line"
				)+
				ylim(range(dataSelect[,"X"]))

		}
		else if(type == "stat_bin-cat")
		{
#			colnames(dataSelect) <- c("X","Y")
#			#p <- qplot(dataSelect[,1],geom="bar", position = "stack")
#			p <- ggplot(dataSelect, aes(x=X))+labs(x=NULL,y=NULL) +  geom_bar( aes(
#						y = ..count.. *diff(range(1:2)) #diff(range(x)) + min(x) 
#					)
#				)
			
			dataTmp <- as.factor(dataSelect[,1])
			#print(tmp)
			count <- c()
			for(z in 1:length(levels(dataTmp)))
				count <- c(count,length(dataTmp[dataTmp==levels(dataTmp)[z]]))
				
			p <- qplot(levels(dataTmp), count, geom="bar", stat="identity")	+ coord_flip() +  
				opts(
					axis.text.y = theme_text(
						angle = 90, 
						vjust = 0, 
						colour = "grey50")
				)


#			p <- ggplot(dataSelect, aes(x=x)) + 
#				stat_bin()
#					aes(
#						y = ..count.. * 1 #diff(range(x)) + min(x)
#					),
#					position = "identity", 
#					colour = "grey20"#, 
#					geom = "line"
#				)
		
		}
		
		
		if(type == "box-hori")
		{
			if( posX != 1)
			{
				p <- p + opts(axis.text.y = theme_blank() )
			}
			if( posY != numCol)
			{
				p <- p + opts(axis.text.x = theme_blank() )
			}	
		}
		else
		{
			if( posX != 1)
			{
				p <- p + opts(axis.text.y = theme_blank() )
			}
			if( posY != numCol)
			{
				p <- p + opts(axis.text.x = theme_blank() )
			}	
		}
		
		p <- p + 
			labs(x = NULL, y = NULL) + 
			opts(plot.margin = unit(rep(0,4), "lines"))

		
		putPlot(as.numeric(posY),as.numeric(posX),p)
		
	}


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
	
	return()
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


ggplot_Text <- function(xP, yP, label, ...)
{

	p <- ggplot(data = 
			data.frame(
				x0 = xP,
				y0 = yP,
				x1 = c(0), 
				x2 = c(1), 
				y1 = c(0), 
				y2 = c(1)
			),
			aes(
				xmin=x1,
				xmax = x2, 
				ymin = y1, 
				ymax = y2
			)
		)+
		geom_rect(fill= I("white")) + 
		labs(x = "X", y = "Y")
	
	p <- p + geom_text(aes(x = x0, y = y0), label = label, ...)
	
	p

}


ggplot_Cor <- function(xVar, yVar)
{

	ggplot_Text(
		x=0.5,
		y=.5,
		paste(
			"Corr:\n",
			signif(
				cor(xVar,yVar),
				3
			),
			sep="",collapse=""
		),
		size=8,
		colour = "Black")

}
