ggpairs <- function (data, mapping = aes(), colour = "black") 
{

	data <- as.data.frame(data)

	grid.newpage()
	numCol <- ncol(data)
	
#	pushViewport(viewport(layout = grid.layout(numCol + 1, numCol + 1, widths = c(0.5,rep(1,numCol)), heights = c(rep(1,numCol),0.5) )))
	pushViewport(viewport(layout = grid.layout(numCol, numCol)
	
	
	
	vplayout <- function(x, y) 
		viewport(layout.pos.row = x, layout.pos.col = y) 




    grid <- expand.grid(x = 1:ncol(data), y = 1:ncol(data))
#    grid <- subset(grid, x != y)
cat("\nGRID\n");print(grid)
#print(dataTypes)


    all <- do.call("rbind", lapply(1:nrow(grid), function(i) {
        xcol <- grid[i, "x"]
        ycol <- grid[i, "y"]
        data.frame(xvar = names(data)[ycol], yvar = names(data)[xcol])
    }))

cat("\n\n\nALL\n");print(all)

	dataTypes <- plot_types(data)
cat("\n\n\nDATA TYPES\n");print(dataTypes)

	for(i in 1:nrow(dataTypes))
	{
		type <- dataTypes[i,"Type"]
#		dataSelect <- get_select_data(data,grid[i,])
		posX <- dataTypes[i,"posx"]
		posY <- dataTypes[i,"posy"]
		dataSelect <- data[,c(posX,posY)]

		print(head(dataSelect))
#		if(type == "scatterplot")
			p <- qplot(dataSelect[,1],dataSelect[,2],xlab=names(data)[posX],ylab=names(data)[posY])
		if(type == "box-hori")
			p <- qplot(dataSelect[,2],dataSelect[,1],ylab=names(data)[posX],xlab=names(data)[posY],geom="boxplot") + coord_flip()
		
		if(type == "box-vert")
			p <- qplot(dataSelect[,1],dataSelect[,2],xlab=names(data)[posX],ylab=names(data)[posY],geom="boxplot")
		
#		if(type == "mosaic")
		
		if(type == "stat_bin-num")
		{
			colnames(dataSelect) <- c("x","y")
			p <- ggplot(dataSelect, aes(x = x)) + 
				scale_x_continuous() + 
				scale_y_continuous() + 
				stat_density(
					aes(
						x = x, 
						y = ..scaled.. * diff(range(x)) + min(x)
					),
					position = "identity", 
					colour = "grey20", 
					geom = "line"
				)+
				xlab(names(data)[posX]) + 
				ylab(names(data)[posY]) + 
				ylim(range(dataSelect[,"x"]))

		}
		
		if(type == "stat_bin-cat")
		{
			p <- qplot(dataSelect[,1],xlab=names(data)[posX],ylab=names(data)[posY],geom="histogram", position = "stack")
		
		}
		
			
		if(type == "box-hori")
		{
			if( posX != 1)
			{
				p <- p + labs(x = NULL)
				p <- p + opts(axis.text.y = theme_blank() )
			}
			if( posY != numCol)
			{
				p <- p + labs(y = NULL)
				p <- p + opts(axis.text.x = theme_blank() )
			}	
		}
		else
		{
			if( posX != 1)
			{
				p <- p + labs(y = NULL)
				p <- p + opts(axis.text.y = theme_blank() )
			}
			if( posY != numCol)
			{
				p <- p + labs(x = NULL)
				p <- p + opts(axis.text.x = theme_blank() )
			}	
		}
		
				
		p <- p + opts(plot.margin = unit(rep(0,4), "lines"))

		
		
		print(p, vp = vplayout(as.numeric(posY), as.numeric(posX)))#+1)) 
		
		
	}

#	for(i in 1:numCol)
#	{
#		pushViewport(vplayout(as.numeric(i), 1))
#		grid.text(names(data)[i],1,0.5,rot=90,just=c("centre","bottom"))
#		popViewport()
#	}
#
#	for(i in 1:numCol)
#	{
#		pushViewport(vplayout(numCol+1, i+1))
#		grid.text(names(data)[i],0.5,1,just=c("centre","top"))
#		popViewport()
#	}


	


#
#
#
#    grid <- expand.grid(x = 1:ncol(data), y = 1:ncol(data))
#    grid <- subset(grid, x != y)
#
#	
#
#print(grid)
#
#	
#	boxRows <- dataTypes[,"Type"] == "box-vert"
#	
#	grid <- as.matrix(subset(grid,!boxRows))
#	grid <- subset(grid,!is.na(c(grid[,1])))
#print(grid)
##print(dataTypes)
#
#
#    all <- do.call("rbind", lapply(1:nrow(grid), function(i) {
#        xcol <- grid[i, "x"]
#        ycol <- grid[i, "y"]
#        data.frame(xvar = names(data)[ycol], yvar = names(data)[xcol], 
#            x = data[, xcol], y = data[, ycol], data)
#    }))
##print(head(all))
#
#    all$xvar <- factor(all$xvar, levels = names(data))
#    all$yvar <- factor(all$yvar, levels = names(data))
#
#    densities <- do.call("rbind", lapply(1:ncol(data), function(i) {
#        data.frame(xvar = names(data)[i], yvar = names(data)[i], 
#            x = as.numeric(data[, i]))
#    }))
#
##print(head(all))
##print(head(densities))
#	allTrick <- all[,c("xvar","yvar","x","y")]
#	allTrick[,"x"] <- 1
#	allTrick[,"y"] <- 1
#	b <<- allTrick
#    mapping <- defaults(mapping, aes_string(x = "x", y = "y"))
#
#    class(mapping) <- "uneval"
#
#    p <- ggplot(allTrick, mapping) + scale_x_continuous() + scale_y_continuous()+
#		facet_grid(xvar ~ yvar, scales = "free") + 
##		stat_density(
##			aes(
##				x = x, 
##				y = ..scaled.. * diff(range(x)) + min(x)
##			), 
##			data = densities,
##			position = "identity", 
##			colour = "grey20", 
##			geom = "line"
##		)+
#
#		geom_point(colour = colour, na.rm = TRUE) #+ 
##		geom_histogram(
##			aes(x = x#, 
##			#y = ..scaled.. * diff(range(x)) + min(x)
##			), 
##			data = densities, 
# #       	position = "identity", 
##			#colour = "grey20"#, 
##			#geom = "line"
##			)
#	
#	if("box-vert" %in% dataTypes[,1])
#	{
#		data_tmp <- get_select_data(all,dataTypes[dataTypes[,1]=="box-vert",])
#
#		print(head(data_tmp))
#		p <- p+ geom_boxplot(data = data_tmp)
#	}
#	
#	p#+ geom_point(na.rm = TRUE)
}
