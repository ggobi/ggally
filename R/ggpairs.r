ggpairs <- function (data, mapping = aes(), colour = "black") 
{
    grid <- expand.grid(x = 1:ncol(data), y = 1:ncol(data))
    grid <- subset(grid, x != y)
print(grid)
    all <- do.call("rbind", lapply(1:nrow(grid), function(i) {
        xcol <- grid[i, "x"]
        ycol <- grid[i, "y"]
        data.frame(xvar = names(data)[ycol], yvar = names(data)[xcol], 
            x = data[, xcol], y = data[, ycol], data)
    }))
#	print(all)

    all$xvar <- factor(all$xvar, levels = names(data))
    all$yvar <- factor(all$yvar, levels = names(data))

    densities <- do.call("rbind", lapply(1:ncol(data), function(i) {
        data.frame(xvar = names(data)[i], yvar = names(data)[i], 
            x = as.numeric(data[, i]))
    }))

	print(head(all))
	print(head(densities))
	allTrick <- all[,c("xvar","yvar","x","y")]
	allTrick[,"x"] <- 1
	allTrick[,"y"] <- 1

    mapping <- defaults(mapping, aes_string(x = "x", y = "y"))

    class(mapping) <- "uneval"

    p <- ggplot(allTrick, mapping) + scale_x_continuous() + scale_y_continuous()+
		facet_grid(xvar ~ yvar, scales = "free") + 
#		stat_density(
#			aes(
#				x = x, 
#				y = ..scaled.. * diff(range(x)) + min(x)
#			), 
#			data = densities,
#			position = "identity", 
#			colour = "grey20", 
#			geom = "line"
#		)+

		geom_point(colour = colour, na.rm = TRUE) #+ 
#		geom_histogram(
#			aes(x = x#, 
#			#y = ..scaled.. * diff(range(x)) + min(x)
#			), 
#			data = densities, 
 #       	position = "identity", 
#			#colour = "grey20"#, 
#			#geom = "line"
#			)
	
	dataTypes <- plot_types(data)
	
	if("box-vert" %in% dataTypes[,1])
	{
		data_tmp <- get_select_data(all,dataTypes[dataTypes[,1]=="box-vert",])
		print(head(data_tmp))
		p <- p+ geom_boxplot(data = data_tmp)
	}
	
	p+ geom_point(na.rm = TRUE)
}