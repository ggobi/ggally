ggscatter_points <- function(data, ...)
{
	
	oNames <- colnames(data)
	colnames(data) <- c("X","Y")

	ggplot(data = data, aes(x = X, y = Y)) + geom_point(...) + labs( x = oNames[1], y = oNames[2])
}

ggscatter_smooth <- function(data, ...)
{
	oNames <- colnames(data)
	colnames(data) <- c("X","Y")

	ggplot(data = data, aes(x = X, y = Y))  + geom_smooth(method="lm", colour = I("black"))+ geom_point(...) + labs( x = oNames[1], y = oNames[2])
}

ggscatter_density <- function(data)
{
	#contour 
		p <- qplot(data[,1], data[,2], geom = "density2d", colour = I("black"))
	
	#filled contour
		# p <- ggplot(data = data, aes(x = X, y = Y)) + stat_density2d(aes(fill = ..level..), geom="polygon")
		
		p
}

ggscatter_cor <- function(data,size = 8,colour = "black")
{
	ggplot_Cor(data[,1], data[,2], size = size, colour = colour)
}
		

ggbox_reg <- function(data, ...)
{

	horizontal <- length(unique(data[,1])) > length(unique(data[,2]))
	print(horizontal)
	if(horizontal)
	{
		p <- qplot(data[,2], data[,1], geom="boxplot",...) + 
			coord_flip() +  
			opts(
				axis.text.y = theme_text(
					angle = 90, 
					vjust = 0, 
					colour = "grey50"
				)
			) + 
			labs(x = colnames(data)[2], y = colnames(data)[1])
	}
	else
	{
		p <- qplot(data[,1], data[,2], geom="boxplot",...) + labs(x = colnames(data)[1], y = colnames(data)[2])
	}
	p 
}

ggbox_dot <- function(data,...)
{
	horizontal <- length(unique(data[,1])) > length(unique(data[,2]))
	
	if(horizontal)
	{
		p <- qplot(data[,1], data[,2], geom = "jitter") + 
			opts(
				axis.text.y = theme_text(
					angle = 90, 
					vjust = 0, 
					colour = "grey50"
				)
			)
	}
	else
	{
		p <- qplot(data[,1], data[,2], geom = "jitter") 
	}
	p + labs(x = colnames(data)[1], y = colnames(data)[2])
}


ggbox_facethist <- function(data, ...)
{
	horizontal <- length(unique(data[,1])) > length(unique(data[,2]))

	oNames <- colnames(data)
	colnames(data) <- c("X","Y")

	if(horizontal)
	{
		data[,2] <- factor( data[,2], levels=levels(data[,2])[length(levels(data[,2])):1] )
				
		p <- qplot(
			X,
			data = data,
			stat = "bin",
			facets = Y  ~ .,
			...
		) + labs(x = oNames[1], y = oNames[2])
	}
	else
	{	
		p <- qplot(
				Y, 
				data = data, 
				stat = "bin", 
				facets = .  ~ X,
				...
			) + coord_flip() + labs(x = oNames[1], y = oNames[2])
									
			# + opts(strip.text.y = theme_blank()) 

	}
	p
}


ggbox_facetdensity <- function(data, ...)
{
	horizontal <- length(unique(data[,1])) > length(unique(data[,2]))

	oNames <- colnames(data)
	colnames(data) <- c("X", "Y")

	if(horizontal)
	{
		data[,2] <- factor(data[,2], levels=levels(data[,2])[length(levels(data[,2])):1])

		p <- ggplot(data, aes(x = X)) + 
			scale_x_continuous() + 
			scale_y_continuous() + 
			stat_density(
				aes(
					x = X, 
					y = ..scaled.. * diff(range(x)) + min(x)
				),
				position = "identity", 
				geom = "line",
				...
			)+
			ylim(range(data[,1])) + 
			facet_grid(Y ~ .)# + opts(strip.gp = NULL)
	}
	else
	{
		p <- ggplot(data, aes(x = Y)) + 
			scale_x_continuous() + 
			scale_y_continuous() + 
			stat_density(
				aes(
					x = Y, 
					y = ..scaled.. * diff(range(x)) + min(x)
				),
				position = "identity", 
				colour = "black", 
				geom = "line"
			)+
			ylim(range(data[,2])) +
			facet_grid(. ~ X) + 
			coord_flip()
			# + opts(strip.text.y = theme_blank()) 

	}
	p
}


ggbox_denstrip <- function(data,...)
{
#	if(ncol(data) != 2)
#		stop("The number of columns in 'data' != 2.")
#	if(is.numeric(data[,2]))
#		stop("Column 2 of data is numeric.  It needs to be factors.")
#	if(!is.numeric(data[,1]))
#		stop("Column 1 of data is not numeric.  It needs to be numeric.")
	
	
	horizontal <- length(unique(data[,1])) < length(unique(data[,2]))
#print(horizontal)
	if(horizontal)
	{
		data[,1] <- factor(data[,1], levels = levels(data[,1])[length(levels(data[,1])):1])
		data <- data[,2:1]
	}
		
	namesData <- colnames(data)
	colnames(data) <- c("X","Y")

	p <- ggplot(data = data, aes( y = 1, x=X)) + 
		stat_bin(
			aes(fill=..density..), 
			geom="tile",  
			position="identity",
			...
		) + 
		labs(x = namesData[1], y = NULL)# + 
		#scale_fill_gradient("Density", low = "grey80", high = "black")
	
	if(horizontal)
		p <- p +facet_grid(Y ~ .) + opts(axis.text.y = theme_blank()) 

	else
		p <- p +facet_grid(. ~ Y) + coord_flip() + opts(axis.text.x = theme_blank()) 

	p

}

ggmosaic_fluc <- function(data,...)
{
	ggfluctuation2(table(data[,2], data[,1])) +  
		opts(
			axis.text.y = theme_text(
				angle = 90, 
				vjust = 0, 
				colour = "grey50")
		)
}


ggiden_density <- function(data, ...)
{
	namesData <- colnames(data)
	colnames(data) <- c("X","Y")

	ggplot(data, aes(x = X)) + 
		scale_x_continuous() + 
		scale_y_continuous() + 
		stat_density(
			aes(
				x = X, 
				y = ..scaled.. * diff(range(x)) + min(x)
			),
			position = "identity", 
			colour = "black", 
			geom = "line"
		)+
		ylim(range(data[,1])) +
		xlab(namesData[1])
}


ggiden_bar <- function(data, ...)
{
#			colnames(data) <- c("X","Y")
#			#p <- qplot(data[,1],geom="bar", position = "stack")
#			p <- ggplot(data, aes(x=X))+labs(x=NULL,y=NULL) +  geom_bar( aes(
#						y = ..count.. *diff(range(1:2)) #diff(range(x)) + min(x) 
#					)
#				)

#			p <- ggplot(data, aes(x=x)) + 
#				stat_bin()
#					aes(
#						y = ..count.. * 1 #diff(range(x)) + min(x)
#					),
#					position = "identity", 
#					colour = "grey20"#, 
#					geom = "line"
#				)

	namesData <- colnames(data)
	colnames(data) <- c("X","Y")


	numer <- is.null(attributes(data[,1])$class)
	
	if(numer)
	{
		p <- qplot(X,data = data, geom="bar")	
	}
	else
	{
		dataTmp <- as.factor(data[,1])
		#print(tmp)
		count <- c()
		for(z in 1:length(levels(dataTmp)))
			count <- c(count,length(dataTmp[dataTmp==levels(dataTmp)[z]]))
					
		p <- qplot(levels(dataTmp), count, geom="bar", stat="identity")	+ coord_flip() +  
			opts(
				axis.text.y = theme_text(
					angle = 90, 
					vjust = 0, 
					colour = "grey50"
				)
			)
	}
	p + xlab(namesData[1])
}


ggplot_Text <- function(xP, yP, label,xrange = c(0,1),yrange = c(0,1), ...)
{

	p <- ggplot(data = 
			data.frame(
				x0 = xP * diff(xrange) + min(xrange),
				y0 = yP * diff(yrange) + min(yrange),
				x1 = xrange[1], 
				x2 = xrange[2], 
				y1 = yrange[1], 
				y2 = yrange[2]
			),
			aes(
				x = x0,
				y = y0,
				xmin=x1,
				xmax = x2, 
				ymin = y1, 
				ymax = y2
			)
		)+
		geom_rect(fill= I("white")) + 
		labs(x = NULL, y = NULL)
	
	p <- p + geom_text( label = label, ...)
	
	p

}


ggplot_Cor <- function(xVar, yVar, ...)
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
		xrange = range(xVar),
		yrange = range(yVar),
		...
	) +  theme_bw()

}


ggiden_fluc <- function(data,...)
{
	ggfluctuation2(table(data[,1], data[,1])) +  
		opts(
			axis.text.y = theme_text(
				angle = 90, 
				vjust = 0, 
				colour = "grey50")
		)
}


ggfluctuation2 <- function (table, floor = 0, ceiling = max(table$freq, 
	na.rm = TRUE)) 
{
	

	xNames <- rownames(table)
	yNames <- colnames(table)

	if (is.table(table)) 
		table <- as.data.frame(t(table))


	oldnames <- names(table)
	names(table) <- c("x", "y", "result")
	table <- add.all.combinations(table, list("x", "y"))
	table <- transform(table, x = as.factor(x), y = as.factor(y), 
		freq = result)

	table <- transform(table, freq = sqrt(pmin(freq * .95, ceiling)/ceiling), 
		border = ifelse(is.na(freq), "grey90", ifelse(freq > 
			ceiling, "grey30", "grey50")))
	table[is.na(table$freq), "freq"] <- 1
	table <- subset(table, freq * ceiling >= floor)
	
	xNew <- as.numeric(table$x) + 1/2 * table$freq
	yNew <- as.numeric(table$y) + 1/2 * table$freq
	
	table <- cbind(table, xNew, yNew)
	print(table)
	#print(xNames)
	#print(yNames)
	p <- ggplot(
			table, 
			aes_string(
				x = "xNew", 
				y = "yNew", 
				height = "freq", 
				width = "freq", 
				fill = "border"
			)
		) + 
		geom_tile(colour = "white") + 
		scale_fill_identity() + 
		scale_x_continuous(
			name=oldnames[1], 
			limits=c(1,length(xNames) + 1), 
			breaks=1:(length(xNames) + 1), 
			labels=c(xNames,""), 
			minor_breaks=FALSE
		) + 
		scale_y_continuous(
			name=oldnames[2], 
			limits=c(1,length(yNames) + 1), 
			breaks=1:(length(yNames) + 1), 
			labels=c(yNames,""), 
			minor_breaks=FALSE
		) + 
		coord_equal() + 
		opts(
			axis.text.x = theme_text(
				hjust = 1, 
				vjust = 1,
				colour = "grey50"
			),
			axis.text.y = theme_text(
				angle = 90, 
				vjust = 0, 
				colour = "grey50"
			)
		)
	
	p
}


gg_blank_plot <- function()
{
	a <- as.data.frame(cbind(1:2,1:2))
	colnames(a) <- c("X","Y")
	
	ggplot(data = a, aes(x = X, y = Y)) + geom_point( colour = "transparent") + opts(
		axis.line = theme_blank(),
		axis.text.x = theme_blank(),
		axis.text.y = theme_blank(),
		axis.ticks = theme_blank(),
		axis.title.x = theme_blank(),
		axis.title.y = theme_blank(),
		legend.background = theme_blank(),
		legend.key = theme_blank(),
		legend.text = theme_blank(),
		legend.title = theme_blank(),
		panel.background = theme_blank(),
		panel.border = theme_blank(),
		panel.grid.major = theme_blank(),
		panel.grid.minor = theme_blank(),
		plot.background = theme_blank(),
		plot.title = theme_blank(),
		strip.background = theme_blank(),
		strip.text.x = theme_blank(),
		strip.text.y = theme_blank()
	)
}

