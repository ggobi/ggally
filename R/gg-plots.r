#' Plots the Scatter Plot
#' Make a scatter plot with a given data set
#'
#' @param data data set using
#' @param ... other arguments to add to geom_point
#' @author Barret Schloerke \email{bigbear@@iastate.edu}, Haesung Kim \email{hae0510@@iastate.edu}, Di Cook \email{dicook@iastate.edu} and Heike Hofmann \email{hofmann@@iastate.edu}
#' @keywords hplot
#' @examples
#' ggplot_points(mtcars[,3:4])
#' ggplot_points(mtcars[,3:4],colour = as.factor(mtcars[,"cyl"]), size = mtcars[,"qsec"] - 16)
ggplot_points <- function(data, ...)
{
	
	oNames <- colnames(data)
	colnames(data) <- c("X","Y")

	ggplot(data = data, aes(x = X, y = Y)) + geom_point(...) + labs( x = oNames[1], y = oNames[2])
}

#' Plots the Scatter Plot with Smoothing
#' Add a smoothed condition mean with a given scatter plot
#'
#' @param data data set using
#' @param ... other arguments to add to geom_point
#' @author Barret Schloerke \email{bigbear@@iastate.edu}, Haesung Kim \email{hae0510@@iastate.edu}, Di Cook \email{dicook@iastate.edu} and Heike Hofmann \email{hofmann@@iastate.edu}
#' @keywords hplot
#' @examples
#' ggplot_smooth(iris[, 1:2])
#' ggplot_smooth(iris[, c(1, 3)], colour = as.factor(iris[, "Species"]))
ggplot_smooth <- function(data, ...)
{
	oNames <- colnames(data)
	colnames(data) <- c("X","Y")

	ggplot(data = data, aes(x = X, y = Y)) +
		geom_smooth(method="lm", colour = I("black")) +
		geom_point(...) +
		labs( x = oNames[1], y = oNames[2])
}

#' Plots the Scatter Density Plot
#' Make a scatter density plot from a given data
#'
#' @param data data set using
#' @author Barret Schloerke \email{bigbear@@iastate.edu}, Haesung Kim \email{hae0510@@iastate.edu}, Di Cook \email{dicook@iastate.edu} and Heike Hofmann \email{hofmann@@iastate.edu}
#' @keywords hplot
#' @examples
#' ggplot_density(iris[,c(1,3)])
#' ggplot_density(iris[,c(1,3)], filled = TRUE)
ggplot_density <- function(data, filled = FALSE)
{
	oNames <- colnames(data)
	colnames(data) <- c("X","Y")

	if(filled)
		p <- ggplot(data = data, aes(x = X, y = Y)) + stat_density2d(aes(fill = ..level..), geom="polygon")
	else
		p <- qplot(data = data, X, Y, geom = "density2d", colour = I("black"))
	
	p <- p + labs(x = oNames[1], y = oNames[2])	
	p
}

#' Correlation from the Scatter Plot
#' estimate correlation from the given data
#'
#' @param data data set using
#' @param size setting size
#' @param colour setting color
#' @author Barret Schloerke \email{bigbear@@iastate.edu}, Haesung Kim \email{hae0510@@iastate.edu}, Di Cook \email{dicook@iastate.edu} and Heike Hofmann \email{hofmann@@iastate.edu}
#' @keywords hplot
#' @examples
#' ggplot_cor(iris[,c(1,3)])
#' ggplot_cor(iris[,c(1,3)], size = 15, colour = "red")
ggplot_cor <- function(data,size = 8,colour = "black", ...)
{
	.ggplot_corInternal(data[,1], data[,2], size = size, colour = colour, ...)
}
		

#' Plots the Box Plot
#' Make a box plot with a given data set
#'
#' @param data data set using
#' @param ...
#' @author Barret Schloerke \email{bigbear@@iastate.edu}, Haesung Kim \email{hae0510@@iastate.edu}, Di Cook \email{dicook@iastate.edu} and Heike Hofmann \email{hofmann@@iastate.edu}
#' @keywords hplot
#' @examples
#' ggplot_box(iris[,4:5])
#' ggplot_box(iris[,5:4], outlier.colour = "red", outlier.shape = 13, outlier.size = 8)
ggplot_box <- function(data, ...)
{

	horizontal <- length(unique(data[,1])) > length(unique(data[,2]))
	#print(horizontal)
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

#' Plots the Box Plot with Dot
#' Add jittering with the box plot
#'
#' @param data data set using
#' @param ...
#' @author Barret Schloerke \email{bigbear@@iastate.edu}, Haesung Kim \email{hae0510@@iastate.edu}, Di Cook \email{dicook@iastate.edu} and Heike Hofmann \email{hofmann@@iastate.edu}
#' @keywords hplot
#' @examples
#' ggplot_dot(iris[,4:5])
#' ggplot_dot(iris[,5:4],colour = as.factor(iris[,"Species"]))
ggplot_dot <- function(data, ...)
{
	horizontal <- length(unique(data[,1])) > length(unique(data[,2]))
	oNames <- colnames(data)
	colnames(data) <- c("X","Y")
	if(horizontal)
	{
		p <- ggplot(data = data, aes(x = X, y = Y)) + geom_jitter(...) + 
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
		p <- ggplot(data = data, aes(x = X, y = Y)) + geom_jitter(...)
	}
	
	p <- p + labs(x = oNames[1], y = oNames[2])
	
	p
}


#' Plots the Histograms by Faceting
#' Make histograms by displaying subsets of the data in different panels
#'
#' @param data data set using
#' @param ...
#' @author Barret Schloerke \email{bigbear@@iastate.edu}, Haesung Kim \email{hae0510@@iastate.edu}, Di Cook \email{dicook@iastate.edu} and Heike Hofmann \email{hofmann@@iastate.edu}
#' @keywords hplot
#' @examples
#' ggplot_facethist(iris[,4:5])
#' ggplot_facethist(iris[,5:4], binwidth = .1)
ggplot_facethist <- function(data, ...)
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
		) + labs(x = oNames[1]) + scale_y_continuous(oNames[2])
	}
	else
	{	
		p <- qplot(
				Y, 
				data = data, 
				stat = "bin", 
				facets = .  ~ X,
				...
			) + coord_flip() + labs(x = oNames[2]) + scale_y_continuous(oNames[1])
									
			# + opts(strip.text.y = theme_blank()) 

	}
	
	p
}


#' Plots the density plots by Faceting
#' Make density plots by displaying subsets of the data in different panels
#'
#' @param data data set using
#' @param ...
#' @author Barret Schloerke \email{bigbear@@iastate.edu}, Haesung Kim \email{hae0510@@iastate.edu}, Di Cook \email{dicook@iastate.edu} and Heike Hofmann \email{hofmann@@iastate.edu}
#' @keywords hplot
#' @examples
#' ggplot_facetdensity(iris[,4:5])
#' ggplot_facetdensity(iris[,5:4], colour = "blue")
ggplot_facetdensity <- function(data, ...)
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
			facet_grid(Y ~ .) + labs(x = oNames[1]) + scale_y_continuous(oNames[2])
	}
	else
	{
		p <- ggplot(data, aes(x = Y)) + 
			scale_y_continuous() + 
			stat_density(
				aes(
					x = Y, 
					y = ..scaled.. * diff(range(x)) + min(x)
				),
				position = "identity",  
				geom = "line",
				...
			)+
			scale_x_continuous()+
			ylim(range(data[,2])) +
			facet_grid(. ~ X) + 
			coord_flip() + labs(x = oNames[1]) + scale_y_continuous(oNames[2])

	}
	p
}


#' Plots the Tile Plot
#' Make Tile Plot as densely as possible
#'
#' @param data data set using
#' @param ...
#' @author Barret Schloerke \email{bigbear@@iastate.edu}, Haesung Kim \email{hae0510@@iastate.edu}, Di Cook \email{dicook@iastate.edu} and Heike Hofmann \email{hofmann@@iastate.edu}
#' @keywords hplot
#' @examples
#' ggplot_denstrip(iris[,4:5])
#' ggplot_denstrip(iris[,5:4], binwidth = .2) + scale_fill_gradient(low = "grey80", high = "black")
ggplot_denstrip <- function(data,...)
{
#	if(ncol(data) != 2)
#		stop("The number of columns in 'data' != 2.")
#	if(is.numeric(data[,2]))
#		stop("Column 2 of data is numeric.  It needs to be factors.")
#	if(!is.numeric(data[,1]))
#		stop("Column 1 of data is not numeric.  It needs to be numeric.")
	
	oNames <- colnames(data)
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
		labs(x = oNames[1]) + scale_y_continuous(oNames[2])
		# + 
		#scale_fill_gradient("Density", low = "grey80", high = "black")
	
	if(horizontal)
		p <- p +facet_grid(Y ~ .) + opts(axis.text.y = theme_blank()) 

	else
		p <- p +facet_grid(. ~ Y) + coord_flip() + opts(axis.text.x = theme_blank()) 

	p

}

#' Plots the Mosaic Plots
#' Plots the mosaic plot by using fluctuation
#'
#' @param data data set using
#' @param ...
#' @author Barret Schloerke \email{bigbear@@iastate.edu}, Haesung Kim \email{hae0510@@iastate.edu}, Di Cook \email{dicook@iastate.edu} and Heike Hofmann \email{hofmann@@iastate.edu}
#' @keywords hplot
#' ggplot_rata(movies[,c("mpaa","Action")])
#' ggplot_rata(movies[,c("mpaa","Action")]) + coord_equal()
#' ggplot_rata(movies[,c("Action","mpaa")]) + opts(aspect.ratio = (length(levels(movies[,"mpaa"])) ) / (length(levels(as.factor(movies[,"Action"]))) ) )
ggplot_rata <- function(data)
{
	dataNames <- colnames(data)
	ggfluctuation2(table(data[,2], data[,1])) + labs(x = dataNames[1], y = dataNames[2])
}


#' Plots the Density Plots by Using Identity
#' Plots the density plots by using identity
#'
#' @param data data set using
#' @param ...
#' @author Barret Schloerke \email{bigbear@@iastate.edu}, Haesung Kim \email{hae0510@@iastate.edu}, Di Cook \email{dicook@iastate.edu} and Heike Hofmann \email{hofmann@@iastate.edu}
#' @keywords hplot
#' ggplot_densityI(movies[,c("rating","rating")])
#' ggplot_densityI(movies[,c("rating","rating")], colour = "blue")
ggplot_densityI <- function(data, ...)
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
			geom = "line",
			...
		)+
		ylim(range(data[,1])) +
		xlab(namesData[1])
}

#' Plots the Bar Plots by Using Identity
#' Plots the bar plots by using identity
#'
#' @param data data set using
#' @param ...
#' @author Barret Schloerke \email{bigbear@@iastate.edu}, Haesung Kim \email{hae0510@@iastate.edu}, Di Cook \email{dicook@iastate.edu} and Heike Hofmann \email{hofmann@@iastate.edu}
#' @keywords hplot
#' ggplot_bar(movies[,c("mpaa","mpaa")])
#' ggplot_bar(movies[,c("rating","rating")], binwidth = .1)
ggplot_bar <- function(data, ...)
{
	namesData <- colnames(data)
	colnames(data) <- c("X","Y")


	numer <- is.null(attributes(data[,1])$class)
	
	if(numer)
	{
		p <- qplot(X,data = data, geom="bar", ...)	
	}
	else
	{
		dataTmp <- as.factor(data[,1])
		count <- c()
		for(z in 1:length(levels(dataTmp)))
			count <- c(count,length(dataTmp[dataTmp==levels(dataTmp)[z]]))
					
		p <- qplot(levels(dataTmp), count, geom="bar", stat="identity", ...) +
			coord_flip() +  
			opts(
				axis.text.y = theme_text(
					angle = 90, 
					vjust = 0, 
					colour = "grey50"
				)
			)
	}
	p <- p + xlab(namesData[1])
	
	p
}


#' GGplot Text
#' Plot text for a plot
#'
#' @param xP horizontal position percentage
#' @param yP vertical position percentage
#' @param label text that you want to appear
#' @param xrange range of the data around it.  Only nice to have if plotting in a matrix
#' @param yrange range of the data around it.  Only nice to have if plotting in a matrix
#' @param ... other arguments for geom_text
#' @author Barret Schloerke \email{bigbear@@iastate.edu}, Haesung Kim \email{hae0510@@iastate.edu}, Di Cook \email{dicook@iastate.edu} and Heike Hofmann \email{hofmann@@iastate.edu}
#' @keywords hplot
#' ggplot_text("Example 1")
#' ggplot_text("Example\nTwo",size = 15, colour = "red")
ggplot_text <- function(label, xP = 0.5, yP = 0.5,xrange = c(0,1), yrange = c(0,1), ...)
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


#' Correlation Text
#' Correlation text printed for a given data set
#'
#' @param xVar x variable
#' @param yVar y variable
#' @param ... arguements to be supplied to geom_text
#' @author Barret Schloerke \email{bigbear@@iastate.edu}, Haesung Kim \email{hae0510@@iastate.edu}, Di Cook \email{dicook@iastate.edu} and Heike Hofmann \email{hofmann@@iastate.edu}
#' @keywords internal
.ggplot_corInternal <- function(xVar, yVar, ...)
{

	ggplot_text(
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


#' Fluctuation plot
#' Create a fluctuation plot.
#'
#' A fluctutation diagram is a graphical representation of a contingency table. This fuction currently only supports 2D contingency tabless but extension to more should be relatively straightforward.
#'
#' @param table a table of values, or a data frame with three columns, the last column being frequency
#' @param floor don't display cells smaller than this value
#' @param ceiling max value to compare to
#' @author Barret Schloerke \email{bigbear@@iastate.edu}, Haesung Kim \email{hae0510@@iastate.edu}, Di Cook \email{dicook@iastate.edu} and Heike Hofmann \email{hofmann@@iastate.edu}
#' @keywords hplot
#' @examples
#' ggfluctuation(table(movies$Action, movies$Comedy))
#' ggfluctuation(table(movies$Action, movies$mpaa))
#' ggfluctuation(table(movies$Action, movies$Comedy), type="colour")
#' ggfluctuation(table(warpbreaks$breaks, warpbreaks$tension))
ggfluctuation2 <- function (table, floor = 0, ceiling = max(table$freq, na.rm = TRUE)) 
{
	

	yNames <- rownames(table)
	xNames <- colnames(table)
	oldnames <- names(table)

	if (is.table(table)) 
	{
		table <- as.data.frame(t(table))
		
		oldnames <- c("X","Y")		
		
	}

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
	
	maxLen <- max(diff(range(as.numeric(table$x))), diff(range(as.numeric(table$y))) )
	

	table <- cbind(table, xNew, yNew)
	#print(table)
	#print(xNames)
	#print(yNames)
	
	#cat("\nmaxLen");print(maxLen)

	
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
#			limits=c(1,maxLen + 2), 
#			breaks=1:(maxLen + 2), 
#			labels=c(xNames,rep("",maxLen - length(xNames) + 2)), 
			limits=c(1,length(xNames) + 1), 
			breaks=1:(length(xNames) + 1), 
			labels=c(xNames,""), 
			minor_breaks=FALSE
		) + 
		scale_y_continuous(
			name=oldnames[2], 
#			limits=c(1,maxLen + 2), 
#			breaks=1:(maxLen + 2), 
#			labels=c(yNames,rep("",maxLen - length(yNames) + 2)), 
			limits=c(1,length(yNames) + 1), 
			breaks=1:(length(yNames) + 1), 
			labels=c(yNames,""), 
			minor_breaks=FALSE
		) + 
#		coord_equal() + 
		opts(
			axis.text.x = theme_text(
				hjust = 0, 
				vjust = 1,
				colour = "grey50"
			),
			axis.text.y = theme_text(
				angle = 90, 
				vjust = 0, 
				hjust = 0,
				colour = "grey50"
			)
		)
	
	p
}


#' Blank
#' Drawing nothing
#' 
#' Makes a "blank" ggplot object that will only draw white space
#'
#' @author Barret Schloerke \email{bigbear@@iastate.edu}, Haesung Kim \email{hae0510@@iastate.edu}, Di Cook \email{dicook@iastate.edu} and Heike Hofmann \email{hofmann@@iastate.edu}
#' @keywords hplot
ggplot_blank <- function()
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

#hi