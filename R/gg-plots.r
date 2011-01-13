





#' Plots the Scatter Plot
#'
#' Make a scatter plot with a given data set
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... other arguments are sent to geom_point
#' @author Barret Schloerke  \email{schloerke@@gmail.com}
#' @keywords hplot
#' @examples
#' ggally_points(mtcars, aes(x = disp, y = hp))
#' ggally_points(mtcars, aes_string(x = "disp", y = "hp"))
#' ggally_points(mtcars, aes_string(x = "disp", y = "hp", color = "as.factor(cyl)", size = "gear"))
ggally_points <- function(data, mapping, ...){
  
  p <- ggplot(data = data, mapping = mapping) + geom_point(...)
  p$type <- "continuous"
  p$subType <- "points"
  p
}

#' Plots the Scatter Plot with Smoothing
#' Add a smoothed condition mean with a given scatter plot
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... other arguments to add to geom_point
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords hplot
#' @examples
#' ggally_smooth(iris, aes(x = Sepal.Length, y = Sepal.Width))
#' ggally_smooth(iris, aes_string(x = "Sepal.Length", y = "Sepal.Width"))
#' ggally_smooth(iris, aes_string(x = "Sepal.Length", y = "Petal.Length", color = "Species"))
ggally_smooth <- function(data, mapping, ...){
	p <- ggplot(data = data, mapping) +
		geom_smooth(method="lm", colour = I("black")) +
		geom_point(...) 
  p$type <- "continuous"
  p$subType <- "smooth"
  p
}

#' Plots the Scatter Density Plot
#' Make a scatter density plot from a given data
#'
#' The aesthetic "fill" determines whether or not stat_density2d (filled) or geom_density2d (lines) is used.
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... parameters sent to either stat_density2d or geom_density2d
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords hplot
#' @examples
#' ggally_density(iris, aes(x = Sepal.Length, y = Petal.Length))
#' ggally_density(iris, aes_string(x = "Sepal.Length", y = "Petal.Length"))
#' ggally_density(iris, aes_string(x = "Sepal.Length", y = "Petal.Length", fill = "..level.."))
#' ggally_density(iris, aes_string(x = "Petal.Length", y = "Petal.Width",fill = "..level..")) + scale_fill_gradient(breaks = c(0.05, 0.1,0.15,0.2)) 
ggally_density <- function(data, mapping, ...){  
  p <- ggplot(data = data, mapping)

	if(!is.null(mapping$fill)) {
		p <- p + stat_density2d(geom="polygon", ...)
	} else {
	  p <- p + geom_density2d(...)
	}
	
  p$type <- "continuous"
  p$subType <- "density"
  p

	
	p
}

#' Correlation from the Scatter Plot
#' estimate correlation from the given data
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param corAlignPercent right align position of numbers. Default is 60 percent across the horizontal
#' @param corSize size of text
#' @param ... other arguments being supplied to geom_text
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords hplot
#' @examples
#' ggally_cor(iris, aes(x = Sepal.Length, y = Petal.Length))
#' ggally_cor(iris, aes_string(x = "Sepal.Length", y = "Petal.Length", size = 15, colour = "red"))
#' ggally_cor(iris, aes_string(x = "Sepal.Length", y = "Petal.Length", color = "Species"), corSize = 15 )
ggally_cor <- function(data, mapping, corAlignPercent = 0.6, corSize = 3, ...){

  # xVar <- data[,as.character(mapping$x)]
  # yVar <- data[,as.character(mapping$y)]
  # x_bad_rows <- is.na(xVar)
  # y_bad_rows <- is.na(yVar)
  # bad_rows <- x_bad_rows | y_bad_rows
  # if (any(bad_rows)) {
  #   total <- sum(bad_rows)
  #   if (total > 1) {
  #     warning("Removed ", total, " rows containing missing values")
  #   } else if (total == 1) {
  #     warning("Removing 1 row that contained a missing value")
  #   }
  # 
  #   xVar <- xVar[!bad_rows]
  #   yVar <- yVar[!bad_rows]
  # }

  # mapping$x <- mapping$y <- NULL

	rows <- complete.cases(data)
	if(any(!rows)) {
		total <- sum(!rows)
		if (total > 1) {
		  warning("Removed ", total, " rows containing missing values")
		} else if (total == 1) {
		  warning("Removing 1 row that contained a missing value")
		}
	}
	data <- data[rows, ]
	xCol <- as.character(mapping$x)
	yCol <- as.character(mapping$y)
	colorCol <- as.character(mapping$colour)
	xVal <- data[,xCol]
	yVal <- data[,yCol]
	
	
	if(length(names(mapping)) > 0){
    for(i in length(names(mapping)):1){
      # find the last value of the aes, such as cyl of as.factor(cyl)
      tmp_map_val <- as.character(mapping[names(mapping)[i]][[1]])
      if(tmp_map_val[length(tmp_map_val)] %in% colnames(data))
        mapping[names(mapping)[i]] <- NULL
        
      if(length(names(mapping)) < 1){
        mapping <- NULL
        break;
      }
    }
  }
  
	
	# splits <- str_c(as.character(mapping$group), as.character(mapping$colour), sep = ", ", collapse = ", ")
	# splits <- str_c(colorCol, sep = ", ", collapse = ", ")
	final_text <- ""
	print(colorCol)
	if(length(colorCol) < 1)
		colorCol <- "ggally_NO_EXIST"
	# browser()
	if(colorCol != "ggally_NO_EXIST" && colorCol %in% colnames(data)) {
		
		txt <- str_c("ddply(data, .(", colorCol, "), transform, ggally_cor = cor(", xCol,", ", yCol,"))[,c('", colorCol, "', 'ggally_cor')]")
		# print(unique(txt))
		# cord <- unique(eval_ggpair(txt))		
		con <- textConnection(txt)
	  on.exit(close(con))
	  cord <- unique(eval(parse(con)))
	  
		# browser()
		cord$ggally_cor <- signif(as.numeric(cord$ggally_cor), 3)
		
		# put in correct order
		lev <- levels(data[[colorCol]])
		ord <- rep(-1, NROW(cord))
		for(i in 1:NROW(cord)) {
			for(j in seq_along(lev)){
				if(identical(as.character(cord[i, colorCol]), as.character(lev[j]))) {
					ord[i] <- j
				}
			}
		}
		cord <- cord[ord, ]
		
		cord$label <- str_c(cord[[colorCol]], ": ", cord$ggally_cor)
		
		txt <- str_c("Cor:", str_c(cord$label, collapse = "\n"), sep = "\n", collapse = "\n")
		
		
		
		
		
		print(cord)
		p <- ggally_text(
			label = "Cor: ",
			mapping,
			xP=0.5,
			yP=0.9,
			xrange = range(xVal),
			yrange = range(yVal),
			color = "black",
			size = corSize,
			...
		) +  
		theme_bw() + 
		opts(legend.position = "none")
		
		xPos <- rep(corAlignPercent, length(lev)) * diff(range(xVal)) + min(range(xVal))
		yPos <- seq(from = 0.9, to = 0.2, length.out = length(lev) + 1) * diff(range(yVal)) + min(range(yVal))
		yPos <- yPos[-1]
		# print(range(yVal))
		# print(yPos)
		cordf <- data.frame(xPos = xPos, yPos = yPos, labelp = cord$label)
				p <- p + geom_text(
						data=cordf,
						aes(
							x = xPos,
							y = yPos,
							label = labelp,
							color = labelp
						),
						hjust = 1,
						size = corSize,
						...
						
					)
		
		p$type <- "continuous"
		p$subType <- "cor"
		p
	} else {
		p <- ggally_text(
			label = paste(
				"Corr:\n",
				signif(
					cor(xVal,yVal),
					3
				),
				sep="",collapse=""
			),
			mapping,
			xP=0.5,
			yP=0.5,
			xrange = range(xVal),
			yrange = range(yVal),
			size = corSize,
			...
		) +  
		theme_bw() + 
		opts(legend.position = "none")

	  p$type <- "continuous"
	  p$subType <- "cor"
	  p		
	}
}
		

#' Plots the Box Plot
#' Make a box plot with a given data set
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... other arguments being supplied to geom_boxplot
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords hplot
#' @examples
#' ggally_box(iris, aes(x = Petal.Width, y = Species))
#' ggally_box(iris, aes_string(x = "Petal.Width", y = "Species"))
#' ggally_box(iris, aes_string(y = "Petal.Width", x = "Species", color = "Species"), outlier.colour = "red", outlier.shape = 13, outlier.size = 18)
ggally_box <- function(data, mapping, ...){
  ggally_dotAndBox(data, mapping, ..., boxPlot = TRUE)
}


#' Plots the Box Plot with Dot
#' Add jittering with the box plot
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... other arguments being supplied to geom_jitter
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords hplot
#' @examples
#' ggally_dot(iris, aes(x = Petal.Width, y = Species))
#' ggally_dot(iris, aes_string(x = "Petal.Width", y = "Species"))
#' ggally_dot(iris, aes_string(x = "Species", y = "Petal.Width", color = "Species"))
#' ggally_dot(iris, aes_string(x = "Species", y = "Petal.Width", color = "Species", shape = "Species")) + scale_shape(solid=FALSE)
ggally_dot <- function(data, mapping, ...){
  ggally_dotAndBox(data, mapping, ..., boxPlot = FALSE)
}


#' Plots either Box Plot or Dot Plots
#' Place box plots or dot plots on the graph
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... parameters passed to either geom_jitter or geom_boxplot
#' @param boxPlot boolean to decide to plot either box plots (TRUE) or dot plots (FALSE)
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords hplot
#' @examples
#' ggally_dotAndBox(iris, aes(x = Petal.Width, y = Species, color = Species), boxPlot=TRUE)
#' ggally_dotAndBox(iris, aes(x = Petal.Width, y = Species, color = Species), boxPlot=FALSE)
ggally_dotAndBox <- function(data, mapping, ..., boxPlot = TRUE){
  horizontal <-  is.factor(data[,as.character(mapping$y)])
  
  if(horizontal) {
#    cat("horizontal dot-box\n")
    mapping$tmp <- mapping$x
    mapping$x <- mapping$y
    mapping$y <- mapping$tmp
    mapping$tmp <- NULL
#    levels(data[,as.character(mapping$x)]) <- rev(levels(data[,as.character(mapping$x)]))
  }

	xVal <- as.character(mapping$x)
  yVal <- as.character(mapping$x)
  mapping$x <- 1

  p <- ggplot(data = data, mapping)
  

  if(boxPlot){
    p <- p + geom_boxplot(...)
    p$subType <- "box"
  } else {
    p <- p + geom_jitter(...)
    p$subType <- "dot"
  }
    

	if(!horizontal){
		p$facet$facets <- paste(". ~ ", yVal, sep = "")
	}else{
		p <- p + coord_flip() + opts(
				axis.text.y = theme_text(
					angle = 90, 
					vjust = 0, 
					colour = "grey50"
				)
			)
		p$facet$facets <- paste(yVal, " ~ .", sep = "")
	}		

  p <- p + scale_x_continuous(xVal, labels="", breaks=1)

  p$type <- "combo"
  p$horizontal <- horizontal
	p
}
# This is done with side by side and not facets
#ggally_dotAndBox <- function(data, mapping, ..., boxPlot = TRUE)
#{
#  horizontal <-  is.factor(data[,as.character(mapping$y)])
#  
#  if(horizontal) {
#    cat("horizontal dot-box\n")
#    mapping$tmp <- mapping$x
#    mapping$x <- mapping$y
#    mapping$y <- mapping$tmp
#    mapping$tmp <- NULL
#    levels(data[,as.character(mapping$x)]) <- rev(levels(data[,as.character(mapping$x)]))
#  }
#print(str(mapping))
#
#  p <- ggplot(data = data, mapping)
#  
#
#  if(boxPlot)
#    p <- p + geom_boxplot(...)
#  else
#    p <- p + geom_jitter(...)
#
#	if(horizontal){
#		p <- p + coord_flip() + opts(
#				axis.text.y = theme_text(
#					angle = 90, 
#					vjust = 0, 
#					colour = "grey50"
#				)
#			)
#	}
#
#	p
#}


#' Plots the Histograms by Faceting
#' Make histograms by displaying subsets of the data in different panels
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... parameters sent to stat_bin()
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords hplot
#' @examples
#' ggally_facethist(iris, aes(x = Petal.Width, y = Species))
#' ggally_facethist(iris, aes_string(x = "Species", y = "Petal.Width"), binwidth = 0.1)
ggally_facethist <- function(data, mapping, ...){
#  str(mapping)
	#aesString <- aes_string(mapping)
	#cat("\naesString\n");print(str(aesString))

  horizontal <-  is.factor(data[,as.character(mapping$y)])

	if(!horizontal){
	   mapping$tmp <- mapping$x
	   mapping$x <- mapping$y
	   mapping$y <- mapping$tmp
	} else {
	   # horizontal
	   # re-order levels to match all other plots
#	   levels(data[,as.character(mapping$y)]) <- rev(levels(data[,as.character(mapping$y)]))
	}	

#cat("Horizontal: ", horizontal, "\n")	
#cat("\nmapping\n");print(str(mapping))
#cat("\ndata\n");print(head(data))
	
	
	
  xVal <- mapping$x
	yVal <- mapping$y
  mapping$y <- NULL
#str(mapping)
#str(xVal)
#str(yVal)

	p <- ggplot(data = data, mapping)
	mapping$x <- NULL
	p <- p + stat_bin(...) 

	if(horizontal){
		p$facet$facets <- paste(as.character(yVal), " ~ .", sep = "")
	} else {
		p <- p + coord_flip() 
		p$facet$facets <- paste(". ~ ", as.character(yVal), sep = "")
	}		
	p <- p + scale_y_continuous(as.character(yVal)) + scale_x_continuous(as.character(xVal))  

  p$type <- "combo"
  p$subType <- "facethist"
  p$horizontal <- horizontal

	p
}


#' Plots the density plots by faceting
#' Make density plots by displaying subsets of the data in different panels
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... other arguments being sent to stat_density
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords hplot
#' @examples
#' ggally_facetdensity(iris, aes(x = Petal.Width, y = Species))
#' ggally_facetdensity(iris, aes_string(x = "Species", y = "Petal.Width", color = "Species"))
ggally_facetdensity <- function(data, mapping, ...){
  ggally_facetdensitystrip(data, mapping, ..., den_strip = FALSE)
}


#' Plots a tile plot with facets
#' Make Tile Plot as densely as possible
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... other arguments being sent to stat_bin
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords hplot
#' @examples
#' ggally_denstrip(iris, aes(x = Petal.Width, y = Species, color = Species))
#' ggally_denstrip(iris, aes_string(x = "Petal.Width", y = "Species"))
#' ggally_denstrip(iris, aes_string(x = "Species", y = "Petal.Width", binwidth = "0.2")) + scale_fill_gradient(low = "grey80", high = "black")
ggally_denstrip <- function(data,mapping, ...){
  ggally_facetdensitystrip(data, mapping, ..., den_strip = TRUE)
}



#' Plots a density plot with facets or a tile plot with facets
#' Make Tile Plot as densely as possible
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... other arguments being sent to either stat_bin or stat_density
#' @param den_strip boolean to deceide whether or not to plot a density strip(TRUE) or a facet density(FALSE) plot.
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords hplot
#' @examples
#' example(ggally_facetdensity)
#' example(ggally_denstrip)
ggally_facetdensitystrip <- function(data, mapping, ..., den_strip = FALSE){
  horizontal <-  is.factor(data[,as.character(mapping$y)])
  if(!horizontal) {
    mapping$tmp <- mapping$x
    mapping$x <- mapping$y
    mapping$y <- mapping$tmp
    mapping$tmp <- NULL

  } else {
    # horizontal
    # re-order levels to match all other plots
#    cat("horizontal facet-density-string\n")
#    levels(data[,as.character(mapping$y)]) <- rev(levels(data[,as.character(mapping$y)]))
  }

  xVal <- mapping$x
	yVal <- mapping$y
  mapping$y <- NULL

	p <- ggplot(data = data, mapping) + labs(x = xVal, y = yVal)
		    
	if(identical(den_strip, TRUE)){
	 # print("Density Strip")	  
		p <- p +    
  		stat_bin(
  		  aes(
    		  y = 1,
  		    fill = ..density..
  		  ), 
  		  position = "identity", 
  		  geom = "tile",
  		  ...
  		)	
    p$subType <- "denstrip"
	} else {
		p <- p +   
  		stat_density(
		  aes(
  		    y = ..scaled.. * diff(range(x)) + min(x)
  		  ), 
  		  position = "identity", 
  		  geom = "line",
  		  ...
  		)
    p$subType <- "facetdensity"
	}
    		
		
	if(horizontal){
		p$facet$facets <- paste(as.character(yVal), " ~ .", sep = "")
		
		if(identical(den_strip, TRUE))
		  p <- p + opts(axis.text.y = theme_blank())
	} else {
		p <- p + coord_flip()
		p$facet$facets <- paste(". ~ ", as.character(yVal), sep = "")
		
		if(identical(den_strip, TRUE))
      p <- p + opts(axis.text.x = theme_blank())
	}		
  p$type <- "combo"
  p$horizontal <- horizontal

	p
}


#' Plots a mosaic plots
#' Plots the mosaic plot by using fluctuation
#'
#' Must send only two discrete columns in the data set.
#'
#' @param data data set using
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords hplot
#' @examples
#' ggally_ratio(movies[,c("mpaa","Action")])
#' ggally_ratio(movies[,c("mpaa","Action")]) + coord_equal()
#' ggally_ratio(movies[,c("Action","mpaa")]) + opts(aspect.ratio = (length(levels(movies[,"mpaa"])) ) / (length(levels(as.factor(movies[,"Action"]))) ) )
ggally_ratio <- function(data){
	dataNames <- colnames(data)
	data <- data[, 2:1]
	tmpData <- table(data)
	tmpData <- tmpData[rev(seq_len(nrow(tmpData))),]
	tmpData <- as.table(tmpData)
	p <- ggfluctuation2(tmpData)# + labs(x = dataNames[1], y = dataNames[2])
  p$type <- "discrete"
  p$subType <- "ratio"
  p
}


#' Plots the Density Plots by Using Diagonal
#' Plots the density plots by using Diagonal
#'
#' @param data data set using
#' @param mapping aesthetics being used.
#' @param ... other arguments sent to stat_density
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords hplot
#' @examples
#' ggally_densityDiag(iris, aes(x = Petal.Width))
#' ggally_densityDiag(movies, aes_string(x="rating"))
#' ggally_densityDiag(movies, aes_string(x="rating", color = "mpaa"))
ggally_densityDiag <- function(data, mapping, ...){

	p <- ggplot(data, mapping) + 
		scale_x_continuous() + 
		scale_y_continuous() + 
		stat_density(
			aes(
				y = ..scaled.. * diff(range(x)) + min(x)
			),
			position = "identity", 
			geom = "line",
			...
		)
  p$type <- "diag"
  p$subType <- "density"
  p

}

#' Plots the Bar Plots by Using Diagonal
#' Plots the bar plots by using Diagonal
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... other arguements are sent to geom_bar
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords hplot
#' @examples
#' ggally_barDiag(movies, aes(x = mpaa))
#' ggally_barDiag(movies, aes_string(x = "mpaa"))
#' ggally_barDiag(movies, aes_string(x ="rating", binwidth = ".1"))
ggally_barDiag <- function(data, mapping, ...){
	mapping$y <- NULL
	numer <- is.null(attributes(data[,as.character(mapping$x)])$class)

  p <- ggplot(data = data, mapping) + geom_bar(...)
	
	if(numer){
		# message("is numeric")
    p$subType <- "bar_num"
 	} else {
		# message("is categorical")
		# xVal <- mapping$x
		# mapping <- addAndOverwriteAes(mapping, aes(x = 1L))
		# # p <- ggplot(m, mapping) + geom_bar(aes(weight = Freq), binwidth = 1, ...)
		# p <- ggplot(data, mapping) + geom_bar(...)
		# # p <- p + scale_x_continuous(NULL, labels ="",breaks = 1)

		# xVal <- mapping$x
		# mapping <- addAndOverwriteAes(mapping, aes(x = 1L))
		# mapping <- addAndOverwriteAes(mapping, aes_string(weight = xVal))
		# mapping <- addAndOverwriteAes(mapping, aes_string(weight = xVal))
    # p <- ggplot(data = data, mapping) + geom_bar(...)
		# p$facet$facets <- paste(". ~ ", as.character(xVal), sep = "")
    p$subType <- "bar_cat"
	}
  p$type <- "diag"
	p
}



#' GGplot Text
#' Plot text for a plot
#'
#' @param label text that you want to appear
#' @param mapping aesthetics that don't relate to position (such as color)
#' @param xP horizontal position percentage
#' @param yP vertical position percentage
#' @param xrange range of the data around it.  Only nice to have if plotting in a matrix
#' @param yrange range of the data around it.  Only nice to have if plotting in a matrix
#' @param ... other arguments for geom_text
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords hplot
#' @examples
#' ggally_text("Example 1")
#' ggally_text("Example\nTwo", mapping = aes_string(size = 15, color = "red"))
ggally_text <- function(
  label, 
  mapping = aes(color = "black"),
  xP = 0.5, 
  yP = 0.5,
  xrange = c(0,1), 
  yrange = c(0,1),
  ...
){

  rectData <- data.frame(
		x1 = xrange[1], 
		x2 = xrange[2], 
		y1 = yrange[1], 
		y2 = yrange[2]
	)
	# print(rectData)
	
	p <- ggplot() + xlim(xrange) + ylim(yrange) + 
		geom_rect(data = rectData,
				aes(
					xmin=x1,
					xmax = x2,
					ymin = y1,
					ymax = y2
				),
				fill= I("white")) + 
				labs(x = NULL, y = NULL)

	new_mapping <- aes_string(x = xP * diff(xrange) + min(xrange), y = yP * diff(yrange) + min(yrange))
	if(is.null(mapping)) {
		mapping <- new_mapping
	} else {
		mapping <- addAndOverwriteAes(mapping, new_mapping)
	}
	colour <- as.character(mapping$colour)
  if(is.null(colour) || length(colour) < 1)
    colour <- "black" 

  # remove colour from the aesthetics
	mapping$colour <- NULL
  
	p <- p + 
	   geom_text( label = label, mapping = mapping, colour = colour, ...) + 
	   opts(legend.position = "none")
	
	p

}


#' Plots the Bar Plots Faceted by Conditional Variable
#'
#' X variables are plotted using \code{geom_bar} and faceted by the Y variable.
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param ... other arguements are sent to geom_bar
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords hplot
#' @examples
#' ggally_facetbar(tips, aes(x = sex, y = smoker, fill = time))
#' ggally_facetbar(tips, aes(x = smoker, y = sex, fill = time))
ggally_facetbar <- function(data, mapping, ...){

	numer <- is.null(attributes(data[,as.character(mapping$x)])$class)
	xVal <- mapping$x
	yVal <- mapping$y
	mapping$y <- NULL
	p <- ggplot(data, mapping) + geom_bar(...)
	p$facet$facets <- paste(as.character(yVal), " ~ .", sep = "")
	p$subType <- "facetbar"
	p$type <- "diag"

	p
}



#' Fluctuation plot
#' Create a fluctuation plot.
#'
#' A fluctutation diagram is a graphical representation of a contingency table. This fuction currently only supports 2D contingency tables.
#' The function was adopted from experiemntal functions within GGplot2 developed by Hadley Wickham.
#'
#' @param table_data a table of values, or a data frame with three columns, the last column being frequency
#' @param floor don't display cells smaller than this value
#' @param ceiling max value to compare to
#' @author Hadley Wickham \email{h.wickham@@gmail.com}, Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords hplot
#' @examples
#' ggfluctuation2(table(movies$Action, movies$Comedy))
#' ggfluctuation2(table(movies$Action, movies$mpaa))
#' ggfluctuation2(table(movies[,c("Action", "mpaa")]))
#' ggfluctuation2(table(warpbreaks$breaks, warpbreaks$tension))
ggfluctuation2 <- function (table_data, floor = 0, ceiling = max(table_data$freq, na.rm = TRUE)) {

	yNames <- rownames(table_data)
	xNames <- colnames(table_data)
	oldnames <- rev(names(dimnames(table_data)))

	if (is.table(table_data)) 
		table_data <- as.data.frame(t(table_data))

	if(all(oldnames == ""))	
		oldnames <- c("X","Y")		
		

	names(table_data) <- c("x", "y", "result")
	table_data <- add.all.combinations(table_data, list("x", "y"))
	table_data <- transform(table_data, x = as.factor(x), y = as.factor(y), 
		freq = result)

	table_data <- transform(table_data, freq = sqrt(pmin(freq * .95, ceiling)/ceiling), 
		border = ifelse(is.na(freq), "grey90", ifelse(freq > 
			ceiling, "grey30", "grey50")))
	table_data[is.na(table_data$freq), "freq"] <- 1
	table_data <- subset(table_data, freq * ceiling >= floor)
	
	xNew <- as.numeric(table_data$x) + 1/2 * table_data$freq
	yNew <- as.numeric(table_data$y) + 1/2 * table_data$freq
	
	maxLen <- max(diff(range(as.numeric(table_data$x))), diff(range(as.numeric(table_data$y))) )
	

	table_data <- cbind(table_data, xNew, yNew)
	#print(table_data)
	#print(xNames)
	#print(yNames)
	
	#cat("\nmaxLen");print(maxLen)

	
	p <- ggplot(
			table_data, 
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
				hjust = 0, 
				vjust = 0,
				angle = 90,
				colour = "grey50"
			)
		)
  p$x_names <- xNames
  p$y_names <- yNames
	
	p
}


#' Blank
#' Drawing nothing
#' 
#' Makes a "blank" ggplot object that will only draw white space
#'
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @param ... other arguments ignored
#' @keywords hplot
ggally_blank <- function(...){
  ignored <- aes(...)
	a <- as.data.frame(cbind(1:2,1:2))
	colnames(a) <- c("X","Y")
	
	p <- ggplot(data = a, aes(x = X, y = Y)) + geom_point( colour = "transparent") + 
  	opts(
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
  p$subType <- p$type <- "blank"
  p
}
