###################
ggplot_points <- function(data, ...)
{
	ggplot(data = data, aes_string(...)) + geom_point()
}


###################
ggplot_smooth <- function(data, ...)
{
	ggplot(data = data, aes_string(...)) +
		geom_smooth(method="lm", colour = I("black")) +
		geom_point() 
}


###################
ggplot_density <- function(data, ..., filled = FALSE)
{
  p <- ggplot(data = data, aes_string(...))
	if(filled)
		p <- p + stat_density2d(aes(fill = ..level..), geom="polygon")
	else
	  p <- p + geom_density2d(, colour = I("black"))
	
	p
}


###################
ggplot_box <- function(data, ...)
{
  ggplot_dotAndBox(data, ..., boxPlot = TRUE)
}


###################
ggplot_dot <- function(data, ...)
{
  ggplot_dotAndBox(data, ..., boxPlot = FALSE)
}



###################
ggplot_dotAndBox <- function(data, ..., boxPlot = TRUE)
{
	aesString <- aes_string(...)
  horizontal <-  is.factor(data[,as.character(aesString$y)])
  
  if(horizontal)
  {
    aesString$tmp <- aesString$x
    aesString$x <- aesString$y
    aesString$y <- aesString$tmp
    aesString$tmp <- NULL
#print(aesString)
  } 

  p <- ggplot(data = data, aesString)

  if(boxPlot)
  {
    p <- p + geom_boxplot()
  }
  else
  {
    p <- p + geom_jitter()
  }

	if(horizontal)
	{
		p <- p + coord_flip() + opts(
				axis.text.y = theme_text(
					angle = 90, 
					vjust = 0, 
					colour = "grey50"
				)
			)
	}
	p
}


###################
ggplot_facethist <- function(data, ...)
{
	aesString <- aes_string(...)
  horizontal <-  is.factor(data[,as.character(aesString$y)])
  
  if(horizontal)
  {
    aesString$tmp <- aesString$x
    aesString$x <- aesString$y
    yVal <- aesString$tmp
    aesString$y <- NULL
    aesString$tmp <- NULL
print(aesString)
  } 
  else
  {
    yVal <- aesString$y
    aesString$y <- NULL
  }
	
	p <- ggplot( data = data, aesString)
	p <- p + stat_bin(aes(y = ..count..) )
	p <- p + scale_y_continuous(as.character(yVal))
	p <- p + facet_grid(aes_string(as.character(yVal)) ~ .) 
	
	if(horizontal)
    p <- p + coord_flip()
	
	p
}


###################
ggplot_facethist <- function(data, ...)
{
	aesString <- aes_string(...)
  horizontal <-  is.factor(data[,as.character(aesString$y)])

  xVal <- aesString$x
	yVal <- aesString$y
  aesString$y <- NULL
  str(aesString)

		p <- ggplot(data = data, aesString)
		p <- p + stat_bin() 
		p <- p + scale_y_continuous(as.character(yVal))
		if(horizontal)
		{
  		p$facet$facets <- paste(as.character(yVal), " ~ .", sep = "")
		}
		else
		{
  		p$facet$facets <- paste(". ~ ", as.character(yVal), sep = "")
  		p <- p + coord_flip()
		}		
		p
}


###################
ggplot_facetdensity <- function(data, ...)
{
  ggplot_facedensitystrip(data, ..., den_strip = FALSE)
}

###################
ggplot_denstrip <- function(data,...)
{
  ggplot_facedensitystrip(data, ..., den_strip = TRUE)
}

###################
ggplot_facetdensitystrip <- function(data, ..., den_strip = FALSE)
{
	aesString <- aes_string(...)
  horizontal <-  is.factor(data[,as.character(aesString$y)])
  if(!horizontal)
  {
    aesString$tmp <- aesString$x
    aesString$x <- aesString$y
    aesString$y <- aesString$tmp
    aesString$tmp <- NULL
  } 

  xVal <- aesString$x
	yVal <- aesString$y
  aesString$y <- NULL
str(aesString)
		p <- ggplot(data = data, aesString) + 
		    scale_y_continuous(as.character(yVal)) + 
		    scale_x_continuous(as.character(xVal)) +  
		    ylim(range(data[,as.character(xVal)]))
		    
		if(den_strip)
		{
  		p <- p +    
    		stat_bin(
    		  aes(
      		  y = 1,
    		    fill = ..density..
    		  ), 
    		  position = "identity", 
    		  geom = "tile"
    		)		  
		}
		else
		{
  		p <- p +   
    		stat_density(
    		  aes(
    		    y = ..scaled.. * diff(range(x)) + min(x)
    		  ), 
    		  position = "identity", 
    		  geom = "line"
    		)
		}
    		
		
		if(horizontal)
		{
		  print("horizontal")
		  p + facet_grid(Species ~ .)
  		p$facet$facets <- paste(as.character(yVal), " ~ .", sep = "")
  		if(den_strip)
  		  p <- p + opts(axis.text.y = theme_blank()) + ylim(0,1)
		}
		else
		{
  		p <- p + coord_flip()
  		p$facet$facets <- paste(". ~ ", as.character(yVal), sep = "")
  		if(den_strip)
        p <- p + opts(axis.text.x = theme_blank())  + ylim(.5,1.5)

		}		
		p
}


###################
ggplot_densityI <- function(data, ...)
{
	aesString <- aes_string(...)
	aesString$y <- NULL

	ggplot(data, aesString) + 
		scale_x_continuous() + 
		scale_y_continuous() + 
		stat_density(
			aes(
				y = ..scaled.. * diff(range(x)) + min(x)
			),
			position = "identity", 
			geom = "line"
		)+
		ylim(range(data[,as.character(aesString$x)]))
}



##################
ggplot_bar <- function(data, ...)
{
	aesString <- aes_string(...)
	aesString$y <- NULL

	numer <- is.null(attributes(data[,as.character(aesString$x)])$class)
	
	if(numer)
	{
    p <- ggplot(data = data, aesString) + geom_bar()
 	}
	else
	{
	 ## create a temporary data set that computes the count manually
		dataTmp <- as.factor(data[,as.character(aesString$x)])
		count <- rep(0, length(levels(dataTmp)))
		for(z in 1:length(levels(dataTmp)))
			count[z] <- length(dataTmp[dataTmp==levels(dataTmp)[z]])
		
		dataTmp <- cbind(levels(dataTmp), count)
		dataTmp <- as.data.frame(dataTmp)
		
		colnames(dataTmp) <- c(as.character(aesString$x), "Count")
		
		## Makes sure the count is numeric instead of factor
		dataTmp$Count <- as.numeric(as.character(dataTmp$Count))
		
#print(head(dataTmp))
#str(dataTmp)
		
		p <- ggplot(data = dataTmp, aesString) + geom_bar(aes(y = Count), stat="identity") +
			coord_flip() +  
			opts(
				axis.text.y = theme_text(
					angle = 90, 
					vjust = 0, 
					colour = "grey50"
				)
			)
	}	
	p
}
