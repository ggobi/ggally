#' Plot Types
#' Retrieves the type of plot that should be used for all combinations
#' 
#' @param data data set to be used
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
plot_types <- function(data)
{
	namesD <- names(data)
	dataInfo <- array("", c(ncol(data)^2,5))
	
	#horizontal then vertical
	for(j in 1:ncol(data))
		for(i in 1:ncol(data))
			dataInfo[(i-1)*ncol(data)+j,] <- c(find_plot_type(data,i,j),namesD[j],namesD[i],j,i)
			
	dataInfo <- as.data.frame(dataInfo)
	colnames(dataInfo) <- c("Type", "xvar", "yvar","posx","posy")
	dataInfo
}



#' Find Plot Types
#' Retrieves the type of plot for the specific columns
#' 
#' @param data data set to be used
#' @param col1 x column
#' @param col2 y column
#' @keywords internal
#' @author Barret Schloerke \email{schloerke@@gmail.com}
find_plot_type <- function(data,col1,col2)
{

	y1Type <- "numeric"
	y2Type <- "numeric"
	
	if(!is.null(attributes(data[,col1])) || all(is.character(data[,col1])))
		y1Type <- "category"
	if(!is.null(attributes(data[,col2])) || all(is.character(data[,col2])))
		y2Type <- "category"
	
	if(col1 == col2)
	{
		if(y1Type == "numeric")
			return("stat_bin-num")
		else
			return("stat_bin-cat")
	}
	
	#cat(names(data)[col2],": ", y2Type,"\t",names(data)[col1],": ",y1Type,"\n")
	return(get_plot_type(y1Type,y2Type))
}

#' Get Plot Type
#' Retrieves the type of plot for specific info
#' 
#' @param y1_type x type.  Either numeric or category
#' @param y2_type y type.  Either numeric or category
#' @keywords internal
#' @author Barret Schloerke \email{schloerke@@gmail.com}
get_plot_type <- function(y1_type,y2_type)
{
	cats <- c(y1_type, y2_type) %in% "category"
	if(TRUE %in% cats)
	{
		if(all(cats))
			return("mosaic")
		if(cats[1])
			return("box-hori")
		return("box-vert")
	}

	return("scatterplot")
}