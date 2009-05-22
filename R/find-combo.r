plot_types <- function(data)
{
	namesD <- names(data)
	dataInfo <- NULL
	
	#horizontal then vertical
	for(j in 1:ncol(data))
		for(i in 1:ncol(data))
			dataInfo <- rbind(dataInfo, c(find_plot_type(data,i,j),namesD[i],namesD[j],i,j))
	dataInfo <- as.data.frame(dataInfo)
	colnames(dataInfo) <- c("Type", "xvar", "yvar","posx","posy")
	dataInfo
}


get_select_data <- function(allData, gridPos)
{
	if(nrow(gridPos) < 1) return(NULL)
	
	print(gridPos)
	
	print(allData)
	
	dataTmp <- allData[,c(gridPos)]
	
	colnames(dataTmp) <- c("x","y")
	

}


find_plot_type <- function(data,col1,col2)
{

	y1Type <- "numeric"
	y2Type <- "numeric"
	
	if(!is.null(attributes(data[,col1])))
		y1Type <- "category"
	if(!is.null(attributes(data[,col2])))
		y2Type <- "category"
	
	if(col1 == col2)
	{
		if(y1Type == "numeric")
			return("stat_bin-num")
		else
			return("stat_bin-cat")
	}
	
	return(get_plot_type(y1Type,y2Type))
}

get_plot_type <- function(y1_type,y2_type)
{
	cats <- c(y1_type, y2_type) %in% "category"
	if(TRUE %in% cats)
	{
		if(all(cats))
			return("mosaic")
		if(cats[1])
			return("box-vert")
		return("box-hori")
	}
	else
	{
		return("scatterplot")
	}	
}




