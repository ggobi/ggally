plot_types <- function(data)
{
	namesD <- names(data)
	dataInfo <- NULL
	for(i in 1:ncol(data))
		for(j in 1:ncol(data))
			dataInfo <- rbind(dataInfo, c(find_plot_type(data,i,j),namesD[i],namesD[j]))
	dataInfo <- as.data.frame(dataInfo)
	colnames(dataInfo) <- c("Type", "xvar", "yvar")
	dataInfo
}


get_select_data <- function(allData, colMatrix)
{
	if(nrow(colMatrix) < 1) return(NULL)
	dataTmp <- NULL
	for(i in 1:nrow(colMatrix))
	{
		print(colMatrix[i,])
		e1 <- allData[,"yvar"] == colMatrix[i,"yvar"]
		e2 <- allData[,"xvar"] == colMatrix[i,"xvar"]
		dataTmp <- rbind(dataTmp,allData[e1 & e2,])
	}
	
	dataTmp

}


find_plot_type <- function(data,col1,col2)
{
	if(col1 == col2)
		return("stat_bin")

	y1Type <- "numeric"
	y2Type <- "numeric"
	
	if(!is.null(attributes(data[,col1])))
		y1Type <- "category"
	if(!is.null(attributes(data[,col2])))
		y2Type <- "category"
	
	
	
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




