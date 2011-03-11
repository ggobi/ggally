# 1. Scaling
#    - Options:
#      - Univariate Min/Max: Define the min and max for each variable to be
#          zero and scale accordingly
#      - Global Min/Max: Define the global min and max to be zero and scale
#          accordingly
#      - Summary centering: Use Univariate Min/Max method to standardize vert.
#          height, then center the "bars" at some specified value
#          - "value" ideas: median, mean, constant
#          - alternative idea: center at one observation
#      - Standardize, set ylim()
#          - mean +/- k*sd
#          - median +/- k*IQR
# 2. Missing Values
#    - Options:
#      - Exclude
#      - Imputation
#        - set to -10% range of min value
#        - mean, median, random
# 3. Display
#    - Lines
#      - All lines
#      - Subset of lines
#        - Random set, user-specified set
#      - Alpha-scaled lines
#    - Aesthetics
#      - Point types, line types
#        - one interesting point type to use is a flat bar; called barcode 
#          plot
#          - for categorical variables, make flat bar thicker for higher n
#      - Probably pass mapping, params to ggplot to allow flexibility
#    - Underlay univariate distributions
#      - boxplots
#    - Reference decorations
#      - lines
#        - @ center
#        - @ min/max
#      - grey box that extends from univariate min to univariate max
# 4. Ordering of Variables
#    - many different options...will discuss later

# levels of scale:
#   - "std": subtract mean, divide by sd
#   - "robust": subtract median, divide by median absolute deviation
#   - "uniminmax": define min of variable to be zero, max to be one
#   - "globalminmax": define global min to be zero, global max to be one
#   - "center": use minmax method to standardize vertical height, then
#     center each variable at a value specified by the scaleSummary param
#   - "centerObs": use minmax method to standardize, then center each variable
#     at the value of the observation specified by centerObsID

# levels of missing:
#   - "exclude": remove all rows with missing values
#   - "mean": set missing values to the mean of the variable
#   - "median": set missing values to the median of the variable
#   - "min10": set missing values to 10% below the minimum of the variable
#   - "random": set missing values to value of randomly chosen observation
#     on that variable

#' ggparcoord - A ggplot2 Parallel Coordinate Plot
#' ; A function for plotting static parallel coordinate plots, utilizing
#' the \code{ggplot2} graphics package.
#'
#' \code{scale} is a character string that denotes how to scale the variables
#' in the parallel coordinate plot. Options:
#' \itemize{
#'   \item{\code{std}}{: univariately, subtract mean and divide by standard deviation}
#'   \item{\code{robust}}{: univariately, subtract median and divide by median absolute deviation}
#'   \item{\code{uniminmax}}{: univariately, scale so the minimum of the variable is zero, and the maximum is one}
#'   \item{\code{globalminmax}}{: scale so the global minimum is zero, and the global maximum is one}
#'   \item{\code{center}}{: use \code{uniminmax} to standardize vertical height, then
#'     center each variable at a value specified by the \code{scaleSummary} param}
#'   \item{\code{centerObs}}{: use \code{uniminmax} to standardize vertical height, then
#'     center each variable at the value of the observation specified by the \code{centerObsID} param}
#' }
#'
#' \code{missing} is a character string that denotes how to handle missing
#'   missing values. Options:
#' \itemize{
#'   \item{\code{exclude}}{: remove all cases with missing values}
#'   \item{\code{mean}}{: set missing values to the mean of the variable}
#'   \item{\code{median}}{: set missing values to the median of the variable}
#'   \item{\code{min10}}{: set missing values to 10\% below the minimum of the variable}
#'   \item{\code{random}}{: set missing values to value of randomly chosen observation
#'     on that variable}
#' }
#'
#' \code{order} is either a vector of indices or a character string that denotes how to 
#'   order the axes (variables) of the parallel coordinate plot. Options:
#' \itemize{
#'   \item{\code{(default)}}{: order by the vector denoted by \code{columns}}
#'   \item{\code{(given vector)}}{: order by the vector specified}
#'   \item{\code{Outlying}}{: order by the scagnostic measure, Outlying, as calculated
#'     by the package \code{scagnostics}. Other scagnostic measures available to order
#'     by are \code{Skewed, Clumpy, Sparse, Striated, Convex, Skinny, Stringy,} and
#'     \code{Monotonic}.}
#' }
#'
#' @param data the dataset to plot
#' @param columns a vector of variables (either names or indices) to be axes in the plot
#' @param groupColumn a single variable to group (color) by
#' @param scale method used to scale the variables (see Details)
#' @param scaleSummary if scale=="summary", summary statistic to univariately
#'   center each variable by
#' @param centerObsID if scale=="centerObs", row number of case plot should
#'   univariately be centered on
#' @param missing method used to handle missing values (see Details)
#' @param order method used to order the axes (see Details)
#' @param showPoints logical operator indicating whether points should be
#'   plotted or not
#' @param alphaLines value of alpha scaler for the lines of the parcoord plot
#' @param boxplot logical operator indicating whether or not boxplots should
#'   underlay the distribution of each variable
#' @param shadeBox color of underlaying box which extends from the min to the
#'   max for each variable (no box is plotted if shadeBox == NULL)
#' @param mapping aes string to pass to ggplot object
#' @param title character string denoting the title of the plot
#' @author Jason Crowley \email{crowley.jason.s@@gmail.com}, Barret Schloerke \email{schloerke@@gmail.com}, Di Cook \email{dicook@@iastate.edu}, Heike Hofmann \email{hofmann@@iastate.edu}, Hadley Wickham \email{h.wickham@@gmail.com}
#' @return ggplot object that if called, will print
#' @examples
#' # basic parallel coordinate plot, using defaults
#' ggparcoord(iris,1:4,5)
#'
#' # underlay univariate boxplots, add title, use uniminmax scaling
#' ggparcoord(iris,1:4,5,scale="uniminmax",boxplot=TRUE,title="Parallel Coord. Plot of Iris Data")
#'
#' # utilize ggplot2 aes to switch to dashed lines
#' ggparcoord(iris,1:4,5,boxplot=TRUE,title="Parallel Coord. Plot of Iris Data",
#'   mapping=aes_string(lty=2))


ggparcoord <- function(
  data,
  columns,
  groupColumn,
  scale="std",
  scaleSummary="mean",
  centerObsID=1,
  missing="exclude",
  order=columns,
  showPoints=FALSE,
  alphaLines=1,
  boxplot=FALSE,
  shadeBox=NULL,
  mapping=NULL,
  title=""
) {
  require(ggplot2)  

  if(is.numeric(groupColumn)) {
    groupCol <- names(data)[groupColumn]
  } else 
    groupCol <- groupColumn
  groupVar <- data[,groupCol]
  data <- data[,columns]

  # Change character vars to factors
  vartypes <- get.VarTypes(data)
  char.vars <- names(vartypes)[vartypes == "character"]
  if(length(char.vars) >= 1) {
    for(i in 1:length(char.vars)) {
      data[,char.vars[i]] <- factor(data[,char.vars[i]])
    }
  }

  # Change factors to numeric
  vartypes <- get.VarTypes(data)
  fact.vars <- names(vartypes)[vartypes == "factor"]
  if(length(fact.vars) >= 1) {
    for(i in 1:length(fact.vars)) {
      data[,fact.vars[i]] <- as.numeric(data[,fact.vars[i]])
    }
  }

  data$.ID <- as.factor(1:dim(data)[1])
  data$anyMissing <- apply(data,1,function(x) { any(is.na(x)) })
  p <- c(dim(data)[2]-1,dim(data)[2])

  ### Scaling ###
  # will need to implement error checking for the scale and other args
  # tried writing a switch statement to reduce code length and perhaps be
  #   be more efficient, but it doesn't appear to accept multiple lines for
  #   an argument

  if(tolower(scale) == "std") {
    data <- rescaler(data)
  }
  else if(tolower(scale) == "robust") {
    data <- rescaler(data,type="robust")
  }
  else if(tolower(scale) == "uniminmax") {
    data <- rescaler(data,type="range")
  }
  else if(tolower(scale) == "globalminmax") {
    data[,-p] <- data[,-p] - min(data[,-p])
    data[,-p] <- data[,-p]/max(data[,-p])
  }
  else if(tolower(scale) == "center") {
    data <- rescaler(data,type="range")
    data[,-p] <- apply(data[,-p],2,function(x){
      x <- x - eval(parse(text=paste(scaleSummary,"(x,na.rm=TRUE)",sep="")))
    })
  }

  ### Imputation ###
  if(tolower(missing) == "exclude") {
    groupVar <- groupVar[complete.cases(data)]
    data <- data[complete.cases(data),]
  }
  else if(tolower(missing) == "mean") {
     data[,-p] <- apply(data[,-p],2,function(x) {
      if(any(is.na(x))) x[is.na(x)] <- mean(x,na.rm=TRUE)
      return(x)
    })
  }
  else if(tolower(missing) == "median") {
     data[,-p] <- apply(data[,-p],2,function(x) {
      if(any(is.na(x))) x[is.na(x)] <- median(x,na.rm=TRUE)
      return(x)
    })
  }
  else if(tolower(missing) == "min10") {
     data[,-p] <- apply(data[,-p],2,function(x) {
      if(any(is.na(x))) x[is.na(x)] <- .9*min(x,na.rm=TRUE)
      return(x)
    })
  }
  else if(tolower(missing) == "random") {
     data[,-p] <- apply(data[,-p],2,function(x) {
      if(any(is.na(x))) {
        num <- sum(is.na(x))
        idx <- sample(which(!is.na(x)),num,replace=TRUE)
        x[is.na(x)] <- x[idx]
      }
      return(x)
    })
  }

  ### Scaling (round 2) ###
  # Centering by observation needs to be done after handling missing values
  #   in case the observation to be centered on has missing values
  if(tolower(scale) == "centerobs") {
    data <- rescaler(data,type="range")
    data[,-p] <- apply(data[,-p],2,function(x){
      x <- x - x[centerObsID]
    })
  }

  data <- cbind(data,groupVar)
  names(data)[dim(data)[2]] <- groupCol

  data.m <- melt(data,id.vars=c(groupCol,".ID","anyMissing"))
  
  ### Ordering ###
  if(length(order) > 1) {
     if(is.numeric(order)) {
       data.m$variable <- factor(data.m$variable,levels=names(data)[order])
     } else {
       data.m$variable <- factor(data.m$variable,levels=order)
     }
  }
  else {
    require(scagnostics)
    scag <- scagnostics(data[,1:(dim(data)[2]-3)])
    data.m$variable <- factor(data.m$variable,levels=scagOrder(scag,names(data[,1:(dim(data)[2]-3)]),order))
  }

  mapcall <- paste("aes_string(x='variable',y='value',group='.ID',colour='",groupCol,"')",sep="")
  mapping2 <- eval(parse(text=mapcall))
  mapping2 <- addAndOverwriteAes(mapping2,mapping)
  p <- ggplot(data=data.m,mapping=mapping2)

  if(!is.null(shadeBox)) {
    # Fix so that if missing = "min10", the box only goes down to the true min
    d.sum <- ddply(data.m,.(variable),summarize,
      min = min(value),
      max = max(value))
    p <- p + geom_linerange(data=d.sum,size=I(10),col=shadeBox,
      mapping=aes(y=NULL,ymin=min,ymax=max,group=variable))
  }
  if(boxplot) p <- p + geom_boxplot(mapping=aes(group=variable),alpha=0.8)
  if(showPoints) p <- p + geom_point()

  p + geom_line(alpha=alphaLines) + opts(title=title)

}

#' Get vector of variable types from data frame
#'
#' @param df data frame to extract variable types from
#' @author Jason Crowley \email{crowley.jason.s@@gmail.com}
#' @return character vector with variable types, with names corresponding to
#'   the variable names from df
get.VarTypes <- function(df) {
  return(unlist(lapply(unclass(df),class)))
}

#' Find order of variables based on a specified scagnostic measure
#' by maximizing the index values of that measure along the path.
#'
#' @param scag \code{scagnostics} object
#' @param vars character vector of the variables to be ordered
#' @param measure scagnostics measure to order according to
#' @author Jason Crowley \email{crowley.jason.s@@gmail.com}
#' @return character vector of variable ordered according to the given 
#'   scagnostic measure
scagOrder <- function(scag, vars, measure) {
  p <- length(vars)
  scag <- sort(scag[measure,],decreasing=TRUE)
  d.scag <- data.frame(var1=NA,var2=NA,val=scag)
  for (i in 1:dim(d.scag)[1]) {
    d.scag$var1[i] <- substr(names(scag)[i],1,regexpr(" * ",names(scag)[i])-1)
    d.scag$var2[i] <- substr(names(scag)[i],regexpr(" * ",names(scag)[i])+3,nchar(names(scag)[i]))
  }  
  a <- c(d.scag$var1[1],d.scag$var2[1])
  d.scag <- d.scag[-1,]
  a <- a[order(c(min(grep(a[1],rownames(d.scag))),min(grep(a[2],rownames(d.scag)))),
    decreasing=TRUE)]
  d.scag <- d.scag[-grep(a[1],rownames(d.scag)),]
  while(length(a) < (p-1)) {
    k <- length(a)
    a[k+1] <- d.scag[1,1:2][!(a[k] == d.scag[1,1:2])]
    d.scag <- d.scag[-grep(a[k],rownames(d.scag)),]
  }
  a[p] <- vars[!(vars %in% a)]
  return(a)
}