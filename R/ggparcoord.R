if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("variable", "value", "ggally_splineFactor"))
}



#' ggparcoord - A ggplot2 Parallel Coordinate Plot
#'
#' A function for plotting static parallel coordinate plots, utilizing
#' the \code{ggplot2} graphics package.
#'
#' \code{scale} is a character string that denotes how to scale the variables
#' in the parallel coordinate plot. Options:
#' \itemize{
#'   \item{\code{std}}{: univariately, subtract mean and divide by standard deviation}
#'   \item{\code{robust}}{: univariately, subtract median and divide by median absolute deviation}
#'   \item{\code{uniminmax}}{: univariately, scale so the minimum of the variable is zero, and the maximum is one}
#'   \item{\code{globalminmax}}{: no scaling is done; the range of the graphs is defined
#'     by the global minimum and the global maximum}
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
#'   \item{\code{anyClass}}{: order variables by their separation between any one class and
#'     the rest (as opposed to their overall variation between classes). This is accomplished
#'     by calculating the F-statistic for each class vs. the rest, for each axis variable.
#'     The axis variables are then ordered (decreasing) by their maximum of k F-statistics,
#'     where k is the number of classes.}
#'   \item{\code{allClass}}{: order variables by their overall F statistic (decreasing) from
#'     an ANOVA with \code{groupColumn} as the explanatory variable (note: it is required
#'     to specify a \code{groupColumn} with this ordering method). Basically, this method
#'     orders the variables by their variation between classes (most to least).}
#'   \item{\code{skewness}}{: order variables by their sample skewness (most skewed to
#'     least skewed)}
#'   \item{\code{Outlying}}{: order by the scagnostic measure, Outlying, as calculated
#'     by the package \code{scagnostics}. Other scagnostic measures available to order
#'     by are \code{Skewed}, \code{Clumpy}, \code{Sparse}, \code{Striated}, \code{Convex}, \code{Skinny}, \code{Stringy}, and
#'     \code{Monotonic}. Note: To use these methods of ordering, you must have the \code{scagnostics}
#'     package loaded.}
#' }
#'
#' @param data the dataset to plot
#' @param columns a vector of variables (either names or indices) to be axes in the plot
#' @param groupColumn a single variable to group (color) by
#' @param scale method used to scale the variables (see Details)
#' @param scaleSummary if scale=="center", summary statistic to univariately
#'   center each variable by
#' @param centerObsID if scale=="centerObs", row number of case plot should
#'   univariately be centered on
#' @param missing method used to handle missing values (see Details)
#' @param order method used to order the axes (see Details)
#' @param showPoints logical operator indicating whether points should be
#'   plotted or not
#' @param splineFactor logical or numeric operator indicating whether spline interpolation should be used.  Numeric values will multiplied by the number of columns, \code{TRUE} will default to cubic interpolation, \code{\link[base]{AsIs}} to set the knot count directly and \code{0}, \code{FALSE}, or non-numeric values will not use spline interpolation.
#' @param alphaLines value of alpha scaler for the lines of the parcoord plot or a column name of the data
#' @param boxplot logical operator indicating whether or not boxplots should
#'   underlay the distribution of each variable
#' @param shadeBox color of underlaying box which extends from the min to the
#'   max for each variable (no box is plotted if shadeBox == NULL)
#' @param mapping aes string to pass to ggplot object
#' @param title character string denoting the title of the plot
#' @author Jason Crowley \email{crowley.jason.s@@gmail.com}, Barret Schloerke \email{schloerke@@gmail.com}, Di Cook \email{dicook@@iastate.edu}, Heike Hofmann \email{hofmann@@iastate.edu}, Hadley Wickham \email{h.wickham@@gmail.com}
#' @return ggplot object that if called, will print
#' @import plyr
#' @importFrom reshape melt melt.data.frame
#' @importFrom stats complete.cases sd median mad lm spline
#' @export
#' @examples
#' # use sample of the diamonds data for illustrative purposes
#' data(diamonds, package="ggplot2")
#' diamonds.samp <- diamonds[sample(1:dim(diamonds)[1], 100), ]
#'
#' # basic parallel coordinate plot, using default settings
#' # ggparcoord(data = diamonds.samp, columns = c(1, 5:10))
#'
#' # this time, color by diamond cut
#' gpd <- ggparcoord(data = diamonds.samp, columns = c(1, 5:10), groupColumn = 2)
#' # gpd
#'
#' # underlay univariate boxplots, add title, use uniminmax scaling
#' gpd <- ggparcoord(data = diamonds.samp, columns = c(1, 5:10), groupColumn = 2,
#'   scale = "uniminmax", boxplot = TRUE, title = "Parallel Coord. Plot of Diamonds Data")
#' # gpd
#'
#' # utilize ggplot2 aes to switch to thicker lines
#' gpd <- ggparcoord(data = diamonds.samp, columns = c(1, 5:10), groupColumn = 2,
#'   title="Parallel Coord. Plot of Diamonds Data", mapping = ggplot2::aes(size = 1))
#' # gpd
#'
#' # basic parallel coord plot of the msleep data, using 'random' imputation and
#' # coloring by diet (can also use variable names in the columns and groupColumn
#' # arguments)
#' data(msleep, package="ggplot2")
#' gpd <- ggparcoord(data = msleep, columns = 6:11, groupColumn = "vore", missing =
#'   "random", scale = "uniminmax")
#' # gpd
#'
#' # center each variable by its median, using the default missing value handler,
#' # 'exclude'
#' gpd <- ggparcoord(data = msleep, columns = 6:11, groupColumn = "vore", scale =
#'   "center", scaleSummary = "median")
#' # gpd
#'
#' # with the iris data, order the axes by overall class (Species) separation using
#' # the anyClass option
#' gpd <- ggparcoord(data = iris, columns = 1:4, groupColumn = 5, order = "anyClass")
#' # gpd
#'
#' # add points to the plot, add a title, and use an alpha scalar to make the lines
#' # transparent
#' gpd <- ggparcoord(data = iris, columns = 1:4, groupColumn = 5, order = "anyClass",
#'   showPoints = TRUE, title = "Parallel Coordinate Plot for the Iris Data",
#'   alphaLines = 0.3)
#' # gpd
#'
#' # color according to a column
#' iris2 <- iris
#' iris2$alphaLevel <- c("setosa" = 0.2, "versicolor" = 0.3, "virginica" = 0)[iris2$Species]
#' gpd <- ggparcoord(data = iris2, columns = 1:4, groupColumn = 5, order = "anyClass",
#'   showPoints = TRUE, title = "Parallel Coordinate Plot for the Iris Data",
#'   alphaLines = "alphaLevel")
#' # gpd
#'
#' ## Use splines on values, rather than lines (all produce the same result)
#' columns <- c(1, 5:10)
#' gpd <- ggparcoord(diamonds.samp, columns, groupColumn = 2, splineFactor = TRUE)
#' # gpd
#' gpd <- ggparcoord(diamonds.samp, columns, groupColumn = 2, splineFactor = 3)
#' # gpd
#' splineFactor <- length(columns) * 3
#' gpd <- ggparcoord(diamonds.samp, columns, groupColumn = 2, splineFactor = I(splineFactor))
#' # gpd
ggparcoord <- function(
  data,
  columns      = 1:ncol(data),
  groupColumn  = NULL,
  scale        = "std",
  scaleSummary = "mean",
  centerObsID  = 1,
  missing      = "exclude",
  order        = columns,
  showPoints   = FALSE,
  splineFactor = FALSE,
  alphaLines   = 1,
  boxplot      = FALSE,
  shadeBox     = NULL,
  mapping      = NULL,
  title        = ""
) {
  saveData <- data

  ### Error Checking ###
  if (is.null(groupColumn)) {
    if (any(tolower(order) %in% c("anyclass", "allclass"))) {
      stop("can't use the 'order' methods anyClass or allClass without specifying groupColumn")
    }
  } else if (
    !( (length(groupColumn) == 1) && (is.numeric(groupColumn) || is.character(groupColumn)))
  ) {
    stop("invalid value for 'groupColumn'; must be a single numeric or character index")
  }

  if (!(tolower(scale) %in% c(
    "std", "robust", "uniminmax", "globalminmax", "center", "centerobs"
  ))) {
    stop(str_c(
      "invalid value for 'scale'; must be one of ",
      "'std', 'robust', 'uniminmax', 'globalminmax', 'center', or 'centerObs'"
    ))
  }

  if (!(centerObsID %in% 1:dim(data)[1])) {
    stop("invalid value for 'centerObsID'; must be a single numeric row index")
  }

  if (!(tolower(missing) %in% c("exclude", "mean", "median", "min10", "random"))) {
    stop(
      "invalid value for 'missing'; must be one of 'exclude', 'mean', 'median', 'min10', 'random'"
    )
  }

  if (!(
    is.numeric(order) || (
      is.character(order) &&
      (order %in% c(
        "skewness", "allClass", "anyClass", "Outlying", "Skewed", "Clumpy",
        "Sparse", "Striated", "Convex", "Skinny", "Stringy", "Monotonic"
      ))
    )) ) {
    stop(str_c(
      "invalid value for 'order'; must either be a vector of column indices or one of ",
      "'skewness', 'allClass', 'anyClass', 'Outlying', 'Skewed', 'Clumpy', 'Sparse', 'Striated', ",
      "'Convex', 'Skinny', 'Stringy', 'Monotonic'"
    ))
  }

  if (!(is.logical(showPoints))) {
    stop("invalid value for 'showPoints'; must be a logical operator")
  }

  alphaLinesIsCharacter <- is.character(alphaLines)
  if (alphaLinesIsCharacter) {
    if (!(alphaLines %in% names(data))) {
      stop("'alphaLines' column is missing in data")
    }

    alphaRange <- range(data[, alphaLines])
    if (any(is.na(alphaRange))) {
      stop("missing data in 'alphaLines' column")
    }

    if (alphaRange[1] < 0 || alphaRange[2] > 1) {
      stop("invalid value for 'alphaLines' column; max range must be from 0 to 1")
    }
    alphaVar <- data[, alphaLines]

  } else if ((alphaLines < 0) || (alphaLines > 1)) { # nolint
    stop("invalid value for 'alphaLines'; must be a scalar value between 0 and 1")
  }

  if (!(is.logical(boxplot))) {
    stop("invalid value for 'boxplot'; must be a logical operator")
  }

  if (is.logical(splineFactor)) {
    if (splineFactor) {
      splineFactor <- 3
    } else {
      splineFactor <- 0
    }
  } else if (! is.numeric(splineFactor)) {
    stop("invalid value for 'splineFactor'; must be a logical or numeric value")
  }


  ### Setup ###
  if (is.numeric(groupColumn)) {
    groupColumn <- names(data)[groupColumn]
  }
  if (!is.null(groupColumn)) {
    groupVar <- data[, groupColumn]
  }

  if (is.character(columns)) {
    columns_ <- c()
    for (colPos in seq_along(columns)) {
      columns_[colPos] <- which(colnames(data) == columns[colPos])
    }
    columns <- columns_
  }
  # data <- data[, columns]

  # Change character vars to factors
  char.vars <- column_is_character(data)
  if (length(char.vars) >= 1) {
    for (char.var in char.vars) {
      data[, char.var] <- factor(data[, char.var])
    }
  }
  # Change factors to numeric
  fact.vars <- column_is_factor(data)
  fact.vars <- setdiff(fact.vars, groupColumn)
  if (length(fact.vars) >= 1) {
    for (fact.var in fact.vars) {
      data[, fact.var] <- as.numeric(data[, fact.var])
    }
  }
  # Save this form of the data for order calculations (don't want imputed
  # missing values affecting order, but do want any factor/character vars
  # being plotted as numeric)
  saveData2 <- data
  saveData2[, groupColumn] <- as.numeric(saveData2[, groupColumn])

  p <- c(ncol(data) + 1, ncol(data) + 2)
  data$.ID <- as.factor(1:nrow(data))
  data$anyMissing <- apply(is.na(data[, columns]), 1, any)
  columnsPlusTwo <- c(columns, p)

  inner_rescaler_default <- function (x, type = "sd", ...) {
    # copied directly from reshape because of import difficulties :-(
    # rescaler.default
    switch(type,
      rank = rank(x, ...),
      var = , # nolint
      sd = (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE),
      robust = (x - median(x, na.rm = TRUE)) / mad(x, na.rm = TRUE),
      I = x,
      range = (x - min(x, na.rm = TRUE)) / diff(range(x, na.rm = TRUE))
    )
  }
  inner_rescaler <- function(x, type = "sd", ...) {
    # copied directly from reshape because of import difficulties :-(
    # rescaler.data.frame
    continuous <- sapply(x, is.numeric)
    if (any(continuous)) {
      if (type %in% c("sd", "robust", "range")) {
        # indicating columns containing only one single value
        singleVal <- sapply(x, function(col){
          if (length(unique(col)) == 1) {
            TRUE
          } else {
            FALSE
          }
        })
        ind          <- continuous & !singleVal
        x[ind]       <- lapply(x[ind], inner_rescaler_default, type = type, ...)
        x[singleVal] <- 1
      } else {
        x[continuous] <- lapply(x[continuous], inner_rescaler_default, type = type, ...)
      }
    }
    x
  }

  ### Scaling ###
  if (tolower(scale) %in% c("std", "robust", "uniminmax", "center")) {
    rescalerType <- c(
      "std" = "sd",
      "robust" = "robust",
      "uniminmax" = "range",
      "center" = "range"
    )[tolower(scale)]
    data[, columnsPlusTwo] <- inner_rescaler(data[, columnsPlusTwo], type = rescalerType)

    if (tolower(scale) == "center") {
      data[, columns] <- apply(data[, columns], 2, function(x) {
        x <- x - eval(
          parse(text = paste(
            scaleSummary,
            "(x, na.rm=TRUE)",
            sep = ""
          ))
        )
      })
    }

  }

  ### Imputation ###
  if (tolower(missing) == "exclude") {
    dataCompleteCases <- complete.cases(data[, columnsPlusTwo])

    if (!is.null(groupColumn)) {
      groupVar <- groupVar[dataCompleteCases]
    }

    if (alphaLinesIsCharacter) {
      alphaVar <- alphaVar[dataCompleteCases]
    }

    data <- data[dataCompleteCases, ]
  }
  else if (tolower(missing) %in% c("mean", "median", "min10", "random")) {
    missingFns <- list(
      mean = function(x) {
        mean(x, na.rm = TRUE)
      },
      median = function(x) {
        median(x, na.rm = TRUE)
      },
      min10 = function(x){
        0.9 * min(x, na.rm = TRUE)
      },
      random = function(x) {
        num <- sum(is.na(x))
        idx <- sample(which(!is.na(x)), num, replace = TRUE)
        x[idx]
      }
    )
    missing_fn <- missingFns[[tolower(missing)]]
    data[, columns] <- apply(data[, columns], 2, function(x) {
      if (any(is.na(x))){
        x[is.na(x)] <- missing_fn(x)
      }
      return(x)
    })

  }

  ### Scaling (round 2) ###
  # Centering by observation needs to be done after handling missing values
  #   in case the observation to be centered on has missing values
  if (tolower(scale) == "centerobs") {
    data[, columnsPlusTwo] <- inner_rescaler(data[, columnsPlusTwo], type = "range")
    data[, columns] <- apply(data[, columns], 2, function(x){
      x <- x - x[centerObsID]
    })
  }

  # meltIDVars <- c(".ID", "anyMissing")
  meltIDVars <- colnames(data)[-columns]

  if (!is.null(groupColumn)) {
    # data <- cbind(data, groupVar)
    # names(data)[dim(data)[2]] <- groupCol

    meltIDVars <- union(groupColumn, meltIDVars)
  }

  if (alphaLinesIsCharacter) {
    data <- cbind(data, alphaVar)
    names(data)[dim(data)[2]] <- alphaLines
    meltIDVars <- union(meltIDVars, alphaLines)
  }

  # if(is.list(mapping)) {
  #   mappingNames <- names(mapping)
  # }
  data.m <- melt(data, id.vars = meltIDVars, measure.vars = columns)

  ### Ordering ###
  if (length(order) > 1 & is.numeric(order)) {
     data.m$variable <- factor(data.m$variable, levels = names(saveData)[order])
  }
  else if (order %in% c("Outlying", "Skewed", "Clumpy", "Sparse", "Striated", "Convex", "Skinny",
    "Stringy", "Monotonic")) {

    require_pkgs("scagnostics")
    scag <- scagnostics::scagnostics(saveData2)
    data.m$variable <- factor(data.m$variable, levels = scag_order(scag, names(saveData2), order))
  }
  else if (tolower(order) == "skewness") {
    abs.skew <- abs(apply(saveData2, 2, skewness))
    data.m$variable <- factor(
      data.m$variable,
      levels = names(abs.skew)[order(abs.skew, decreasing = TRUE)]
    )
  }
  else if (tolower(order) == "allclass") {
    f.stats <- rep(NA, length(columns))
    names(f.stats) <- names(saveData2[columns])
    for (i in 1:length(columns)) {
      f.stats[i] <- summary(lm(saveData2[, i] ~ groupVar))$fstatistic[1]
    }
    data.m$variable <- factor(
      data.m$variable,
      levels = names(f.stats)[order(f.stats, decreasing = TRUE)]
    )
  }
  else if (tolower(order) == "anyclass") {
    axis.order <- singleClassOrder(groupVar, saveData2)
    data.m$variable <- factor(data.m$variable, levels = axis.order)
  }

  if (!is.null(groupColumn)) {
    mapping2 <- aes_string(
      x = "variable",
      y = "value",
      group = ".ID",
      colour = groupColumn
    )
  } else {
    mapping2 <- aes_string(
      x = "variable",
      y = "value",
      group = ".ID"
    )
  }
  mapping2 <- add_and_overwrite_aes(mapping2, mapping)
  # mapping2 <- add_and_overwrite_aes(aes_string(size = I(0.5)), mapping2)
  p <- ggplot(data = data.m, mapping = mapping2)

  if (!is.null(shadeBox)) {
    # Fix so that if missing = "min10", the box only goes down to the true min
    d.sum <- ddply(data.m, .(variable), summarize,
      min = min(value),
      max = max(value))
    p <- p + geom_linerange(data = d.sum, size = I(10), col = shadeBox,
      mapping = aes(y = NULL, ymin = min, ymax = max, group = variable))
  }

  if (boxplot)
    p <- p + geom_boxplot(mapping = aes(group = variable), alpha = 0.8)

  if (!is.null(mapping2$size)) {
    lineSize <- mapping2$size
  } else {
    lineSize <- 0.5
  }

  if (splineFactor > 0) {
    data.m$ggally_splineFactor <- splineFactor
    if (class(splineFactor) == "AsIs") {
      data.m <- ddply(
        data.m, ".ID", transform,
        spline = spline(variable, value, n = ggally_splineFactor[1])
      )
    } else {
      data.m <- ddply(
        data.m, ".ID", transform,
        spline = spline(variable, value, n = length(variable) * ggally_splineFactor[1])
      )
    }

    linexvar <- "spline.x"
    lineyvar <- "spline.y"

    if (alphaLinesIsCharacter) {
      p <- p +
        geom_line(
          aes_string(x = linexvar, y = lineyvar, alpha = alphaLines),
          size = lineSize,
          data = data.m
        ) +
        scale_alpha(range = alphaRange)

    } else {
      p <- p +
        geom_line(
          aes_string(x = linexvar, y = lineyvar),
          alpha = alphaLines,
          size = lineSize,
          data = data.m
        )
    }

    if (showPoints) {
      p <- p + geom_point(aes(x = as.numeric(variable), y = value))
    }

    xAxisLabels <- levels(data.m$variable)
    # while continuous data, this makes it present like it's discrete
    p <- p + scale_x_discrete(breaks = seq_along(xAxisLabels), labels = xAxisLabels)

  } else {
    if (alphaLinesIsCharacter) {
      p <- p +
        geom_line(aes_string(alpha = alphaLines), size = lineSize, data = data.m) +
        scale_alpha(range = alphaRange)

    } else {
      # p <- p + geom_line(alpha = alphaLines, size = lineSize)
      p <- p + geom_line(alpha = alphaLines)
    }

    if (showPoints) {
      p <- p + geom_point()
    }
  }

  if (title != "") {
    p <- p + labs(title = title)
  }

  p
}

#' Get vector of variable types from data frame
#'
#' @keywords internal
#' @param df data frame to extract variable types from
#' @author Jason Crowley \email{crowley.jason.s@@gmail.com}
#' @return character vector with variable types, with names corresponding to
#'   the variable names from df
column_is_character <- function(df) {
  x <- unlist(lapply(unclass(df), is.character))
  names(x)[x]
}
#' @rdname column_is_character
column_is_factor <- function(df) {
  x <- unlist(lapply(unclass(df), is.factor))
  names(x)[x]
}

#' Find order of variables
#'
#' Find order of variables based on a specified scagnostic measure
#' by maximizing the index values of that measure along the path.
#'
#' @param scag \code{scagnostics} object
#' @param vars character vector of the variables to be ordered
#' @param measure scagnostics measure to order according to
#' @author Barret Schloerke
#' @return character vector of variable ordered according to the given
#'   scagnostic measure
scag_order <- function(scag, vars, measure) {

  scag <- sort(scag[measure, ], decreasing = TRUE)
  scagNames <- names(scag)

  # retrieve all names.  assume name doesn't contain a space
  nameLocs <- regexec("^([^ ]+) \\* ([^ ]+)$", scagNames)

  colNames <- lapply(seq_along(nameLocs), function(i) {
    nameLoc <- nameLocs[[i]]
    scagName <- scagNames[[i]]
    # retrieve the column name from "FIRSTNAME * SECONDNAME"
    substr(rep(scagName, 2), nameLoc[-1], nameLoc[-1] + attr(nameLoc, "match.length")[-1] - 1)
  })

  ret <- c()
  colNamesLength <- length(colNames)
  colNameValues <- unlist(colNames)
  for (i in seq_along(colNames)) {
    cols <- colNames[[i]]
    colsUsed <- cols %in% ret
    # if none of the columns have been added...
    if (colsUsed[1] == FALSE && colsUsed[2] == FALSE) {
      # find out which column comes next in the set, append that one first
      if (i < colNamesLength) {
        remainingColumns <- colNameValues[(2 * (i + 1)):(2 * colNamesLength)]
        col1Pos <- which.min(cols[1] == remainingColumns)
        col2Pos <- which.min(cols[2] == remainingColumns)
        if (col2Pos < col1Pos) {
          cols <- rev(cols)
        }
        ret <- append(ret, cols)
      } else {
        # nothing left in set, append both
        ret <- append(ret, cols)
      }

    # if only the first hasn't been added...
    } else if (colsUsed[1] == FALSE) {
      ret <- append(ret, cols[1])

    # if only the second hasn't been added...
    } else if (colsUsed[2] == FALSE) {
      ret <- append(ret, cols[2])
    }
  }

  if (length(ret) != length(vars)) {
    stop(str_c(
      "Could not compute a correct ordering: ",
      length(vars) - length(ret), " values are missing. ",
      "Missing: ", paste0(vars[! (vars %in% ret)], collapse = ", ")
    ))
  }

  return(ret)
}


#' Order axis variables
#'
#' Order axis variables by separation between one class and the rest
#' (most separation to least).
#'
#' @param classVar class variable (vector from original dataset)
#' @param axisVars variables to be plotted as axes (data frame)
#' @param specClass character string matching to level of \code{classVar}; instead
#'   of looking for separation between any class and the rest, will only look for
#'   separation between this class and the rest
#' @author Jason Crowley \email{crowley.jason.s@@gmail.com}
#' @importFrom stats lm
#' @return character vector of names of axisVars ordered such that the first
#'   variable has the most separation between one of the classes and the rest, and
#'   the last variable has the least (as measured by F-statistics from an ANOVA)
singleClassOrder <- function(classVar, axisVars, specClass=NULL) {
  if (!is.null(specClass)) {
    # for when user is interested in ordering by variation between one class and
    # the rest...will add this later
  } else {
    var.names <- colnames(axisVars)
    class.names <- levels(classVar)
    f.stats <- matrix(NA, nrow = length(class.names), ncol = length(var.names), dimnames =
      list(class.names, var.names))
    for (i in 1:length(class.names)) {
      f.stats[i, ] <- apply(axisVars, 2, function(x) {
        return(summary(lm(x ~ as.factor(classVar == class.names[i])))$fstatistic[1])
      })
    }
    var.maxF <- apply(f.stats, 2, max)
    return(names(var.maxF)[order(var.maxF, decreasing = TRUE)])
  }
}

#' Sample skewness
#'
#' Calculate the sample skewness of a vector
#' while ignoring missing values.
#'
#' @param x numeric vector
#' @author Jason Crowley \email{crowley.jason.s@@gmail.com}
#' @return sample skewness of \code{x}
skewness <- function(x) {
  x <- x[!is.na(x)]
  xbar <- mean(x)
  n <- length(x)
  skewness <- (1 / n) * sum( (x - xbar) ^ 3) / ( (1 / n) * sum( (x - xbar) ^ 2)) ^ (3 / 2)
  return(skewness)
}
