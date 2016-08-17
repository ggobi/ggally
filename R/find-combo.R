#' Plot Types
#'
#' Retrieves the type of plot that should be used for all combinations
#'
#' @param data data set to be used
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
plot_types <- function(data, columnsX, columnsY, allowDiag = TRUE) {


  plotTypesX <- lapply(data[columnsX], plotting_data_type)
  plotTypesY <- lapply(data[columnsY], plotting_data_type)

  columnNamesX <- names(data)[columnsX]
  columnNamesY <- names(data)[columnsY]

  isNaData <- as.data.frame(is.na(data))

  lenX <- length(plotTypesX)
  lenY <- length(plotTypesY)
  n <- lenX * lenY

  plotType <- character(n)
  xVar <- character(n)
  yVar <- character(n)
  posX <- integer(n)
  posY <- integer(n)

  #horizontal then vertical
  for (yI in seq_len(lenY)) {
    yColName <- columnNamesY[yI]
    for (xI in seq_len(lenX)) {
      xColName <- columnNamesX[xI]
      yVarVal <- ifelse(xColName == yColName && allowDiag, NA, yColName)
      pos <- (yI - 1) * lenX + xI

      plotType[pos] <- find_plot_type(
        xColName, yColName,
        plotTypesX[xI], plotTypesY[yI],
        isAllNa = all(isNaData[[xColName]] | isNaData[[yColName]]),
        allowDiag = allowDiag
      )
      xVar[pos] <- xColName
      yVar[pos] <- yVarVal
      posX[pos] <- xI
      posY[pos] <- yI
    }
  }

  dataInfo <- data.frame(
    plotType = plotType,
    xVar = xVar,
    yVar = yVar,
    posX = posX,
    posY = posY,
    isVertical = NA,
    stringsAsFactors = FALSE
  )

  isCombo <- dataInfo$plotType == "combo"
  if (any(isCombo)) {
    dataInfo$isVertical[isCombo] <- unlist(plotTypesX[xVar[isCombo]]) == "discrete"
  }

  dataInfo
}

#' Find Plot Types
#'
#' Retrieves the type of plot for the specific columns
#'
#' @param col1Name x column name
#' @param col2Name y column name
#' @param type1 x column type
#' @param type2 y column type
#' @param isAllNa is.na(data)
#' @param allowDiag allow for diag values to be returned
#' @author Barret Schloerke \email{schloerke@@gmail.com}
find_plot_type <- function(col1Name, col2Name, type1, type2, isAllNa, allowDiag) {

  # diag calculations
  if (col1Name == col2Name && allowDiag) {
    if (type1 == "na") {
      return("na-diag")
    } else if (type1 == "continuous") {
      return("continuous-diag")
    } else {
      return("discrete-diag")
    }
  }

  if (type1 == "na" | type2 == "na") {
    return("na")
  }

  #cat(names(data)[col2Name],": ", type2,"\t",names(data)[col1Name],": ",type1,"\n")
  isCats <- c(type1, type2) %in% "discrete"
  if (any(isCats)) {
    if (all(isCats)) {
      return("discrete")
    }

    return("combo")
  }

  # check if any combo of the two columns is all na
  if (isAllNa) {
    return("na")
  }

  return("continuous")
}

#' Check if object is a date
#'
#' @keywords internal
#' @param x vector
is_date <- function(x) {
  inherits(x, c("POSIXt", "POSIXct", "POSIXlt", "Date"))
}

#' Get plotting data type
#'
#' @keywords internal
#' @param x vector
plotting_data_type <- function(x) {
  if (all(is.na(x))) {
    return("na")
  }
  if (is_date(x)) {
    "continuous"
  } else if (!is.null(attributes(x)) || all(is.character(x))) {
    "discrete"
  } else {
    "continuous"
  }
}
