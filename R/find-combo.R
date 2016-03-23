#' Plot Types
#'
#' Retrieves the type of plot that should be used for all combinations
#'
#' @param data data set to be used
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
plot_types <- function(data, columnsX, columnsY) {


  plotTypesX <- lapply(data[columnsX], plotting_data_type)
  plotTypesY <- lapply(data[columnsY], plotting_data_type)

  columnNamesX <- names(data)[columnsX]
  columnNamesY <- names(data)[columnsY]

  isNaData <- is.na(data)

  lenX <- length(plotTypesX)
  lenY <- length(plotTypesY)

  dataInfo <- array("", c(lenX * lenY, 5))

  #horizontal then vertical
  for (yI in seq_len(lenY)) {
    yColName <- columnNamesY[yI]
    for (xI in seq_len(lenX)) {
      xColName <- columnNamesX[xI]
      yvar <- ifelse(xColName == yColName, NA, yColName)
      dataInfo[(yI - 1) * lenX + xI, ] <- c(
        find_plot_type(
          xColName, yColName,
          plotTypesX[xI], plotTypesY[yI],
          isAllNa = all(isNaData[, xColName] | isNaData[, yColName])
        ),
        xColName, yvar,
        xI, yI
      )
    }
  }

  dataInfo <- as.data.frame(dataInfo)
  colnames(dataInfo) <- c("Type", "xvar", "yvar", "posx", "posy")

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
#' @param isNaData is.na(data)
#' @author Barret Schloerke \email{schloerke@@gmail.com}
find_plot_type <- function(col1Name, col2Name, type1, type2, isAllNa) {

  # diag calculations
  if (col1Name == col2Name) {
    if (type1 == "na") {
      return("NA-diag")
    } else if (type1 == "continuous") {
      return("stat_bin-num")
    } else {
      return("stat_bin-cat")
    }
  }

  if (type1 == "na" | type2 == "na") {
    return("NA")
  }

  #cat(names(data)[col2Name],": ", type2,"\t",names(data)[col1Name],": ",type1,"\n")
  isCats <- c(type1, type2) %in% "category"
  if (any(isCats)) {
    if (all(isCats)) {
      return("mosaic")
    }

    if (isCats[1]) {
      return("box-hori")
    } else {
      return("box-vert")
    }
  }

  # check if any combo of the two columns is all na
  if (isAllNa) {
    return("NA")
  }

  return("scatterplot")
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
    "category"
  } else {
    "continuous"
  }
}
