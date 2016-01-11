#' Plot Types
#'
#' Retrieves the type of plot that should be used for all combinations
#'
#' @param data data set to be used
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
plot_types <- function(data) {
  namesD <- names(data)
  dataInfo <- array("", c(ncol(data) ^ 2, 5))

  #horizontal then vertical
  for (j in 1:ncol(data)) {
    for (i in 1:ncol(data)) {
      dataInfo[(i - 1) * ncol(data) + j, ] <- c(
        find_plot_type(data, i, j),
        namesD[j],
        namesD[i],
        j,
        i
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
#' @param data data set to be used
#' @param col1 x column
#' @param col2 y column
#' @keywords internal
#' @author Barret Schloerke \email{schloerke@@gmail.com}
find_plot_type <- function(data, col1, col2) {

  y1Type <- plotting_data_type(data[, col1])

  # diag calculations
  if (col1 == col2) {
    if (y1Type == "na") {
      return("NA-diag")
    } else if (y1Type == "continuous") {
      return("stat_bin-num")
    } else {
      return("stat_bin-cat")
    }
  }

  y2Type <- plotting_data_type(data[, col2])

  if (y1Type == "na" | y2Type == "na") {
    return("NA")
  }

  #cat(names(data)[col2],": ", y2Type,"\t",names(data)[col1],": ",y1Type,"\n")
  isCats <- c(y1Type, y2Type) %in% "category"
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
  if (all(is.na(data[, col1]) | is.na(data[, col2]))) {
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
