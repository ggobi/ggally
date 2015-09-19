#' Data from the Data Expo JSM 2006.
#'
#' This data was provided by NASA for the competition.
#'
#' The data shows 6 years of monthly measurements of a 24x24 spatial grid
#' from Central America:
#'
#' @details \itemize{
#'   \item time integer specifying temporal order of measurements
#'   \item x, y, lat, long spatial location of measurements.
#'   \item cloudhigh, cloudlow, cloudmid, ozone, pressure, surftemp, temperature
#'   are the various satellite measurements.
#'   \item date, day, month, year specifying the time of measurements.
#'   \item id unique ide for each spatial position.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name nasa
#' @usage data(nasa)
#' @format A data frame with 41472 rows and 17 variables
#' @references
#' Murrell, P. (2010) The 2006 Data Expo of the American Statistical Association.
#'   Computational Statistics, 25:551-554.
NULL
