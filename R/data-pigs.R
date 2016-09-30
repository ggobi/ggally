#' United Kingdom Pig Production
#'
#' This data contains about the United Kingdom Pig Production from the book 'Data' by Andrews and Herzberg. The original data can be on Statlib: http://lib.stat.cmu.edu/datasets/Andrews/T62.1
#'
#' The time variable has been added from a combination of year and quarter
#'
#' @details \itemize{
#'   \item time year + (quarter - 1) / 4
#'   \item year year of production
#'   \item quarter quarter of the year of production
#'   \item gilts number of sows giving birth for the first time
#'   \item profit ratio of price to an index of feed price
#'   \item s_per_herdsz ratio of the number of breeding pigs slaughtered to the total breeding herd size
#'   \item production number of pigs slaughtered that were reared for meat
#'   \item herdsz breeding herd size
#' }
#'
#' @docType data
#' @keywords datasets
#' @name pigs
#' @usage data(pigs)
#' @format A data frame with 48 rows and 8 variables
#' @references
#' Andrews, David F., and Agnes M. Herzberg. Data: a collection of problems from many fields for the student and research worker. Springer Science & Business Media, 2012.
NULL
