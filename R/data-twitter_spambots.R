#' Twitter spambots
#'
#' A network of spambots found on Twitter as part of a data mining project.
#'
#' Each node of the network is identified by the Twitter screen name of the
#' account and further carries five vertex attributes:
#'
#' @details \itemize{
#'   \item location user's location, as provided by the user
#'   \item lat latitude, based on the user's location
#'   \item lon longitude, based on the user's location
#'   \item followers number of Twitter accounts that follow this account
#'   \item friends number of Twitter accounts followed by the account
#' }
#'
#' @docType data
#' @author Amos Elberg
#' @keywords datasets
#' @name twitter_spambots
#' @usage data(twitter_spambots)
#' @format An object of class \code{network} with 120 edges and 94 vertices.
NULL
