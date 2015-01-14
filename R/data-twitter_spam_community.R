#' Historical data used for classification examples.
#'
#' This data contains an igraph network of a network of spam bots found on twitter as part of a data mining project
#'
#' @details \itemize{
#'   \item name screenName of the account
#'   \item location text of the user's location, provided by the user
#'   \item friendsCount the number of accounts the account "follows"
#'   \item followersCount the number of accounts that "follow" this account
#'   \item indegree the indegree of the node
#'   \item outdegree the outdegree of the node
#'   \item betweenness the betweenness of the node
#' }
#'
#' @docType data
#' @keywords datasets
#' @name twitter_spam_community
#' @usage data(twitter_spam_community)
#' @format An igraph network with 120 edges, 94 nodes, and 6 items of metadata per node
NULL

