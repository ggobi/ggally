if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("X1", "X2", "Y1", "Y2", "group"))
}

#' ggnet - Plot a network with ggplot2
#'
#' Function for making a network plot from an object of class \code{network} or \code{igraph}, using ggplot2.  Please visit \url{http://github.com/briatte/ggnet} for the latest development and descriptions about ggnet.
#'
#' @export
#' @param net an object of class \code{igraph} or \code{network}. If the object is of class \code{igraph}, the \link[intergraph:asNetwork]{intergraph} package is used to convert it to class \code{network}.
#' @param mode a placement method from the list of modes provided in the \link[sna:gplot.layout]{sna} package. Defaults to the Fruchterman-Reingold force-directed algorithm. If \code{net} contains two vertex attributes called \code{"lat"} and \code{"lon"}, these are used instead for geographic networks.
#' @param layout.par options to the placement method, as listed in \link[sna]{gplot.layout}.
#' @param size size of the network nodes. Defaults to 12. If the nodes are weighted, their area is proportionally scaled up to the size set by \code{size}.
#' @param alpha a level of transparency for nodes, vertices and arrows. Defaults to 0.75.
#' @param weight.method a weighting method for the nodes. Accepts \code{"indegree"}, \code{"outdegree"} or \code{"degree"} (the default). Set to \code{"none"} to plot unweighted nodes.
#' @param names a character vector of two elements to use as legend titles for the node groups and node weights. Defaults to empty strings.
#' @param node.group a vector of character strings to label the nodes with, of the same length and order as the vertex names. Factors are converted to strings prior to plotting.
#' @param node.color a vector of character strings to color the nodes with, holding as many colors as there are levels in \code{node.group}. Tries to default to \code{"Set1"} if missing.
#' @param node.alpha transparency of the nodes. Inherits from \code{alpha}.
#' @param segment.alpha transparency of the vertex links. Inherits from \code{alpha}.
#' @param segment.color color of the vertex links. Defaults to \code{"grey"}.
#' @param segment.size size of the vertex links, as a vector of values or as a single value. Defaults to 0.25.
#' @param segment.label labels for the vertex links at mid-edges. Label size will be set to 1 / \code{segment.size}, and label alpha will inherit from \code{alpha}.
#' @param arrow.size size of the vertex arrows for directed network plotting, in centimeters. Defaults to 0.
#' @param label.nodes label nodes with their vertex names attribute. If set to \code{TRUE}, all nodes are labelled. Also accepts a vector of character strings to match with vertex names.
#' @param top8.nodes use the top 8 nodes as node groups, colored with \code{"Set1"}. The rest of the network will be plotted as the ninth (grey) group. Experimental.
#' @param trim.labels removes '@@', 'http://', 'www.' and the ending '/' from vertex names. Cleans up labels for website and Twitter networks. Defaults to \code{TRUE}.
#' @param quantize.weights break node weights to quartiles. Fails when quartiles do not uniquely identify nodes.
#' @param subset.threshold delete nodes prior to plotting, based on \code{weight.method} < \code{subset.threshold}. If \code{weight.method} is unspecified, total degree (Freeman's measure) is used. Defaults to 0 (no subsetting).
#' @param legend.position location of the captions for node colors and weights. Accepts all positions supported by ggplot2 themes. Defaults to "right".
#' @param ... other arguments supplied to geom_text for the node labels. Arguments pertaining to the title or other items can be achieved through ggplot2 methods.
#' @seealso \code{\link[sna]{gplot}} in the \link[sna:gplot]{sna} package
#' @author Moritz Marbach \email{mmarbach@@mail.uni-mannheim.de} and Francois Briatte \email{f.briatte@@ed.ac.uk}
#' @details The \code{weight.method} argument produces visually scaled nodes that are proportionally sized to their unweighted degree. To compute weighted centrality or degree measures, see Tore Opsahl's \code[tnet]{tnet} package.
#' @importFrom grid arrow
#' @examples
#' require(network)
#' # make toy random network
#' x                  <- 10
#' ndyads             <- x * (x - 1)
#' density            <- x / ndyads
#' nw.mat             <- matrix(0, nrow = x, ncol = x)
#' dimnames(nw.mat)   <- list(1:x, 1:x)
#' nw.mat[row(nw.mat) != col(nw.mat)] <- runif(ndyads) < density
#' nw.mat
#' rnd <- network(nw.mat)
#' rnd
#'
#' # random network
#' ggnet(rnd, label = TRUE, alpha = 1, color = "white", segment.color = "grey10")
#'
#' # random groups
#' category = LETTERS[rbinom(x, 4, .5)]
#' ggnet(rnd, label = TRUE, color = "white", segment.color = "grey10", node.group = category)
#'
#' # city and service firms data from the UCIrvine Network Data Repository
#' url = url("http://networkdata.ics.uci.edu/netdata/data/cities.RData")
#' print(load(url))
#' close(url)
#'
#' # plot cities, firms and law firms
#' type = cities %v% "type"
#' type = ifelse(grepl("City|Law", type), gsub("I+", "", type), "Firm")
#' ggnet(cities, mode = "kamadakawai", alpha = .2, node.group = type,
#'       label = c("Paris", "Beijing", "Chicago"), color = "darkred")

ggnet <- function(
  net,                          # an object of class network
  mode             = "fruchtermanreingold", # placement algorithm
  layout.par       = NULL,      # placement options
  size             = 12,        # node size
  alpha            = .75,       # transparency
  weight.method    = "none",    # what to weight the nodes with: "freeman", "indegree", "outdegree"
  names            = c("", ""), # what to call the node color and node weight legends
  node.group       = NULL,      # what to color the nodes with
  node.color       = NULL,      # what colors to use for the node classes
  node.alpha       = NULL,      # transparency for nodes (inherits from alpha)
  segment.alpha    = NULL,      # transparency for links (inherits from alpha)
  segment.color    = "grey",    # default links are rgb(190, 190, 190)
  segment.label    = NULL,      # label network at mid-edges
  segment.size     = .25,       # set to 0 to remove from plot
  arrow.size       = 0,         # set to 0 to remove from plot
  label.nodes      = FALSE,     # add vertex names in small print; can be a list of vertex names
  top8.nodes       = FALSE,     # color the top 8 nodes by weighting method with ColorBrewer Set1
  trim.labels      = TRUE,      # clean vertex names
  quantize.weights = FALSE,     # break weights to quartiles
  subset.threshold = 0,         # what nodes to exclude, based on weight.method ≥ subset
  legend.position  = "right",   # set to "none" to remove from plot
  ...                           # passed to geom_text for node labels
){
  require(intergraph   , quietly = TRUE) # igraph conversion
  require(network      , quietly = TRUE) # vertex attributes
  require(RColorBrewer , quietly = TRUE) # default colors
  require(sna          , quietly = TRUE) # placement algorithm

  # get arguments
  weight    = c("indegree", "outdegree")
  weight    = ifelse(weight.method %in% weight, weight.method, "freeman")
  quartiles = quantize.weights
  labels    = label.nodes

  # alpha default
  inherit <- function(x) ifelse(is.null(x), alpha, x)

  # support for igraph objects
  if(class(net) == "igraph") {
    net = asNetwork(net)
  }
  if(class(net) != "network")
    stop("net must be a network object of class 'network' or 'igraph'")

  # subset
  if(subset.threshold > 0) {
    network::delete.vertices(
      net,
      which(sna::degree(net, cmode = weight) < subset.threshold))
  }

  # get sociomatrix
  m <- as.matrix.network.adjacency(net)

  # get coordinates placement algorithm
  placement <- paste0("gplot.layout.", mode)
  if(!exists(placement)) stop("Unsupported placement method.")

  if(all(c("lat", "lon") %in% list.vertex.attributes(net))) {
      plotcord = data.frame(X1 = as.numeric(net %v% "lon"), X2 = as.numeric(net %v% "lat"))
      # remove outliers
      plotcord$X1[ abs(plotcord$X1) > quantile(abs(plotcord$X1), .9, na.rm = TRUE) ] = NA
      plotcord$X2[ is.na(plotcord$X1) | abs(plotcord$X2) > quantile(abs(plotcord$X2), .9, na.rm = TRUE) ] = NA
      plotcord$X1[ is.na(plotcord$X2) ] = NA
    } else {
      plotcord <- do.call(placement, list(m, layout.par))
      plotcord <- data.frame(plotcord)
      colnames(plotcord) = c("X1", "X2")
    }

  # get edgelist
  edglist <- as.matrix.network.edgelist(net)
  edges   <- data.frame(plotcord[edglist[, 1], ], plotcord[edglist[, 2], ])

  # get node groups
  if(!is.null(node.group)) {
    network::set.vertex.attribute(net, "elements", as.character(node.group))
    plotcord$group <- as.factor(network::get.vertex.attribute(net, "elements"))
  }

  # get node weights
  degrees <- data.frame(
    id        = network.vertex.names(net),
    indegree  = sapply(net$iel, length),
    outdegree = sapply(net$oel, length)
  )
  degrees$freeman <- with(degrees, indegree + outdegree)

  # trim vertex names
  if(trim.labels) {
    degrees$id = gsub("@|http://|www.|/$", "", degrees$id)
  }

  # set top 8 nodes as groups
  if(top8.nodes) {
    all                  = degrees[, weight]
    top                  = degrees$id[order(all, decreasing = TRUE)[1:8]]
    top                  = which(degrees$id %in% top)
    plotcord$group       = as.character(degrees$id)
    plotcord$group[-top] = paste0("(", weight, " > ", subset.threshold - 1, ")")
    node.group           = plotcord$group
    node.color           = brewer.pal(9, "Set1")[c(9, 1:8)]
  }

  colnames(edges) <- c("X1", "Y1", "X2", "Y2")

  # set vertex names
  plotcord$id <- as.character(degrees$id)
  if(is.logical(labels)) {
    if(!labels) {
      plotcord$id = ""
    }
  } else {
    plotcord$id[-which(plotcord$id %in% labels)] = ""
  }

  # get vertice midpoints (not -yet- used later on)
  edges$midX  <- (edges$X1 + edges$X2) / 2
  edges$midY  <- (edges$Y1 + edges$Y2) / 2

  # plot the network
  pnet <- ggplot(plotcord, aes(X1, X2)) +
    # plot vertices (links)
    geom_segment(
      aes(x = X1, y = Y1, xend = X2, yend = Y2),
      data   = edges,
      size   = segment.size,
      colour = segment.color,
      alpha  = inherit(segment.alpha),
      arrow  = arrow(
        type   = "closed",
        length = unit(arrow.size, "cm")
      )
    )

  # label mid-edges
  if(!is.null(segment.label) & length(segment.label) == nrow(edges)) {
    pnet <- pnet + geom_text(
      aes(x = midX, y = midY),
      data   = edges,
      label = segment.label,
      size = 1 / segment.size, # fixed setting
      colour = segment.color,
      alpha  = inherit(segment.alpha)
      )
  }
  
  # null weighting
  if(weight.method == "none") {
    pnet <- pnet + geom_point(
      data  = plotcord,
      alpha = inherit(node.alpha),
      size  = size
    )
  }
  else {
    plotcord$weight <- degrees[, which(names(degrees) == weight)]

    # show top weights
    cat(nrow(plotcord), "nodes, weighted by", weight, "\n\n")
    print(head(degrees[order(-degrees[weight]), ]))

    # proportional scaling
    sizer <- scale_size_area(names[2], max_size = size)

    # quartiles
    if(quartiles) {
      plotcord$weight.label <- cut(
        plotcord$weight,
        breaks         = quantile(plotcord$weight),
        include.lowest = TRUE,
        ordered        = TRUE
      )
      plotcord$weight <- as.integer(plotcord$weight.label)
      sizer <- scale_size_area(
        names[2],
        max_size = size,
        labels   = levels(plotcord$weight.label)
      )
    }

    # add to plot
    pnet <- pnet + geom_point(
      aes(size = weight),
      data  = plotcord,
      alpha = inherit(node.alpha)
    ) + sizer
  }

  # default colors
  n = length(unique(suppressWarnings(na.omit(node.group))))
  if(length(node.color) != n & !is.null(node.group)) {
    warning("Node groups and node colors are of unequal length; using default colors.")
    if(n > 0 & n < 10) {
      node.color = brewer.pal(9, "Set1")[1:n]
    }
  }

  # color the nodes
  if(!is.null(node.group)) {
    pnet <- pnet +
      aes(colour = group) +
      scale_colour_manual(
        names[1],
        values = node.color,
        guide  = guide_legend(override.aes = list(size = 1 + sqrt(size)))
      )
  }

  # add text labels
  if(length(unique(plotcord$id)) > 1 | unique(plotcord$id)[1] != "") {
    pnet <- pnet + geom_text(aes(label = id), size = 1 + sqrt(size), ...)
  }

  # finalize: remove grid, axes and scales
  pnet <- pnet +
    scale_x_continuous(breaks = NULL) +
    scale_y_continuous(breaks = NULL) +
    theme(
      panel.background = element_rect(fill = "white"),
      panel.grid       = element_blank(),
      axis.title       = element_blank(),
      legend.key       = element_rect(colour = "white"),
      legend.position  = legend.position
    )

  return(pnet)
}
