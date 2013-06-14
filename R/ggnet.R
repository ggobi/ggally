#' ggnet - Plot a network with ggplot2
#' 
#' Function for making a network plot from an object of class \code{network} or \code{igraph}, using ggplot2.
#' 
#' @export
#' @param net an object of class \code{igraph} or \code{network}. If the object is of class \code{igraph}, the \link{intergraph} package is used to convert it to class \code{network}.
#' @param mode a placement method from the list of modes provided in the \link{sna} package. Defaults to the Fruchterman-Reingold force-directed algorithm.
#' @param size size of the network nodes. Defaults to 12. If the nodes are weighted, their area is proportionally scaled up to the size set by \code{size}.
#' @param alpha a level of transparency for nodes, vertices and arrows. Defaults to 0.75.
#' @param weight.method a weighting method for the nodes. Accepts \code{"indegree"}, \code{"outdegree"} or \code{"degree"} (the default). Set to \code{"none"} to plot unweighted nodes.
#' @param names a character vector of two elements to use as legend titles for the node groups and node weights. Defaults to empty strings.
#' @param node.group a vector of character strings to label the nodes with, of the same length and order as the vertex names. Factors are converted to strings prior to plotting.
#' @param node.color a vector of character strings to color the nodes with, holding as many colors as there are levels in \code{node.group}. Tries to default to \code{"Set1"} if missing.
#' @param node.alpha transparency of the nodes. Inherits from \code{alpha}.
#' @param segment.alpha transparency of the vertex links. Inherits from \code{alpha}.
#' @param segment.color color of the vertex links. Defaults to \code{"grey"}.
#' @param segment.size size of the vertex links. Defaults to 0.25.
#' @param arrow.size size of the vertex arrows for directed network plotting. Defaults to 0.
#' @param label.nodes label nodes with their vertex attributes. If set to \code{TRUE}, all nodes are labelled. Also accepts a vector of character strings to match with vertex names.
#' @param quantize.weights Break node weights to quartiles. Might fail if quartiles do not uniquely identify nodes.
#' @param legend.position location of the captions for node colors and weights. Accepts all positions supported by ggplot2 themes. Defaults to "right".
#' @param ... other arguments supplied to geom_text for the node labels. Arguments pertaining to the title or other items can be achieved through ggplot2 methods.
#' @seealso \code{\link{gplot}} in the \link{sna} package
#' @author Moritz Marbach \email{mmarbach@@mail.uni-mannheim.de} and François Briatte \email{f.briatte@@ed.ac.uk}
#' @examples
#' # random network
#' rnd = network(10)
#' ggnet(rnd, label = TRUE, alpha = 1, color = "white", segment.color = "grey10")
#' # adding groups
#' cat = LETTERS[rbinom(10, 4, .5)]
#' ggnet(rnd, label = TRUE, color = "white", segment.color = "grey10", node.group = cat)

ggnet <- function(net, # an object of class network
  mode = "fruchtermanreingold", # placement algorithm
  size = 12,                # node size
  alpha = .75,              # transparency
  weight.method = "none",   # what to weight the nodes with: "degree", "indegree", "outdegree"
  names = c("", ""),        # what to call the node color and node weight legends
  node.group = NULL,        # what to color the nodes with
  node.color = NULL,        # what colors to use for the node classes
  node.alpha = NULL,        # transparency for nodes (inherits from alpha)
  segment.alpha = NULL,     # transparency for links (inherits from alpha)
  segment.color = "grey",   # default links are rgb(190, 190, 190)
  segment.size  = .25,      # set to 0 to remove from plot
  arrow.size = 0,           # set to 0 to remove from plot
  label.nodes = FALSE,      # add vertex names in small print; can be a list of vertex names
  quantize.weights = FALSE, # break weights to quartiles
  legend.position = "right",# set to "none" to remove from plot
  ...)                      # passed to geom_text for node labels
  {
  require(ggplot2)       # plot
  require(grid)          # arrows
  require(intergraph)    # igraph conversion
  require(network)       # vertex attributes
  require(RColorBrewer)  # default colors
  require(sna)           # placement algorithm

  # support for igraph objects
  if(class(net) == "igraph") net = asNetwork(net)
  if(class(net) != "network") stop("net must be a network object of class 'network' or 'igraph'")
  
  # alpha default
  inherit <- function(x) ifelse(is.null(x), alpha, x)

  # options
  weight = weight.method
  quartiles = quantize.weights
  labels = label.nodes

  set.vertex.attribute(net, "elements", as.character(node.group))
  # get sociomatrix
  m <- as.matrix.network.adjacency(net)
  # get coordinates placement algorithm
  placement <- paste0("gplot.layout.", mode)
  if(!exists(placement)) stop("Unsupported placement method.")
  plotcord <- do.call(placement, list(m, NULL))
  plotcord <- data.frame(plotcord)
  colnames(plotcord) = c("X1", "X2")
  # get edgelist
  edglist <- as.matrix.network.edgelist(net)
  edges <- data.frame(plotcord[edglist[,1],], plotcord[edglist[,2],])
  plotcord$group <- as.factor(get.vertex.attribute(net, "elements"))
  
  # get weights
  degrees <- data.frame(id = network.vertex.names(net), 
                        indegree  = sapply(net$iel, length), 
                        outdegree = sapply(net$oel, length))
  degrees$degree <- with(degrees, indegree + outdegree)
  
  colnames(edges) <- c("X1", "Y1", "X2", "Y2")

  # set vertex names
  plotcord$id <- as.character(degrees$id)
  if(is.logical(labels)) {
    if(!labels) plotcord$id = ""
  } else plotcord$id[-which(plotcord$id %in% labels)] = ""

  # get vertice midpoints (not used later on)
  edges$midX  <- (edges$X1 + edges$X2) / 2
  edges$midY  <- (edges$Y1 + edges$Y2) / 2
  
  # plot the network
  pnet <- ggplot(plotcord, aes(X1, X2)) +
    # plot vertices (links)
    geom_segment(aes(x = X1, y = Y1, xend = X2, yend = Y2), 
                 data = edges, 
                 size = segment.size, 
                 colour = segment.color, 
                 alpha = inherit(segment.alpha),
                 arrow = arrow(type = "closed", 
                               length = unit(arrow.size, "cm")))

  # null weighting
  if(!weight %in% c("degree", "indegree", "outdegree")) {
    if(weight != "none") warning("Unsupported weighting method; plotting unweigthed nodes.")
    pnet <- pnet + geom_point(data = plotcord, alpha = inherit(node.alpha), size = size)
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
      plotcord$weight.label <- cut(plotcord$weight, 
                                   breaks = quantile(plotcord$weight),
                                   include.lowest = TRUE, ordered = TRUE)
      plotcord$weight <- as.integer(plotcord$weight.label)
      sizer <- scale_size_area(names[2], 
                               max_size = size, 
                               labels = levels(plotcord$weight.label))
    }
    
    # add to plot
    pnet <- pnet + geom_point(aes(size = weight),
                              data = plotcord, alpha = inherit(node.alpha)) + sizer
  }
  
  # default colors
  n = length(unique(node.group))
  if(length(node.color) != n &!is.null(node.group)) {
    warning("Node groups and node colors are of unequal length; using default colors.")
    if(n > 0 & n < 10) node.color = brewer.pal(9, "Set1")[1:n]
  }
    
  # color the nodes
  if(!is.null(node.group)) pnet <- pnet + 
    aes(colour = group) +
    scale_colour_manual(names[1], values = node.color,
                        guide = guide_legend(override.aes = list(size = sqrt(size)))) 

  # add text labels
  pnet <- pnet + geom_text(aes(label = id), ...)

  # finalize: remove grid, axes and scales
  pnet <- pnet +
    scale_x_continuous(breaks = NULL) + 
    scale_y_continuous(breaks = NULL) + 
    theme(
      panel.background = element_rect(fill = "white"),
      panel.grid = element_blank(),
      axis.title = element_blank(),
      legend.key = element_rect(colour = "white"),
      legend.position = legend.position
    )

  return(pnet)
}