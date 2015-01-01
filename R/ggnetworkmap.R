if(getRversion() >= "2.15.1") {
	utils::globalVariables(c("X1", "X2", "Y1", "Y2", "group", "id", "midX", "midY"))
}

#' ggmapnetwork - Plot a network with ggplot2 suitable for overlay on a ggmap:: map ggplot, or other ggplot
#'
#' Function for making a network plot from an object of class \code{network} or \code{igraph}, using ggplot2.  Please visit \url{http://github.com/briatte/ggnet} for the latest development and descriptions about ggnet.
#'
#' @export
#' @param net an object of class \code{igraph} or \code{network}. If the object is of class \code{igraph}, the \link[intergraph:asNetwork]{intergraph} package is used to convert it to class \code{network}.
#' @param mode a placement method from the list of modes provided in the \link[sna:gplot.layout]{sna} package. Defaults to the Fruchterman-Reingold force-directed algorithm. If \code{mode} is set to \code{"geo"} and \code{net} contains two vertex attributes called \code{"lat"} and \code{"lon"}, these are used instead for geographic networks.
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
#' @param label.size size of the labels.  Defaults to \code{size / 2}.
#' @param top8.nodes use the top 8 nodes as node groups, colored with \code{"Set1"}. The rest of the network will be plotted as the ninth (grey) group. Experimental.
#' @param trim.labels removes '@@', 'http://', 'www.' and the ending '/' from vertex names. Cleans up labels for website and Twitter networks. Defaults to \code{TRUE}.
#' @param quantize.weights break node weights to quartiles. Fails when quartiles do not uniquely identify nodes.
#' @param subset.threshold delete nodes prior to plotting, based on \code{weight.method} < \code{subset.threshold}. If \code{weight.method} is unspecified, total degree (Freeman's measure) is used. Defaults to 0 (no subsetting).
#' @param legend.position location of the captions for node colors and weights. Accepts all positions supported by ggplot2 themes. Defaults to "right".
#' @param ... other arguments supplied to geom_text for the node labels. Arguments pertaining to the title or other items can be achieved through ggplot2 methods.
#' @seealso \code{\link[sna]{gplot}} in the \link[sna:gplot]{sna} package
#' @author Moritz Marbach \email{mmarbach@@mail.uni-mannheim.de} and Francois Briatte \email{f.briatte@@gmail.com}
#' @details The \code{weight.method} argument produces visually scaled nodes that are proportionally sized to their unweighted degree. To compute weighted centrality or degree measures, see Tore Opsahl's \code{\link[tnet]{tnet}} package.
#' @importFrom grid arrow
#' @examples
#' if(require(network)){
#' # make toy random network
#' x                  <- 10
#' ndyads             <- x * (x - 1)
#' density            <- x / ndyads
#' nw.mat             <- matrix(0, nrow = x, ncol = x)
#' dimnames(nw.mat)   <- list(1:x, 1:x)
#' nw.mat[row(nw.mat) != col(nw.mat)] <- runif(ndyads) < density
#' nw.mat
#' rnd <- network::network(nw.mat)
#' rnd
#'
#' # random network
#' pRnd <- ggnet(rnd, label.nodes = TRUE, alpha = 1, color = "white", segment.color = "grey10")
#' # pRnd
#'
#' # random groups
#' category = LETTERS[rbinom(x, 4, .5)]
#' ggnet(rnd, label.nodes = TRUE, color = "white", segment.color = "grey10", node.group = category)
#'
#' # city and service firms data from the UCIrvine Network Data Repository
#' data(cityServiceFirms, package = "GGally")
#'
#' # plot cities, firms and law firms
#' type = cityServiceFirms %v% "type"
#' type = ifelse(grepl("City|Law", type), gsub("I+", "", type), "Firm")
#' pRnd <- ggnet(cityServiceFirms, mode = "kamadakawai", alpha = .2, node.group = type,
#'       label.nodes = c("Paris", "Beijing", "Chicago"), color = "darkred")
#' # pRnd
#' }

ggnetworkmap <- function (gg, net, layout.par = NULL,
					size = 12, alpha = 0.75, weight.method = "none", names = c("",
										 ""), node.group = NULL, node.color = NULL, node.alpha = NULL,
					segment.alpha = NULL, segment.color = "grey", segment.label = NULL,
					segment.size = 0.25, arrow.size = 0, label.nodes = FALSE,
					label.size = size/2, trim.labels = TRUE,
					quantize.weights = FALSE, subset.threshold = 0, legend.position = "right",
					...)
{
	GGally:::require_pkgs(c("intergraph", "network", "RColorBrewer", "sna","grid"))
	if (class(net) == "igraph") {
		net = intergraph::asNetwork(net)
	}
	if (class(net) != "network")
		stop("net must be a network object of class 'network' or 'igraph'")
	vattr = network::list.vertex.attributes(net)
	weight = c("indegree", "outdegree", vattr)
	weight = ifelse(weight.method %in% weight | length(weight.method) >
										1, weight.method, "freeman")
	quartiles = quantize.weights
	labels = label.nodes
	inherit <- function(x) ifelse(is.null(x), alpha, x)
	if (subset.threshold > 0) {
		network::delete.vertices(net, which(sna::degree(net,
																										cmode = weight) < subset.threshold))
	}
	m <- network::as.matrix.network.adjacency(net)
	v_function = get("%v%", envir = as.environment("package:network"))
	plotcord = data.frame(lon = as.numeric(v_function(net,
																										 "lon")), lat = as.numeric(v_function(net, "lat")))
	plotcord$lon[abs(plotcord$lon) > quantile(abs(plotcord$lon),
																						0.9, na.rm = TRUE)] = NA
	plotcord$lat[is.na(plotcord$lon) | abs(plotcord$lat) > quantile(abs(plotcord$lat),
																																 0.9, na.rm = TRUE)] = NA
	plotcord$lon[is.na(plotcord$lat)] = NA

	edglist <- network::as.matrix.network.edgelist(net)
	edges <- data.frame(plotcord[edglist[, 1], ], plotcord[edglist[,
																																 2], ])
	if (!is.null(node.group)) {
		network::set.vertex.attribute(net, "elements", as.character(node.group))
		plotcord$group <- as.factor(network::get.vertex.attribute(net,
																															"elements"))
	}
	degrees <- data.frame(id = network::network.vertex.names(net),
												indegree = sapply(net$iel, length), outdegree = sapply(net$oel,
																																							 length))
	degrees$freeman <- with(degrees, indegree + outdegree)
	if (length(weight.method) == network::network.size(net)) {
		degrees$user = weight.method
		weight = "user"
	}
	if (weight.method %in% vattr) {
		degrees$user = v_function(net, weight.method)
		names(degrees)[ncol(degrees)] = weight.method
		weight = weight.method
	}
	if (trim.labels) {
		degrees$id = gsub("@|http://|www.|/$", "", degrees$id)
	}

	colnames(edges) <- c("lon", "Y1", "lat", "Y2")
	plotcord$id <- as.character(degrees$id)
	if (is.logical(labels)) {
		if (!labels) {
			plotcord$id = ""
		}
	}
	else {
		plotcord$id[-which(plotcord$id %in% labels)] = ""
	}
	edges$midX <- (edges$lon + edges$lat)/2
	edges$midY <- (edges$Y1 + edges$Y2)/2
	gg <- gg + geom_segment(aes(x = lon, y = Y1, xend = lat, yend = Y2), data = edges, size = segment.size,
																											 colour = segment.color, alpha = inherit(segment.alpha),
																											 arrow = arrow(type = "closed", length = unit(arrow.size,
																											 																						 "cm")))
	if (!is.null(segment.label) & length(segment.label) == nrow(edges)) {
		gg <- gg + geom_text(aes(x = midX, y = midY), data = edges,
														 label = segment.label, size = 1/segment.size, colour = segment.color,
														 alpha = inherit(segment.alpha))
	}
	if (weight.method == c("none")) {
		gg <- gg + geom_point(data = plotcord, alpha = inherit(node.alpha),
															size = size)
	}
	else {
		plotcord$weight <- degrees[, weight]
		cat(nrow(plotcord), "nodes, weighted by", weight, "\n\n")
		print(head(degrees[order(-degrees[weight]), ]))
		sizer <- scale_size_area(names[2], max_size = size)
		if (quartiles) {
			plotcord$weight.label <- cut(plotcord$weight, breaks = quantile(plotcord$weight),
																	 include.lowest = TRUE, ordered = TRUE)
			plotcord$weight <- as.integer(plotcord$weight.label)
			sizer <- scale_size_area(names[2], max_size = size,
															 labels = levels(plotcord$weight.label))
		}
		gg <- gg + geom_point(aes(size = weight), data = plotcord,
															alpha = inherit(node.alpha)) + sizer
	}
	n = length(unique(suppressWarnings(na.omit(node.group))))
	if (length(node.color) != n & !is.null(node.group)) {
		warning("Node groups and node colors are of unequal length; using default colors.")
		if (n > 0 & n < 10) {
			node.color = RColorBrewer::brewer.pal(9, "Set1")[1:n]
		}
	}
	if (!is.null(node.group)) {
		gg <- gg + aes(colour = node.group) + scale_colour_manual(names[1],
																														 values = node.color, guide = guide_legend(override.aes = list(size = label.size)))
	}
	if (length(unique(plotcord$id)) > 1 | unique(plotcord$id)[1] !=
				"") {
		gg <- gg + geom_text(aes(label = id), size = label.size,
														 ...)
	}
	return(gg)
}