if(getRversion() >= "2.15.1") {
	utils::globalVariables(c("lon", "lat", "Y1", "Y2", "group", "id", "midX", "midY"))
}

#' ggmapnetwork - Plot a network with ggplot2 suitable for overlay on a ggmap:: map ggplot, or other ggplot
#'
#' This is a descendent of the original ggnet function.  ggnet added the innovation of plotting the network geographically
#' However, ggnet needed to be the first object in the ggplot chain.  ggnetworkplot refuses to be the first object, and only plots
#' networks geogrpahically.  ggnetwork plot must be passed a ggplot object, such as the output from ggmap::ggmap, as its first
#' argument, and then draws the network as an overlay.
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
#' @param trim.labels removes '@@', 'http://', 'www.' and the ending '/' from vertex names. Cleans up labels for website and Twitter networks. Defaults to \code{TRUE}.
#' @param quantize.weights break node weights to quartiles. Fails when quartiles do not uniquely identify nodes.
#' @param subset.threshold delete nodes prior to plotting, based on \code{weight.method} < \code{subset.threshold}. If \code{weight.method} is unspecified, total degree (Freeman's measure) is used. Defaults to 0 (no subsetting).
#' @param ... other arguments supplied to geom_text for the node labels. Arguments pertaining to the title or other items can be achieved through ggplot2 methods.
#' @seealso \code{\link[sna]{gplot}} in the \link[sna:gplot]{sna} package
#' @author Amos Elberg \email{amos.elberg@gmail.com}
#' @author Original by Moritz Marbach \email{mmarbach@@mail.uni-mannheim.de}, Francois Briatte \email{f.briatte@@gmail.com}
#' @details The \code{weight.method} argument produces visually scaled nodes that are proportionally sized to their unweighted degree. To compute weighted centrality or degree measures, see Tore Opsahl's \code{\link[tnet]{tnet}} package.
#' @importFrom grid arrow

ggnetworkmap <- function (gg, net, layout.par = NULL,
					size = 12, alpha = 0.75, weight.method = "none", names = c("",
										 ""), node.group = NULL, node.color = NULL, node.alpha = NULL,
					segment.alpha = NULL, segment.color = "grey", segment.label = NULL,
					segment.size = 0.25, arrow.size = 0, label.nodes = FALSE,
					label.size = size/2, trim.labels = TRUE,
					quantize.weights = FALSE, subset.threshold = 0,
					...)
{


	require_pkgs(c("intergraph", "network", "RColorBrewer","grid"))
	# intergraph   # igraph conversion
	# network      # vertex attributes
	# RColorBrewer # default colors

	# support for igraph objects
	if(class(net) == "igraph") {
		net = intergraph::asNetwork(net)
	}
	if(class(net) != "network")
		stop("net must be a network object of class 'network' or 'igraph'")

	# vertex attributes for weight detection
	vattr = network::list.vertex.attributes(net)

	# get arguments
	weight    = c("indegree", "outdegree", vattr)
	weight    = ifelse(weight.method %in% weight | length(weight.method) > 1,
										 weight.method, "freeman")
	quartiles = quantize.weights
	labels    = label.nodes

	# alpha default
	inherit <- function(x) ifelse(is.null(x), alpha, x)
	# subset
	if(subset.threshold > 0) {
		network::delete.vertices(
			net,
			which(sna::degree(net, cmode = weight) < subset.threshold))
	}

	# get sociomatrix
	m <- network::as.matrix.network.adjacency(net)
	v_function = get("%v%", envir = as.environment("package:network"))

	plotcord = data.frame(
			lon = as.numeric(v_function(net, "lon")),
			lat = as.numeric(v_function(net, "lat"))
		)

		# remove outliers
		plotcord$lon[ abs(plotcord$lon) > quantile(abs(plotcord$lon), .9, na.rm = TRUE) ] = NA
		plotcord$lat[ is.na(plotcord$lon) | abs(plotcord$lat) > quantile(abs(plotcord$lat), .9, na.rm = TRUE) ] = NA
		plotcord$lon[ is.na(plotcord$lat) ] = NA



	# get edgelist
	edglist <- network::as.matrix.network.edgelist(net)
	edges   <- data.frame(plotcord[edglist[, 1], ], plotcord[edglist[, 2], ])

	# get node groups
	if(!is.null(node.group)) {
		network::set.vertex.attribute(net, "elements", as.character(node.group))
		plotcord$group <- as.factor(network::get.vertex.attribute(net, "elements"))
	} else {
		network::set.vertex.attribute(net, "elements", as.character("_nogroup"))
		plotcord$group <- as.factor(network::get.vertex.attribute(net, "elements"))
	}

	# get node weights
	degrees <- data.frame(
		id        = network::network.vertex.names(net),
		indegree  = sapply(net$iel, length),
		outdegree = sapply(net$oel, length)
	)
	degrees$freeman <- with(degrees, indegree + outdegree)

	# custom weights: vector of weights
	if(length(weight.method) == network::network.size(net)) {
		degrees$user = weight.method
		weight = "user"
	}

	# custom weights: vertex attribute
	if(weight.method %in% vattr) {
		degrees$user = v_function(net, weight.method)
		names(degrees)[ ncol(degrees) ] = weight.method
		weight = weight.method
	}

	# trim vertex names
	if(trim.labels) {
		degrees$id = gsub("@|http://|www.|/$", "", degrees$id)
	}


	colnames(edges) <- c("lon", "Y1", "lat", "Y2")

	# set vertex names
	plotcord$id <- as.character(degrees$id)
	if(is.logical(labels)) {
		if(!labels) {
			plotcord$id = ""
		}
	} else {
		plotcord$id[ -which(plotcord$id %in% labels) ] = ""
	}

	# get vertice midpoints (not -yet- used later on)
	edges$midX  <- (edges$lon + edges$lat) / 2
	edges$midY  <- (edges$Y1 + edges$Y2) / 2

	# plot the network
	gg <- gg +
		# plot vertices (links)
		geom_segment(
			aes(x = lon, y = Y1, xend = lat, yend = Y2),
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
		gg <- gg + geom_text(
			aes(x = midX, y = midY),
			data   = edges,
			label = segment.label,
			size = 1 / segment.size, # fixed setting
			colour = segment.color,
			alpha  = inherit(segment.alpha)
		)
	}

	# null weighting
	if(weight.method == c("none")) {
		gg <- gg + geom_point(
			aes(color = group),
			data  = plotcord,
			alpha = inherit(node.alpha),
			size  = size
		)
	}
	else {

		plotcord$weight <- degrees[, weight ]

		# show top weights
		cat(nrow(plotcord), "nodes, weighted by", weight, "\n\n")
		print(head(degrees[ order( -degrees[weight] ), ]))

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
		gg <- gg + geom_point(
			aes(size = weight, color = group),
			data  = plotcord,
			alpha = inherit(node.alpha)
		) + sizer
	}

	# default colors
	n = length(unique(suppressWarnings(na.omit(node.group))))
	if(length(node.color) != n & !is.null(node.group)) {
		warning("Node groups and node colors are of unequal length; using default colors.")
		if(n > 0 & n < 10) {
			node.color = RColorBrewer::brewer.pal(9, "Set1")[1:n]
		}
	}

	# color the nodes
	if(!is.null(node.group)) {
		gg <- gg +
			scale_colour_manual(
				names[1],
				values = node.color,
				guide  = guide_legend(override.aes = list(size = label.size))
			)
	}

	# add text labels
	if(length(unique(plotcord$id)) > 1 | unique(plotcord$id)[1] != "") {
		gg <- gg + geom_text(aes(label = id), size = label.size, ...)
	}

	return(gg)
}