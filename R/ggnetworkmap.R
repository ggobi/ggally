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
#' @param gg an object of class \code{ggplot}.
#' @param data an object of class \code{igraph} or \code{network}. If the object is of class \code{igraph}, the \link[intergraph:asNetwork]{intergraph} package is used to convert it to class \code{network}.
#' @param size size of the network nodes. Defaults to 12. If the nodes are weighted, their area is proportionally scaled up to the size set by \code{size}.
#' @param alpha a level of transparency for nodes, vertices and arrows. Defaults to 0.75.
#' @param weight.method a weighting method for the nodes, used to determine the size of the node.  One of:  The quoted name of a vertex attribute in \code{data}; one of the keywords \code{"indegree"}, \code{"outdegree"} or \code{"degree"} (the default).  Set to \code{"none"} to plot unweighted nodes.
#' @param node.group a \code{factor} or numeric \code{vector} of the same length of as the number of vertices in \quote{data}.
#' @param node.color If \code{node.group} is null, a character string specifying a color.  Otherwise, an object produced by a \quote{scale_color_} function from the \code{ggplot2} package.
#' @param node.alpha transparency of the nodes. Inherits from \code{alpha}.
#' @param segment.alpha transparency of the vertex links. Inherits from \code{alpha}
#' @param segment.color color of the vertex links. Defaults to \code{"grey"}.
#' @param segment.size size of the vertex links, as a vector of values or as a single value. Defaults to 0.25.
#' @param great.circles whether to draw edges as great circles using the \code{geosphere} package.  Defaults to \code{FALSE}
#' @param arrow.size size of the vertex arrows for directed network plotting, in centimeters. Defaults to 0.
#' @param label.nodes label nodes with their vertex names attribute. If set to \code{TRUE}, all nodes are labelled. Also accepts a vector of character strings to match with vertex names.
#' @param label.size size of the labels.  Defaults to \code{size / 2}.
#' @param trim.labels removes '@@', 'http://', 'www.' and the ending '/' from vertex names. Cleans up labels for website and Twitter networks. Defaults to \code{TRUE}.
#' @param quantize.weights break node weights to quartiles. Fails when quartiles do not uniquely identify nodes.
#' @param subset.threshold delete nodes prior to plotting, based on \code{weight.method} < \code{subset.threshold}. If \code{weight.method} is unspecified, total degree (Freeman's measure) is used. Defaults to 0 (no subsetting).
#' @param ... other arguments supplied to geom_text for the node labels. Arguments pertaining to the title or other items can be achieved through ggplot2 methods.
#' @author Amos Elberg \email{amos.elberg@@gmail.com}
#' @author Original by Moritz Marbach \email{mmarbach@@mail.uni-mannheim.de}, Francois Briatte \email{f.briatte@@gmail.com}
#' @importFrom grid arrow
#' @importFrom geosphere gcIntermediate

ggnetworkmap <- function (gg, data, layout.par = NULL,
					size = 12, alpha = 0.75, weight.method = "outdegree", names = c("",
										 ""), node.group = NULL, node.color = NULL, node.alpha = NULL,
					segment.alpha = NULL, segment.color = "grey", great.circles = FALSE,
					segment.size = 0.25, arrow.size = 0, label.nodes = FALSE,
					label.size = size/2, trim.labels = TRUE,
					quantize.weights = FALSE, subset.threshold = 0,
					...)
{


	GGally:::require_pkgs(c("intergraph", "network", "geosphere","grid"))
	# intergraph   # igraph conversion
	# network      # vertex attributes
	# geosphere 	 # great circles

	# support for igraph objects
	net <- data
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

	# get node groups
	if(!is.null(node.group)) {
		if (is.numeric(node.group)) {
			network::set.vertex.attribute(net, "elements", node.group)
			plotcord$group <- node.group
		}
		if (is.factor(node.group)) {
			network::set.vertex.attribute(net, "elements", as.character(node.group))
			plotcord$group <- node.group
		}
		if (is.character(node.group) & length(node.group) == 1) {
			plotcord$group <- network::get.vertex.attribute(net, node.group)
		}
		if (is.character(node.group) & length(node.group) > 1) {
			network::set.vertex.attribute(net, "elements", node.group)
			plotcord$group <- as.factor(node.group)
		}
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

	# set vertex names
	plotcord$id <- as.character(degrees$id)
	if(is.logical(labels)) {
		if(!labels) {
			plotcord$id = ""
		}
	} else {
		plotcord$id[ -which(plotcord$id %in% labels) ] = ""
	}

	# plot the network
	# get edgelist
	edglist <- network::as.matrix.network.edgelist(net)
	edges   <- data.frame(
 		lat1 = plotcord[edglist[, 1], "lat"],
 		lon1 = plotcord[edglist[, 1], "lon"],
		lat2 =  plotcord[edglist[, 2], "lat"],
		lon2 = plotcord[edglist[,2], "lon"]) %>%
		filter(! is.na(lat1) &  ! is.na(lat2) & ! is.na(lon1) & !is.na(lon2)
					 & ! (lat1 == lat2 & lon2 == lon2))

	if (great.circles) {
		pts <- 50  # number of intermediate points for drawing great circles
		i <- 0 # used to keep track of groups when getting intermediate points for great circles
		egl <- geosphere::gcIntermediate(edges[,c("lon1", "lat1")],
																			 edges[,c("lon2", "lat2")],
																			 n = pts,
																			 addStartEnd = TRUE)

		edges <-	plyr::ldply(.data = egl, .fun = function(x) {
				i <<- i + 1
				cbind(x, group = i)
			})

		gg <- gg + geom_path(data = edges,
												 aes(x = lon, y = lat, group = group),
												 size = segment.size,
												 colour = segment.color,
												 alpha = inherit(segment.alpha),
												 arrow = grid::arrow(
												 		type   = "closed",
												 		length = unit(arrow.size, "cm")
												 )
		)
	} else {

		gg <- gg +
			# plot vertices (links)
			geom_segment(
				aes(x = lon1, y = lat1, xend = lon2, yend = lat2),
				data   = edges,
				size   = segment.size,
				colour = segment.color,
				alpha  = inherit(segment.alpha),
				arrow  = arrow(
					type   = "closed",
					length = unit(arrow.size, "cm")
				)
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

		# proportional scaling
		sizer <- scale_size_area(substitute(weight.method), max_size = size)

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
				substitute(weight.method),
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

	if ("scale" %in% class(node.color)) {
		gg <- gg + node.color
	} else {
		if (is.null(node.group)) {
			if (is.null(node.color)) node.color <- "black"
			gg <- gg + scale_color_manual(values = node.color, guide=FALSE)
		}
	}

	# add text labels
	if(length(unique(plotcord$id)) > 1 | unique(plotcord$id)[1] != "") {
		gg <- gg + geom_text(aes(label = id), size = label.size, ...)
	}

	return(gg)
}