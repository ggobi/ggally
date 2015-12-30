if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("X1", "X2", "Y1", "Y2", "midX", "midY"))
}

#' ggnet2 - Plot a network with ggplot2
#'
#' Function for plotting network objects using ggplot2, with additional control
#' over graphical parameters that are not supported by the \code{\link{ggnet}}
#' function. Please visit \url{http://github.com/briatte/ggnet} for the latest
#' version of ggnet2, and \url{https://briatte.github.io/ggnet} for a vignette
#' that contains many examples and explanations.
#'
#' @export
#' @param net an object of class \code{\link[network]{network}}, or any object
#' that can be coerced to this class, such as an adjacency or incidence matrix,
#' or an edge list: see \link[network]{edgeset.constructors} and
#' \link[network]{network} for details. If the object is of class
#' \code{\link[igraph:igraph-package]{igraph}} and the
#' \code{\link[intergraph:intergraph-package]{intergraph}} package is installed,
#' it will be used to convert the object: see
#' \code{\link[intergraph]{asNetwork}} for details.
#' @param mode a placement method from those provided in the
#' \code{\link[sna]{sna}} package: see \link[sna:gplot.layout]{gplot.layout} for
#' details. Also accepts the names of two numeric vertex attributes of
#' \code{net}, or a matrix of numeric coordinates, in which case the first two
#' columns of the matrix are used.
#' Defaults to the Fruchterman-Reingold force-directed algorithm.
#' @param layout.par options to be passed to the placement method, as listed in
#' \link[sna]{gplot.layout}.
#' Defaults to \code{NULL}.
#' @param layout.exp a multiplier to expand the horizontal axis if node labels
#' get clipped: see \link[scales]{expand_range} for details.
#' Defaults to \code{0} (no expansion).
#' @param alpha the level of transparency of the edges and nodes, which might be
#' a single value, a vertex attribute, or a vector of values.
#' Also accepts \code{"mode"} on bipartite networks (see 'Details').
#' Defaults to \code{1} (no transparency).
#' @param color the color of the nodes, which might be a single value, a vertex
#' attribute, or a vector of values.
#' Also accepts \code{"mode"} on bipartite networks (see 'Details').
#' Defaults to \code{grey75}.
#' @param shape the shape of the nodes, which might be a single value, a vertex
#' attribute, or a vector of values.
#' Also accepts \code{"mode"} on bipartite networks (see 'Details').
#' Defaults to \code{19} (solid circle).
#' @param size the size of the nodes, in points, which might be a single value,
#' a vertex attribute, or a vector of values. Also accepts \code{"indegree"},
#' \code{"outdegree"}, \code{"degree"} or \code{"freeman"} to size the nodes by
#' their unweighted degree centrality (\code{"degree"} and \code{"freeman"} are
#' equivalent): see \code{\link[sna]{degree}} for details. All node sizes must
#' be strictly positive.
#' Also accepts \code{"mode"} on bipartite networks (see 'Details').
#' Defaults to \code{9}.
#' @param max_size the \emph{maximum} size of the node when \code{size} produces
#' nodes of different sizes, in points.
#' Defaults to \code{9}.
#' @param na.rm whether to subset the network to nodes that are \emph{not}
#' missing a given vertex attribute. If set to any vertex attribute of
#' \code{net}, the nodes for which this attribute is \code{NA} will be removed.
#' Defaults to \code{NA} (does nothing).
#' @param palette the palette to color the nodes, when \code{color} is not a
#' color value or a vector of color values. Accepts named vectors of color
#' values, or if \code{\link[RColorBrewer]{RColorBrewer}} is installed, any
#' ColorBrewer palette name: see \code{\link[RColorBrewer]{brewer.pal}} and
#' \url{http://colorbrewer2.org/} for details.
#' Defaults to \code{NULL}, which will create an array of grayscale color values
#' if \code{color} is not a color value or a vector of color values.
#' @param alpha.palette the palette to control the transparency levels of the
#' nodes set by \code{alpha} when the levels are not numeric values.
#' Defaults to \code{NULL}, which will create an array of alpha transparency
#' values if \code{alpha} is not a numeric value or a vector of numeric values.
#' @param alpha.legend the name to assign to the legend created by
#' \code{alpha} when its levels are not numeric values.
#' Defaults to \code{NA} (no name).
#' @param color.palette see \code{palette}
#' @param color.legend the name to assign to the legend created by
#' \code{palette}.
#' Defaults to \code{NA} (no name).
#' @param shape.palette the palette to control the shapes of the nodes set by
#' \code{shape} when the shapes are not numeric values.
#' Defaults to \code{NULL}, which will create an array of shape values if
#' \code{shape} is not a numeric value or a vector of numeric values.
#' @param shape.legend the name to assign to the legend created by
#' \code{shape} when its levels are not numeric values.
#' Defaults to \code{NA} (no name).
#' @param size.palette the palette to control the sizes of the nodes set by
#' \code{size} when the sizes are not numeric values.
#' @param size.legend the name to assign to the legend created by
#' \code{size}.
#' Defaults to \code{NA} (no name).
#' @param size.zero whether to accept zero-sized nodes based on the value(s) of
#' \code{size}.
#' Defaults to \code{FALSE}, which ensures that zero-sized nodes are still
#' shown in the plot and its size legend.
#' @param size.cut whether to cut the size of the nodes into a certain number of
#' quantiles. Accepts \code{TRUE}, which tries to cut the sizes into quartiles,
#' or any positive numeric value, which tries to cut the sizes into that many
#' quantiles. If the size of the nodes do not contain the specified number of
#' distinct quantiles, the largest possible number is used.
#' See \code{\link[stats]{quantile}} and \code{\link[base]{cut}} for details.
#' Defaults to \code{FALSE} (does nothing).
#' @param size.min whether to subset the network to nodes with a minimum size,
#' based on the values of \code{size}.
#' Defaults to \code{NA} (preserves all nodes).
#' @param size.max whether to subset the network to nodes with a maximum size,
#' based on the values of \code{size}.
#' Defaults to \code{NA} (preserves all nodes).
#' @param label whether to label the nodes. If set to \code{TRUE}, nodes are
#' labeled with their vertex names. If set to a vector that contains as many
#' elements as there are nodes in \code{net}, nodes are labeled with these. If
#' set to any other vector of values, the nodes are labeled only when their
#' vertex name matches one of these values.
#' Defaults to \code{FALSE} (no labels).
#' @param label.alpha the level of transparency of the node labels, as a
#' numeric value, a vector of numeric values, or as a vertex attribute
#' containing numeric values.
#' Defaults to \code{1} (no transparency).
#' @param label.color the color of the node labels, as a color value, a vector
#' of color values, or as a vertex attribute containing color values.
#' Defaults to \code{"black"}.
#' @param label.size the size of the node labels, in points, as a numeric value,
#' a vector of numeric values, or as a vertex attribute containing numeric
#' values.
#' Defaults to \code{max_size / 2} (half the maximum node size), which defaults
#' to \code{4.5}.
#' @param label.trim whether to apply some trimming to the node labels. Accepts
#' any function that can process a character vector, or a strictly positive
#' numeric value, in which case the labels are trimmed to a fixed-length
#' substring of that length: see \code{\link[base]{substr}} for details.
#' Defaults to \code{FALSE} (does nothing).
#' @param node.alpha see \code{alpha}
#' @param node.color see \code{color}
#' @param node.label see \code{label}
#' @param node.shape see \code{shape}
#' @param node.size see \code{size}
#' @param edge.alpha the level of transparency of the edges.
#' Defaults to the value of \code{alpha}, which defaults to \code{1}.
#' @param edge.color the color of the edges, as a color value, a vector of color
#' values, or as an edge attribute containing color values.
#' Defaults to \code{"grey50"}.
#' @param edge.lty the linetype of the edges, as a linetype value, a vector of
#' linetype values, or as an edge attribute containing linetype values.
#' Defaults to \code{"solid"}.
#' @param edge.size the size of the edges, in points, as a numeric value, a
#' vector of numeric values, or as an edge attribute containing numeric values.
#' All edge sizes must be strictly positive.
#' Defaults to \code{0.25}.
#' @param edge.label the labels to plot at the middle of the edges, as a single
#' value, a vector of values, or as an edge attribute.
#' Defaults to \code{NULL} (no edge labels).
#' @param edge.label.alpha the level of transparency of the edge labels, as a
#' numeric value, a vector of numeric values, or as an edge attribute
#' containing numeric values.
#' Defaults to \code{1} (no transparency).
#' @param edge.label.color the color of the edge labels, as a color value, a
#' vector of color values, or as an edge attribute containing color values.
#' Defaults to \code{label.color}, which defaults to \code{"black"}.
#' @param edge.label.fill the background color of the edge labels.
#' Defaults to \code{"white"}.
#' @param edge.label.size the size of the edge labels, in points, as a numeric
#' value, a vector of numeric values, or as an edge attribute containing numeric
#' values. All edge label sizes must be strictly positive.
#' Defaults to \code{max_size / 2} (half the maximum node size), which defaults
#' to \code{4.5}.
#' @param arrow.size the size of the arrows for directed network edges, in
#' points. See \code{\link[grid]{arrow}} for details.
#' Defaults to \code{0} (no arrows).
#' @param arrow.gap a setting aimed at improving the display of edge arrows by
#' plotting slightly shorter edges. Accepts any value between \code{0} and
#' \code{1}, where a value of \code{0.05} will generally achieve good results
#' when the size of the nodes is reasonably small.
#' Defaults to \code{0} (no shortening).
#' @param arrow.type the type of the arrows for directed network edges. See
#' \code{\link[grid]{arrow}} for details.
#' Defaults to \code{"closed"}.
#' @param legend.size the size of the legend symbols and text, in points.
#' Defaults to \code{9}.
#' @param legend.position the location of the plot legend(s). Accepts all
#' \code{legend.position} values supported by \code{\link[ggplot2]{theme}}.
#' Defaults to \code{"right"}.
#' @param ... other arguments passed to the \code{geom_text} object that sets
#' the node labels: see \code{\link[ggplot2]{geom_text}} for details.
#' @seealso \code{\link{ggnet}} in this package,
#' \code{\link[sna]{gplot}} in the \code{\link[sna]{sna}} package, and
#' \code{\link[network]{plot.network}} in the \code{\link[network]{network}}
#' package
#' @author Moritz Marbach and Francois Briatte, with help from Heike Hoffmann,
#' Pedro Jordano and Ming-Yu Liu
#' @details The degree centrality measures that can be produced through the
#' \code{size} argument will take the directedness of the network into account,
#' but will be unweighted. To compute weighted network measures, see the
#' \code{\link[tnet:tnet-package]{tnet}} package by Tore Opsahl.
#'
#' The nodes of bipartite networks can be mapped to their mode by passing the
#' \code{"mode"} argument to any of \code{alpha}, \code{color}, \code{shape} and
#' \code{size}, in which case the nodes of the primary mode will be mapped as
#' \code{"actor"}, and the nodes of the secondary mode will be mapped as
#' \code{"event"}.
#' @importFrom utils installed.packages
#' @importFrom grDevices gray.colors
#' @examples
#' if(require(network)) {
#'
#'   # random adjacency matrix
#'   x           <- 10
#'   ndyads      <- x * (x - 1)
#'   density     <- x / ndyads
#'   m           <- matrix(0, nrow = x, ncol = x)
#'   dimnames(m) <- list(letters[ 1:x ], letters[ 1:x ])
#'   m[ row(m) != col(m) ] <- runif(ndyads) < density
#'   m
#'
#'   # random undirected network
#'   n <- network::network(m, directed = FALSE)
#'   n
#'
#'   ggnet2(n, label = TRUE)
#'   ggnet2(n, label = TRUE, shape = 15)
#'   ggnet2(n, label = TRUE, shape = 15, color = "black", label.color = "white")
#'
#'   # add vertex attribute
#'   x = network.vertex.names(n)
#'   x = ifelse(x %in% c("a", "e", "i"), "vowel", "consonant")
#'   n %v% "phono" = x
#'
#'   ggnet2(n, color = "phono")
#'   ggnet2(n, color = "phono", palette = c("vowel" = "gold", "consonant" = "grey"))
#'   ggnet2(n, shape = "phono", color = "phono")
#'
#'   if (require(RColorBrewer)) {
#'
#'     # random groups
#'     n %v% "group" <- sample(LETTERS[1:3], 10, replace = TRUE)
#'
#'     ggnet2(n, color = "group", palette = "Set2")
#'
#'   }
#'
#'   # random weights
#'   n %e% "weight" <- sample(1:3, network.edgecount(n), replace = TRUE)
#'   ggnet2(n, edge.size = "weight", edge.label = "weight")
#'
#'   # edge arrows on a directed network
#'   ggnet2(network(m, directed = TRUE), arrow.gap = 0.05, arrow.size = 10)
#'
#'   # Padgett's Florentine wedding data
#'   data(flo, package = "network")
#'   flo
#'
#'   ggnet2(flo, label = TRUE)
#'   ggnet2(flo, label = TRUE, label.trim = 4, vjust = -1, size = 3, color = 1)
#'   ggnet2(flo, label = TRUE, size = 12, color = "white")
#'
#' }
ggnet2 <- function(
  net,
  mode             = "fruchtermanreingold",
  layout.par       = NULL,
  layout.exp       = 0,
  alpha            = 1,
  color            = "grey75",
  shape            = 19,
  size             = 9,
  max_size         = 9,
  na.rm            = NA,
  palette          = NULL,
  alpha.palette    = NULL,
  alpha.legend     = NA,
  color.palette    = palette,
  color.legend     = NA,
  shape.palette    = NULL,
  shape.legend     = NA,
  size.palette     = NULL,
  size.legend      = NA,
  size.zero        = FALSE,
  size.cut         = FALSE,
  size.min         = NA,
  size.max         = NA,
  label            = FALSE,
  label.alpha      = 1,
  label.color      = "black",
  label.size       = max_size / 2,
  label.trim       = FALSE,
  node.alpha       = alpha,
  node.color       = color,
  node.label       = label,
  node.shape       = shape,
  node.size        = size,
  edge.alpha       = 1,
  edge.color       = "grey50",
  edge.lty         = "solid",
  edge.size        = .25,
  edge.label       = NULL,
  edge.label.alpha = 1,
  edge.label.color = label.color,
  edge.label.fill  = "white",
  edge.label.size  = max_size / 2,
  arrow.size       = 0,
  arrow.gap        = 0,
  arrow.type       = "closed",
  legend.size      = 9,
  legend.position  = "right",
  ...
){

  # -- packages ----------------------------------------------------------------

  require_pkgs(c("network", "sna", "scales"))

  # -- conversion to network class ---------------------------------------------

  if (class(net) == "igraph" && "intergraph" %in% rownames(installed.packages())) {
    net = intergraph::asNetwork(net)
  } else if (class("net") == "igraph") {
    stop("install the 'intergraph' package to use igraph objects with ggnet2")
  }

  if (!network::is.network(net)) {
    net = try(network::network(net), silent = TRUE)
  }

  if (!network::is.network(net)) {
    stop("could not coerce net to a network object")
  }

  # -- network functions -------------------------------------------------------

  get_v = get("%v%", envir = as.environment("package:network"))
  get_e = get("%e%", envir = as.environment("package:network"))

  set_mode = function(x, mode = network::get.network.attribute(x, "bipartite")) {
    c(rep("actor", mode), rep("event", n_nodes - mode))
  }

  set_node = function(x, value, mode = TRUE) {

    if (is.null(x) || is.na(x) || is.infinite(x) || is.nan(x)) {
      stop(paste("incorrect", value, "value"))
    } else if (is.numeric(x) && any(x < 0)) {
      stop(paste("incorrect", value, "value"))
    } else if (length(x) == n_nodes) {
      x
    } else if (length(x) > 1) {
      stop(paste("incorrect", value, "length"))
    } else if (x %in% v_attr) {
      get_v(net, x)
    } else if (mode && x == "mode" & is_bip) {
      set_mode(net)
    } else {
      x
    }

  }

  set_edge = function(x, value) {

    if (is.null(x) || is.na(x) || is.infinite(x) || is.nan(x)) {
      stop(paste("incorrect", value, "value"))
    } else if (is.numeric(x) && any(x < 0)) {
      stop(paste("incorrect", value, "value"))
    } else if (length(x) == n_edges) {
      x
    } else if (length(x) > 1) {
      stop(paste("incorrect", value, "length"))
    } else if (x %in% e_attr) {
      get_e(net, x)
    } else {
      x
    }

  }

  set_attr = function(x) {

    if (length(x) == n_nodes) {
      x
    } else if (length(x) > 1) {
      stop(paste("incorrect coordinates length"))
    } else if (!x %in% v_attr) {
      stop(paste("vertex attribute", x, "was not found"))
    } else if (!is.numeric(get_v(net, x))) {
      stop(paste("vertex attribute", x, "is not numeric"))
    } else {
      get_v(net, x)
    }

  }

  set_name = function(x, y) {

    z = length(x) == 1 && x %in% v_attr
    z = ifelse(is.na(y), z, y)
    z = ifelse(isTRUE(z), x, z)
    ifelse(is.logical(z), "", z)

  }

  set_size = function(x) {

    y = x + (0 %in% x) * !size.zero
    y = scales::rescale_max(y)
    y = scales::abs_area(max_size)(y)
    if (is.null(names(x))) names(y) = x else names(y) = names(x)
    y

  }

  is_one = function(x) length(unique(x)) == 1
  is_col = function(x) all(is.numeric(x)) | all(network::is.color(x))

  # -- network structure -------------------------------------------------------

  n_nodes = network::network.size(net)
  n_edges = network::network.edgecount(net)

  v_attr = network::list.vertex.attributes(net)
  e_attr = network::list.edge.attributes(net)

  is_bip = network::is.bipartite(net)
  is_dir = ifelse(network::is.directed(net), "digraph", "graph")

  if (!is.numeric(arrow.size) || arrow.size < 0) {
    stop("incorrect arrow.size value")
  } else if (arrow.size > 0 & is_dir == "graph") {
    warning("network is undirected; arrow.size ignored")
    arrow.size = 0
  }

  if (!is.numeric(arrow.gap) || arrow.gap < 0 || arrow.gap > 1) {
    stop("incorrect arrow.gap value")
  } else if (arrow.gap > 0 & is_dir == "graph") {
    warning("network is undirected; arrow.gap ignored")
    arrow.gap = 0
  }

  if (network::is.hyper(net)) {
    stop("ggnet2 cannot plot hyper graphs")
  }

  if (network::is.multiplex(net)) {
    stop("ggnet2 cannot plot multiplex graphs")
  }

  if (network::has.loops(net)) {
    warning("ggnet2 does not know how to handle self-loops")
  }

  # -- check max_size ----------------------------------------------------------

  x = max_size

  if (!is.numeric(x) || is.infinite(x) || is.nan(x) || x < 0) {
    stop("incorrect max_size value")
  }

  # -- initialize dataset ------------------------------------------------------

  data = data.frame(label = get_v(net, "vertex.names"), stringsAsFactors = FALSE)

  data$alpha = set_node(node.alpha , "node.alpha")
  data$color = set_node(node.color , "node.color")
  data$shape = set_node(node.shape , "node.shape")
  data$size  = set_node(node.size  , "node.size")

  # -- node removal ------------------------------------------------------------

  if (length(na.rm) > 1) {
    stop("incorrect na.rm value")
  } else if (!is.na(na.rm)) {

    if (!na.rm %in% v_attr) {

      stop(paste("vertex attribute", na.rm, "was not found"))

    }

    x = which(is.na(get_v(net, na.rm)))
    message(paste("na.rm removed", length(x), "nodes out of", nrow(data)))

    if (length(x) > 0) {

      data = data[ -x, ]
      network::delete.vertices(net, x)

      if (!nrow(data)) {

        warning("na.rm removed all nodes; nothing left to plot")
        return(invisible(NULL))

      }

    }

  }

  # -- weight methods ----------------------------------------------------------

  x = size

  if (length(x) == 1 && x %in% c("indegree", "outdegree", "degree", "freeman")) {

    # prevent namespace conflict with igraph
    if ("package:igraph" %in% search()) {

      y = ifelse(is_dir == "digraph", "directed", "undirected")
      z = c("indegree" = "in", "outdegree" = "out", "degree" = "all", "freeman" = "all")[ x ]
      data$size = igraph::degree(igraph::graph.adjacency(as.matrix(net), mode = y), mode = z)

    } else {
      data$size = sna::degree(net, gmode = is_dir, cmode = ifelse(x == "degree", "freeman", x))
    }

    size.legend = ifelse(is.na(size.legend), x, size.legend)

  }

  # -- weight thresholds -------------------------------------------------------

  x = ifelse(is.na(size.min), 0, size.min)

  if (length(x) > 1 || !is.numeric(x) || is.infinite(x) || is.nan(x) || x < 0) {
    stop("incorrect size.min value")
  } else if (x > 0 && !is.numeric(data$size)) {
    warning("node.size is not numeric; size.min ignored")
  } else if (x > 0) {

    x = which(data$size < x)
    message(paste("size.min removed", length(x), "nodes out of", nrow(data)))

    if (length(x) > 0) {

      data = data[ -x, ]
      network::delete.vertices(net, x)

      if (!nrow(data)) {

        warning("size.min removed all nodes; nothing left to plot")
        return(invisible(NULL))

      }

    }

  }

  x = ifelse(is.na(size.max), 0, size.max)

  if (length(x) > 1 || !is.numeric(x) || is.infinite(x) || is.nan(x) || x < 0) {
    stop("incorrect size.max value")
  } else if (x > 0 && !is.numeric(data$size)) {
    warning("node.size is not numeric; size.max ignored")
  } else if (x > 0) {

    x = which(data$size > x)
    message(paste("size.max removed", length(x), "nodes out of", nrow(data)))

    if (length(x) > 0) {

      data = data[ -x, ]
      network::delete.vertices(net, x)

      if (!nrow(data)) {

        warning("size.max removed all nodes; nothing left to plot")
        return(invisible(NULL))

      }

    }

  }

  # -- weight quantiles --------------------------------------------------------

  x = size.cut

  if (length(x) > 1 || is.null(x) || is.na(x) || is.infinite(x) || is.nan(x)) {
    stop("incorrect size.cut value")
  } else if (isTRUE(x)) {
    x = 4
  } else if (is.logical(x) && !x) {
    x = 0
  } else if (!is.numeric(x)) {
    stop("incorrect size.cut value")
  }

  if (x >= 1 && !is.numeric(data$size)) {
    warning("node.size is not numeric; size.cut ignored")
  } else if (x >= 1) {

    x = unique(quantile(data$size, probs = seq(0, 1, by = 1 / as.integer(x))))

    if (length(x) > 1) {
      data$size = cut(data$size, unique(x), include.lowest = TRUE)
    } else {
      warning("node.size is invariant; size.cut ignored")
    }

  }

  # -- alpha palette -----------------------------------------------------------

  if (!is.null(alpha.palette)) {
    x = alpha.palette
  } else if (is.factor(data$alpha)) {
    x = levels(data$alpha)
  } else {
    x = unique(data$alpha)
  }

  if (!is.null(names(x))) {

    y = unique(na.omit(data$alpha[ !data$alpha %in% names(x) ]))

    if (length(y) > 0) {
      stop(paste("no alpha.palette value for", paste0(y, collapse = ", ")))
    }

  } else if (is.factor(data$alpha) || !is.numeric(x)) {

    data$alpha = factor(data$alpha)
    x = scales::rescale_max(1:length(levels(data$alpha)))
    names(x) = levels(data$alpha)

  }

  alpha.palette = x

  # -- color palette -----------------------------------------------------------

  if (!is.null(color.palette)) {
    x = color.palette
  } else if (is.factor(data$color)) {
    x = levels(data$color)
  } else {
    x = unique(data$color)
  }

  if (length(x) == 1 && "RColorBrewer" %in% rownames(installed.packages()) &&
      x %in% rownames(RColorBrewer::brewer.pal.info)) {

    data$color = factor(data$color)

    n_groups = length(levels(data$color))
    n_colors = RColorBrewer::brewer.pal.info[ x, "maxcolors" ]

    if (n_groups > n_colors) {

      stop(paste0("too many node groups (", n_groups, ") for ",
                  "ColorBrewer palette ", x, " (max: ", n_colors, ")"))

    } else if (n_groups < 3) {
      n_groups = 3
    }

    x = RColorBrewer::brewer.pal(n_groups, x)[ 1:length(levels(data$color)) ]
    names(x) = levels(data$color)

  }

  if (!is.null(names(x))) {

    y = unique(na.omit(data$color[ !data$color %in% names(x) ]))

    if (length(y) > 0) {
      stop(paste("no color.palette value for", paste0(y, collapse = ", ")))
    }

  } else if (is.factor(data$color) || !is_col(x)) {

    data$color = factor(data$color)

    x = gray.colors(length(x))
    names(x) = levels(data$color)

  }

  color.palette = x

  # -- shape palette -----------------------------------------------------------

  if (!is.null(shape.palette)) {
    x = shape.palette
  } else if (is.factor(data$shape)) {
    x = levels(data$shape)
  } else {
    x = unique(data$shape)
  }

  if (!is.null(names(x))) {

    y = unique(na.omit(data$shape[ !data$shape %in% names(x) ]))

    if (length(y) > 0) {
      stop(paste("no shape.palette value for", paste0(y, collapse = ", ")))
    }

  } else if (is.factor(data$shape) || !is.numeric(x)) {

    data$shape = factor(data$shape)
    x = scales::shape_pal()(length(levels(data$shape)))
    names(x) = levels(data$shape)

  }

  shape.palette = x

  # -- size palette ------------------------------------------------------------

  if (!is.null(size.palette)) {
    x = size.palette
  } else if (is.factor(data$size)) {
    x = levels(data$size)
  } else {
    x = unique(data$size)
  }

  if (!is.null(names(x))) {

    y = unique(na.omit(data$size[ !data$size %in% names(x) ]))

    if (length(y) > 0) {
      stop(paste("no size.palette value for", paste0(y, collapse = ", ")))
    }

  } else if (is.factor(data$size) || !is.numeric(x)) {

    data$size = factor(data$size)
    x = 1:length(levels(data$size))
    names(x) = levels(data$size)

  }

  size.palette = x

  # -- node labels -------------------------------------------------------------

  l = node.label

  if (isTRUE(l)) {
    l = data$label
  } else if (length(l) > 1 & length(l) == n_nodes) {
    data$label = l
  } else if (length(l) == 1 && l %in% v_attr) {
    l = get_v(net, l)
  } else {
    l = ifelse(data$label %in% l, data$label, "")
  }

  # -- node placement ----------------------------------------------------------

  if (is.character(mode) && length(mode) == 1) {

    mode = paste0("gplot.layout.", mode)
    if (!exists(mode)) {
      stop(paste("unsupported placement method:", mode))
    }

    # sna placement algorithm
    xy = network::as.matrix.network.adjacency(net)
    xy = do.call(mode, list(xy, layout.par))
    xy = data.frame(x = xy[, 1], y = xy[, 2])

  } else if (is.character(mode) && length(mode) == 2) {

    # fixed coordinates from vertex attributes
    xy = data.frame(x = set_attr(mode[1]), y = set_attr(mode[2]))

  } else if (is.numeric(mode) && is.matrix(mode)) {

    # fixed coordinates from matrix
    xy = data.frame(x = set_attr(mode[, 1]), y = set_attr(mode[, 2]))

  } else {

    stop("incorrect mode value")

  }

  xy$x = scale(xy$x, min(xy$x), diff(range(xy$x)))
  xy$y = scale(xy$y, min(xy$y), diff(range(xy$y)))

  data = cbind(data, xy)

  # -- edge colors -------------------------------------------------------------

  edges = network::as.matrix.network.edgelist(net)

  if (edge.color[1] == "color" && length(edge.color) == 2) {

    # edge colors from node source and target
    edge.color = ifelse(data$color[ edges[, 1]] == data$color[ edges[, 2]],
                        as.character(data$color[ edges[, 1]]), edge.color[2])

    if (!is.null(names(color.palette))) {
      x = which(edge.color %in% names(color.palette))
      edge.color[x] = color.palette[ edge.color[x] ]
    }

    edge.color[ is.na(edge.color) ] = edge.color[2]

  }

  edge.color = set_edge(edge.color, "edge.color")

  if (!is_col(edge.color)) {
    stop("incorrect edge.color value")
  }

  # -- edge list ---------------------------------------------------------------

  edges = data.frame(xy[ edges[, 1], ], xy[ edges[, 2], ])
  names(edges) = c("X1", "Y1", "X2", "Y2")

  # -- edge labels, colors and sizes -------------------------------------------

  if (!is.null(edge.label)) {

    edges$midX = (edges$X1 + edges$X2) / 2
    edges$midY = (edges$Y1 + edges$Y2) / 2
    edges$label = set_edge(edge.label, "edge.label")

    edge.label.alpha = set_edge(edge.label.alpha, "edge.label.alpha")

    if (!is.numeric(edge.label.alpha)) {
      stop("incorrect edge.label.alpha value")
    }

    edge.label.color = set_edge(edge.label.color, "edge.label.color")

    if (!is_col(edge.label.color)) {
      stop("incorrect edge.label.color value")
    }

    edge.label.size = set_edge(edge.label.size, "edge.label.size")

    if (!is.numeric(edge.label.size)) {
      stop("incorrect edge.label.size value")
    }

  }

  # -- edge linetype -----------------------------------------------------------

  edge.lty = set_edge(edge.lty, "edge.lty")

  # -- edge size ---------------------------------------------------------------

  edge.size = set_edge(edge.size, "edge.size")

  if (!is.numeric(edge.size) || any(edge.size <= 0)) {
    stop("incorrect edge.size value")
  }

  # -- plot edges --------------------------------------------------------------

  p = ggplot(data, aes(x = x, y = y))

  if (nrow(edges) > 0) {

    if (arrow.gap > 0) {

      x.length = with(edges, abs(X2 - X1))
      y.length = with(edges, abs(Y2 - Y1))

      arrow.gap = with(edges, arrow.gap / sqrt(x.length ^ 2 + y.length ^ 2))

      edges = transform(edges,
                        X1 = X1 + arrow.gap * x.length,
                        Y1 = Y1 + arrow.gap * y.length,
                        X2 = X1 + (1 - arrow.gap) * x.length,
                        Y2 = Y1 + (1 - arrow.gap) * y.length)

    }

    p = p +
      geom_segment(
        data = edges,
        aes(x = X1, y = Y1, xend = X2, yend = Y2),
        size   = edge.size,
        color  = edge.color,
        alpha  = edge.alpha,
        lty    = edge.lty,
        arrow  = arrow(
          type   = arrow.type,
          length = unit(arrow.size, "pt")
        )
      )

  }

  if (nrow(edges) > 0 && !is.null(edge.label)) {

    p = p +
      geom_point(
        data = edges,
        aes(x = midX, y = midY),
        alpha  = edge.alpha,
        color  = edge.label.fill,
        size   = edge.label.size * 1.5
      ) +
      geom_text(
        data = edges,
        aes(x = midX, y = midY, label = label),
        alpha  = edge.label.alpha,
        color  = edge.label.color,
        size   = edge.label.size
      )

  }

  # -- plot nodes --------------------------------------------------------------

  x = list()

  if (is.numeric(data$alpha) && is_one(data$alpha)) {
    x = c(x, alpha = unique(data$alpha))
  }

  if (!is.factor(data$color) && is_one(data$color)) {
    x = c(x, colour = unique(data$color)) # must be English spelling
  }

  if (is.numeric(data$shape) && is_one(data$shape)) {
    x = c(x, shape = unique(data$shape))
  }

  if (is.numeric(data$size) && is_one(data$size)) {
    x = c(x, size = unique(data$size))
  } else {
    x = c(x, size = max_size)
  }

  p = p +
    geom_point(aes(alpha = factor(alpha), color = factor(color),
                   shape = factor(shape), size = factor(size)))

  # -- legend: alpha -----------------------------------------------------------

  if (is.numeric(data$alpha)) {

    v_alpha = unique(data$alpha)
    names(v_alpha) = unique(data$alpha)

    p = p +
      scale_alpha_manual("", values = v_alpha) + guides(alpha = FALSE)

  } else {

    p = p +
      scale_alpha_manual(set_name(node.alpha, alpha.legend),
                         values = alpha.palette,
                         breaks = names(alpha.palette),
                         guide = guide_legend(override.aes = x))

  }

  # -- legend: color -----------------------------------------------------------

  if (!is.null(names(color.palette))) {

    p = p +
      scale_color_manual(set_name(node.color, color.legend),
                         values = color.palette,
                         breaks = names(color.palette),
                         guide = guide_legend(override.aes = x))

  } else {

    v_color = unique(data$color)
    names(v_color) = unique(data$color)

    p = p +
      scale_color_manual("", values = v_color) + guides(color = FALSE)

  }

  # -- legend: shape -----------------------------------------------------------

  if (is.numeric(data$shape)) {

    v_shape = unique(data$shape)
    names(v_shape) = unique(data$shape)

    p = p +
      scale_shape_manual("", values = v_shape) + guides(shape = FALSE)

  } else {

    p = p +
      scale_shape_manual(set_name(node.shape, shape.legend),
                         values = shape.palette,
                         breaks = names(shape.palette),
                         guide = guide_legend(override.aes = x))
  }

  # -- legend: size ------------------------------------------------------------

  x = x[ names(x) != "size" ]

  if (is.numeric(data$size)) {

    v_size = set_size(unique(data$size))

    if (length(v_size) == 1) {

      v_size = as.numeric(names(v_size))
      p = p +
        scale_size_manual("", values = v_size) + guides(size = FALSE)

    } else {

      p = p +
        scale_size_manual(set_name(node.size, size.legend),
                          values = v_size,
                          guide = guide_legend(override.aes = x))

    }

  } else {

    p = p +
      scale_size_manual(set_name(node.size, size.legend),
                        values = set_size(size.palette),
                        guide = guide_legend(override.aes = x))

  }

  # -- plot node labels --------------------------------------------------------

  if (!is_one(l) || unique(l) != "") {

    label.alpha = set_node(label.alpha, "label.alpha", mode = FALSE)

    if (!is.numeric(label.alpha)) {
      stop("incorrect label.alpha value")
    }

    label.color = set_node(label.color, "label.color", mode = FALSE)

    if (!is_col(label.color)) {
      stop("incorrect label.color value")
    }

    label.size = set_node(label.size, "label.size", mode = FALSE)

    if (!is.numeric(label.size)) {
      stop("incorrect label.size value")
    }

    x = label.trim

    if (length(x) > 1 || (!is.logical(x) & !is.numeric(x) & !is.function(x))) {
      stop("incorrect label.trim value")
    } else if (is.numeric(x) && x > 0) {
      l = substr(l, 1, x)
    } else if (is.function(x)) {
      l = x(l)
    }

    p = p +
      geom_text(
        label = l,
        alpha = label.alpha,
        color = label.color,
        size  = label.size,
        ...
      )

  }

  # -- horizontal scale expansion ----------------------------------------------

  x = range(data$x)

  if (!is.numeric(layout.exp) || layout.exp < 0) {
    stop("incorrect layout.exp value")
  } else if (layout.exp > 0) {
    x = scales::expand_range(x, layout.exp / 2)
  }

  # -- finalize ----------------------------------------------------------------

  p = p +
    scale_x_continuous(breaks = NULL, limits = x) +
    scale_y_continuous(breaks = NULL) +
    theme(
      panel.background = element_blank(),
      panel.grid       = element_blank(),
      axis.title       = element_blank(),
      legend.key       = element_blank(),
      legend.position  = legend.position,
      legend.text      = element_text(size = legend.size),
      legend.title     = element_text(size = legend.size)
    )

  return(p)

}
