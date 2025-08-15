#' Network plot
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Function for plotting network objects using \pkg{ggplot2}, now replaced by the
#' \code{\link{ggnet2}} function, which provides additional control over
#' plotting parameters. Please visit \url{https://github.com/briatte/ggnet} for
#' the latest version of ggnet2, and \url{https://briatte.github.io/ggnet/} for a
#' vignette that contains many examples and explanations.
#'
#' @export
#' @param net an object of class \code{\link[network]{network}}, or any object
#' that can be coerced to this class, such as an adjacency or incidence matrix,
#' or an edge list: see \link[network]{edgeset.constructors} and
#' \link[network]{network} for details. If the object is of class
#' [igraph][igraph::igraph-package] and the
#' [intergraph][intergraph::intergraph-package] package is installed,
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
#' @param size size of the network nodes. If the nodes are weighted, their area is proportionally scaled up to the size set by \code{size}.
#' Defaults to \code{9}.
#' @param alpha a level of transparency for nodes, vertices and arrows.
#' Defaults to \code{1}.
#' @param weight the weighting method for the nodes, which might be a vertex
#' attribute or a vector of size values. Also accepts \code{"indegree"},
#' \code{"outdegree"}, \code{"degree"} or \code{"freeman"} to size the nodes by
#' their unweighted degree centrality (\code{"degree"} and \code{"freeman"} are
#' equivalent): see \code{\link[sna]{degree}} for details. All node weights must
#' be positive.
#' Defaults to \code{"none"} (no weighting).
#' @param weight.method see \code{weight}
#' @param weight.legend the name to assign to the legend created by
#' \code{weight}.
#' Defaults to \code{NA} (no name).
#' @param weight.min whether to subset the network to nodes with a minimum size,
#' based on the values of \code{weight}.
#' Defaults to \code{NA} (preserves all nodes).
#' @param weight.max whether to subset the network to nodes with a maximum size,
#' based on the values of \code{weight}.
#' Defaults to \code{NA} (preserves all nodes).
#' @param weight.cut whether to cut the size of the nodes into a certain number
#' of quantiles. Accepts \code{TRUE}, which tries to cut the sizes into
#' quartiles, or any positive numeric value, which tries to cut the sizes into
#' that many quantiles. If the size of the nodes do not contain the specified
#' number of distinct quantiles, the largest possible number is used.
#' See \code{\link[stats]{quantile}} and \code{\link[base]{cut}} for details.
#' Defaults to \code{FALSE} (does nothing).
#' @param group the groups of the nodes, either as a vector of values or as a
#' vertex attribute. If set to \code{mode} on a bipartite network, the nodes
#' will be grouped as \code{"actor"} if they belong to the primary mode and
#' \code{"event"} if they belong to the secondary mode.
#' @param group.legend the name to assign to the legend created by
#' \code{group}.
#' @param node.group see \code{group}
#' @param node.color a vector of character strings to color the nodes with,
#' holding as many colors as there are levels in \code{node.group}.
#' Defaults to \code{NULL}, which will assign grayscale colors to each group.
#' @param node.alpha transparency of the nodes. Inherits from \code{alpha}.
#' @param segment.alpha the level of transparency of the edges.
#' Defaults to \code{alpha}, which defaults to \code{1}.
#' @param segment.color the color of the edges, as a color value, a vector of
#' color values, or as an edge attribute containing color values.
#' Defaults to \code{"grey50"}.
#' @param segment.size the size of the edges, in points, as a single numeric
#' value, a vector of values, or as an edge attribute.
#' Defaults to \code{0.25}.
#' @param segment.label the labels to plot at the middle of the edges, as a
#' single value, a vector of values, or as an edge attribute.
#' Defaults to \code{NULL} (no edge labels).
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
#' @param label whether to label the nodes. If set to \code{TRUE}, nodes are
#' labeled with their vertex names. If set to a vector that contains as many
#' elements as there are nodes in \code{net}, nodes are labeled with these. If
#' set to any other vector of values, the nodes are labeled only when their
#' vertex name matches one of these values.
#' Defaults to \code{FALSE} (no labels).
#' @param label.nodes see \code{label}
#' @param label.size the size of the node labels, in points, as a numeric value,
#' a vector of numeric values, or as a vertex attribute containing numeric
#' values.
#' Defaults to \code{size / 2} (half the maximum node size), which defaults to
#' \code{6}.
#' @param label.trim whether to apply some trimming to the node labels. Accepts
#' any function that can process a character vector, or a strictly positive
#' numeric value, in which case the labels are trimmed to a fixed-length
#' substring of that length: see \code{\link[base]{substr}} for details.
#' Defaults to \code{FALSE} (does nothing).
#' @param legend.size the size of the legend symbols and text, in points.
#' Defaults to \code{9}.
#' @param legend.position the location of the plot legend(s). Accepts all
#' \code{legend.position} values supported by \code{\link[ggplot2]{theme}}.
#' Defaults to \code{"right"}.
#' @param names `r lifecycle::badge("deprecated")` see \code{group.legend} and \code{size.legend}
#' @param quantize.weights `r lifecycle::badge("deprecated")` see \code{weight.cut}
#' @param subset.threshold `r lifecycle::badge("deprecated")` see \code{weight.min}
#' @param top8.nodes `r lifecycle::badge("deprecated")` this functionality was experimental and has
#' been removed entirely from \code{ggnet}
#' @param trim.labels `r lifecycle::badge("deprecated")` see \code{label.trim}
#' @param ... other arguments passed to the \code{geom_text} object that sets
#' the node labels: see \code{\link[ggplot2]{geom_text}} for details.
#' @seealso \code{\link{ggnet2}} in this package,
#' \code{\link[sna]{gplot}} in the \code{\link[sna]{sna}} package, and
#' \code{\link[network]{plot.network}} in the \code{\link[network]{network}}
#' package
#' @author Moritz Marbach and Francois Briatte, with help from Heike Hofmann,
#' Pedro Jordano and Ming-Yu Liu
#' @details The degree centrality measures that can be produced through the
#' \code{weight} argument will take the directedness of the network into account,
#' but will be unweighted. To compute weighted network measures, see the
#' \code{tnet} package by Tore Opsahl (\code{help("tnet", package = "tnet")}).
#' @importFrom stats quantile na.omit
#' @importFrom utils head installed.packages
#' @importFrom grDevices gray.colors
#' @keywords internal
#' @examples
#' # Small function to display plots only if it's interactive
#' p_ <- GGally::print_if_interactive
#'
#' library(network)
#'
#' # random adjacency matrix
#' x           <- 10
#' ndyads      <- x * (x - 1)
#' density     <- x / ndyads
#' m           <- matrix(0, nrow = x, ncol = x)
#' dimnames(m) <- list(letters[1:x], letters[1:x])
#' m[row(m) != col(m)] <- runif(ndyads) < density
#' m
#'
#' # random undirected network
#' n <- network::network(m, directed = FALSE)
#' n
#'
#' ggnet(n, label = TRUE, alpha = 1, color = "white", segment.color = "black")
#'
#' # random groups
#' g <- sample(letters[1:3], 10, replace = TRUE)
#' g
#'
#' # color palette
#' p <- c("a" = "steelblue", "b" = "forestgreen", "c" = "tomato")
#'
#' p_(ggnet(n, node.group = g, node.color = p, label = TRUE, color = "white"))
#'
#' # edge arrows on a directed network
#' p_(ggnet(network(m, directed = TRUE), arrow.gap = 0.05, arrow.size = 10))
ggnet <- function(
  net,
  mode = "fruchtermanreingold",
  layout.par = NULL,
  layout.exp = 0,
  size = 9,
  alpha = 1,
  weight = "none",
  weight.legend = NA,
  weight.method = weight,
  weight.min = NA,
  weight.max = NA,
  weight.cut = FALSE,
  group = NULL,
  group.legend = NA,
  node.group = group,
  node.color = NULL,
  node.alpha = alpha,
  segment.alpha = alpha,
  segment.color = "grey50",
  segment.label = NULL,
  segment.size = 0.25,
  arrow.size = 0,
  arrow.gap = 0,
  arrow.type = "closed",
  label = FALSE,
  label.nodes = label,
  label.size = size / 2,
  label.trim = FALSE,
  legend.size = 9,
  legend.position = "right",
  # -- deprecated arguments ----------------------------------------------------
  names = deprecated(),
  quantize.weights = deprecated(),
  subset.threshold = deprecated(),
  top8.nodes = deprecated(),
  trim.labels = deprecated(),
  ...
) {
  lifecycle::deprecate_soft("2.2.2", "ggnet()", "ggnet2()")

  # -- packages ----------------------------------------------------------------

  rlang::check_installed(c("network", "sna", "scales"))
  # -- deprecations ------------------------------------------------------------

  if (length(mode) == 1 && mode == "geo") {
    lifecycle::deprecate_warn(
      when = "2.3.0",
      what = "ggnet(mode='cannot be `geo`')",
      details = "Please use mode = c('lon', 'lat') instead"
    )
    mode = c("lon", "lat")
  }

  if (lifecycle::is_present(names)) {
    lifecycle::deprecate_warn(
      when = "2.3.0",
      what = "ggnet(names)",
      details = "Please use group.legend and size.legend instead"
    )
    group.legend = names[1]
    size.legend = names[2]
  }

  if (lifecycle::is_present(quantize.weights)) {
    lifecycle::deprecate_warn(
      when = "2.3.0",
      what = "ggnet(quantize.weights)",
      details = "Please use weight.cut instead"
    )
    weight.cut = quantize.weights
  }

  if (lifecycle::is_present(subset.threshold)) {
    lifecycle::deprecate_warn(
      when = "2.3.0",
      what = "ggnet(subset.threshold)",
      details = "Please use weight.min instead"
    )
    weight.min = subset.threshold
  }

  if (lifecycle::is_present(top8.nodes)) {
    lifecycle::deprecate_warn(
      when = "2.3.0",
      what = "ggnet(top8.nodes)"
    )
  }

  if (lifecycle::is_present(trim.labels)) {
    lifecycle::deprecate_warn(
      when = "2.3.0",
      what = "ggnet(trim.labels)",
      details = "Please use label.trim instead"
    )
    label.trim = function(x) gsub("^@|^http://(www\\.)?|/$", "", x)
  }

  # -- conversion to network class ---------------------------------------------

  if (
    inherits(net, "igraph") && "intergraph" %in% rownames(installed.packages())
  ) {
    net = intergraph::asNetwork(net)
  } else if (inherits(net, "igraph")) {
    cli::cli_abort(
      "install the {.pkg intergraph} package to use {.pkg igraph} objects with {.pkg ggnet}"
    )
  }

  if (!network::is.network(net)) {
    net = try(network::network(net), silent = TRUE)
  }

  if (!network::is.network(net)) {
    cli::cli_abort("could not coerce {.arg net} to a {.pkg network} object")
  }

  # -- network functions -------------------------------------------------------

  get_v = utils::getFromNamespace("%v%", ns = "network")
  get_e = utils::getFromNamespace("%e%", ns = "network")

  set_mode = function(
    x,
    mode = network::get.network.attribute(x, "bipartite")
  ) {
    c(rep("actor", mode), rep("event", n_nodes - mode))
  }

  set_node = function(x, value, mode = TRUE) {
    if (is.null(x) || any(is.na(x)) || any(is.infinite(x)) || any(is.nan(x))) {
      cli::cli_abort("incorrect {value} value")
    } else if (is.numeric(x) && any(x < 0)) {
      cli::cli_abort("incorrect {value} value")
    } else if (length(x) == n_nodes) {
      x
    } else if (length(x) > 1) {
      cli::cli_abort("incorrect {value} length")
    } else if (any(x %in% v_attr)) {
      get_v(net, x)
    } else if (mode && identical(x, "mode") && is_bip) {
      set_mode(net)
    } else {
      x
    }
  }

  set_edge = function(x, value) {
    if (is.null(x) || any(is.na(x)) || any(is.infinite(x)) || any(is.nan(x))) {
      cli::cli_abort("incorrect {value} value")
    } else if (is.numeric(x) && any(x < 0)) {
      cli::cli_abort("incorrect {value} value")
    } else if (length(x) == n_edges) {
      x
    } else if (length(x) > 1) {
      cli::cli_abort("incorrect {value} length")
    } else if (any(x %in% e_attr)) {
      get_e(net, x)
    } else {
      x
    }
  }

  set_attr = function(x) {
    if (length(x) == n_nodes) {
      x
    } else if (length(x) > 1) {
      cli::cli_abort("incorrect coordinates length")
    } else if (!x %in% v_attr) {
      cli::cli_abort("vertex attribute {x} was not found")
    } else if (!is.numeric(get_v(net, x))) {
      cli::cli_abort("vertex attribute {x} is not numeric")
    } else {
      get_v(net, x)
    }
  }

  set_name = function(x, y) ifelse(length(x) == 1, x, ifelse(is.na(y), "", y))

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
    cli::cli_abort("incorrect {.arg arrow.size} value")
  } else if (arrow.size > 0 && is_dir == "graph") {
    cli::cli_warn("network is undirected; {.arg arrow.size} ignored")
    arrow.size = 0
  }

  if (!is.numeric(arrow.gap) || arrow.gap < 0 || arrow.gap > 1) {
    cli::cli_abort("incorrect {.arg arrow.gap} value")
  } else if (arrow.gap > 0 && is_dir == "graph") {
    cli::cli_warn("network is undirected; {.arg arrow.gap} ignored")
    arrow.gap = 0
  }

  if (network::is.hyper(net)) {
    cli::cli_abort("{.pkg ggnet} cannot plot hyper graphs")
  }

  if (network::is.multiplex(net)) {
    cli::cli_abort("{.pkg ggnet} cannot plot multiplex graphs")
  }

  if (network::has.loops(net)) {
    cli::cli_warn("{.pkg ggnet} does not know how to handle self-loops")
  }

  # -- check size --------------------------------------------------------------

  x = size

  if (!is.numeric(x) || is.infinite(x) || is.nan(x) || x < 0 || length(x) > 1) {
    cli::cli_abort("incorrect {.arg size} value")
  }

  # -- initialize dataset ------------------------------------------------------

  data = data.frame(
    label = get_v(net, "vertex.names"),
    stringsAsFactors = FALSE
  )

  # -- weight methods ----------------------------------------------------------

  x = weight.method

  if (
    length(x) == 1 && x %in% c("indegree", "outdegree", "degree", "freeman")
  ) {
    # prevent namespace conflict with igraph
    if ("package:igraph" %in% search()) {
      y = ifelse(is_dir == "digraph", "directed", "undirected")
      z = c(
        "indegree" = "in",
        "outdegree" = "out",
        "degree" = "all",
        "freeman" = "all"
      )[x]
      data$weight = igraph::degree(
        igraph_graph_adjacency_matrix(as.matrix(net), mode = y),
        mode = z
      )
    } else {
      data$weight = sna::degree(
        net,
        gmode = is_dir,
        cmode = ifelse(x == "degree", "freeman", x)
      )
    }
  } else if (length(x) > 1 && length(x) == n_nodes) {
    data$weight = x
  } else if (length(x) == 1 && x %in% v_attr) {
    data$weight = get_v(net, x)
  }

  if (!is.null(data$weight) && !is.numeric(data$weight)) {
    cli::cli_abort("incorrect {.arg weight.method} value")
  }

  # -- weight thresholds -------------------------------------------------------

  x = ifelse(is.na(weight.min), 0, weight.min)

  if (length(x) > 1 || !is.numeric(x) || is.infinite(x) || is.nan(x) || x < 0) {
    cli::cli_abort("incorrect {.arg weight.min} value")
  } else if (x > 0) {
    x = which(data$weight < x)
    cli::cli_inform(
      "{.arg weight.min} removed {length(x)} nodes out of {nrow(data)}"
    )

    if (length(x) > 0) {
      data = data[-x, ]
      network::delete.vertices(net, x)

      if (!nrow(data)) {
        cli::cli_warn(
          "{.arg weight.min} removed all nodes; nothing left to plot"
        )
        return(invisible(NULL))
      }
    }
  }

  x = ifelse(is.na(weight.max), 0, weight.max)

  if (length(x) > 1 || !is.numeric(x) || is.infinite(x) || is.nan(x) || x < 0) {
    cli::cli_abort("incorrect {.arg weight.max} value")
  } else if (x > 0) {
    x = which(data$weight > x)
    cli::cli_inform(
      "{.arg weight.max} removed {length(x)} nodes out of {nrow(data)}"
    )

    if (length(x) > 0) {
      data = data[-x, ]
      network::delete.vertices(net, x)

      if (!nrow(data)) {
        cli::cli_warn(
          "{.arg weight.max} removed all nodes; nothing left to plot"
        )
        return(invisible(NULL))
      }
    }
  }

  # -- weight quantiles --------------------------------------------------------

  x = weight.cut

  if (length(x) > 1 || is.null(x) || is.na(x) || is.infinite(x) || is.nan(x)) {
    cli::cli_abort("incorrect {.arg weight.cut} value")
  } else if (isTRUE(x)) {
    x = 4
  } else if (is.logical(x) && !x) {
    x = 0
  } else if (!is.numeric(x)) {
    cli::cli_abort("incorrect {.arg weight.cut} value")
  }

  if (x >= 1) {
    x = unique(quantile(data$weight, probs = seq(0, 1, by = 1 / as.integer(x))))

    if (length(x) > 1) {
      data$weight = cut(data$weight, unique(x), include.lowest = TRUE)
    } else {
      cli::cli_warn("node weight is invariant; {.arg weight.cut} ignored")
    }
  }

  # -- node sizing -------------------------------------------------------------

  if (is.factor(data$weight)) {
    sizer = scale_size_area(
      set_name(weight.method, weight.legend),
      max_size = size,
      breaks = sort(unique(as.integer(data$weight))),
      labels = levels(data$weight)[sort(unique(as.integer(data$weight)))]
    )
    data$weight = as.integer(data$weight)
  } else {
    sizer = scale_size_area(
      set_name(weight.method, weight.legend),
      max_size = size
    )
  }

  # -- node grouping -----------------------------------------------------------

  if (!is.null(node.group)) {
    data$group = factor(set_node(node.group, "node.group"))

    x = length(unique(na.omit(data$group)))

    if (length(node.color) != x) {
      if (!is.null(node.color)) {
        cli::cli_warn(
          "node groups and colors are of unequal length; using grayscale colors"
        )
      }

      node.color = gray.colors(x)
      names(node.color) = unique(na.omit(data$group))
    }
  }

  # -- node labels -------------------------------------------------------------

  l = label.nodes

  if (isTRUE(l)) {
    l = data$label
  } else if (length(l) > 1 && length(l) == n_nodes) {
    data$label = l
  } else if (length(l) == 1 && l %in% v_attr) {
    l = get_v(net, l)
  } else {
    l = ifelse(data$label %in% l, data$label, "")
  }

  # -- node placement ----------------------------------------------------------

  if (is.character(mode) && length(mode) == 1) {
    mode = paste0("gplot.layout.", mode)

    snaNamespace = asNamespace("sna")
    if (!exists(mode, envir = snaNamespace)) {
      cli::cli_abort("unsupported placement method: {.val {mode}}")
    }
    mode = get(mode, envir = snaNamespace)

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
    cli::cli_abort("incorrect {.arg mode} value")
  }

  xy$x = scale(xy$x, min(xy$x), diff(range(xy$x)))[, 1]
  xy$y = scale(xy$y, min(xy$y), diff(range(xy$y)))[, 1]

  data = cbind(data, xy)

  # -- edge list ---------------------------------------------------------------

  edges = network::as.matrix.network.edgelist(net)
  edges = data.frame(xy[edges[, 1], ], xy[edges[, 2], ])
  names(edges) = c("X1", "Y1", "X2", "Y2")

  # -- edge labels -------------------------------------------------------------

  if (!is.null(segment.label)) {
    edges$midX = (edges$X1 + edges$X2) / 2
    edges$midY = (edges$Y1 + edges$Y2) / 2
    edges$label = set_edge(segment.label, "segment.label")
  }

  # -- plot edges --------------------------------------------------------------

  p = ggplot(data, aes(x = .data$x, y = .data$y))

  if (nrow(edges) > 0) {
    if (arrow.gap > 0) {
      x.length = with(edges, abs(X2 - X1))
      y.length = with(edges, abs(Y2 - Y1))

      arrow.gap = with(edges, arrow.gap / sqrt(x.length^2 + y.length^2))

      edges$X1 = edges$X1 + arrow.gap * x.length
      edges$Y1 = edges$Y1 + arrow.gap * y.length
      edges$X2 = edges$X1 + (1 - arrow.gap) * x.length
      edges$Y2 = edges$Y1 + (1 - arrow.gap) * y.length
    }

    p = p +
      geom_segment(
        data = edges,
        aes(x = .data$X1, y = .data$Y1, xend = .data$X2, yend = .data$Y2),
        alpha = segment.alpha,
        linewidth = segment.size,
        color = segment.color,
        arrow = arrow(
          type = arrow.type,
          length = unit(arrow.size, "pt")
        )
      )
  }

  if (nrow(edges) > 0 && !is.null(segment.label)) {
    p = p +
      geom_point(
        data = edges,
        aes(x = .data$midX, y = .data$midY),
        color = "white",
        size = size
      ) +
      geom_text(
        data = edges,
        aes(x = .data$midX, y = .data$midY, label = label),
        alpha = segment.alpha,
        color = segment.color,
        size = size / 2
      )
  }

  # -- plot nodes --------------------------------------------------------------

  if (length(weight.method) == 1 && weight.method == "none") {
    p = p +
      geom_point(
        alpha = node.alpha,
        size = size
      )
  } else {
    p = p +
      geom_point(
        aes(size = .data$weight),
        alpha = node.alpha
      ) +
      sizer
  }

  # -- plot node colors --------------------------------------------------------

  if (!is.null(node.group)) {
    p = p +
      aes(color = .data$group) +
      scale_color_manual(
        set_name(node.group, group.legend),
        values = node.color,
        guide = guide_legend(override.aes = list(size = legend.size))
      )
  }

  # -- plot node labels --------------------------------------------------------

  if (!is_one(l) || unique(l) != "") {
    label.size = set_node(label.size, "label.size", mode = FALSE)

    if (!is.numeric(label.size)) {
      cli::cli_abort("incorrect {.arg label.size} value")
    }

    x = label.trim

    if (
      length(x) > 1 || (!is.logical(x) && !is.numeric(x) && !is.function(x))
    ) {
      cli::cli_abort("incorrect {.arg label.trim} value")
    } else if (is.numeric(x) && x > 0) {
      l = substr(l, 1, x)
    } else if (is.function(x)) {
      l = x(l)
    }

    p = p +
      geom_text(
        label = l,
        size = label.size,
        show.legend = FALSE, # required by ggplot2 >= 1.0.1.9003
        ...
      )
  }

  # -- horizontal scale expansion ----------------------------------------------

  x = range(data$x)

  if (!is.numeric(layout.exp) || layout.exp < 0) {
    cli::cli_abort("incorrect {.arg layout.exp} value")
  } else if (layout.exp > 0) {
    x = scales::expand_range(x, layout.exp / 2)
  }

  # -- finalize ----------------------------------------------------------------

  p = p +
    scale_x_continuous(breaks = NULL, limits = x) +
    scale_y_continuous(breaks = NULL) +
    theme(
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.title = element_blank(),
      legend.key = element_blank(),
      legend.position = legend.position,
      legend.text = element_text(size = legend.size),
      legend.title = element_text(size = legend.size)
    )

  return(p)
}


igraph_graph_adjacency_matrix <- function(...) {
  if (utils::packageVersion("igraph") >= "2.0.0") {
    igraph::graph_from_adjacency_matrix(...)
  } else {
    igraph::graph.adjacency(...)
  }
}
