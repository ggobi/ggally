
context("ggnet2")

if ("package:igraph" %in% search()) {
  detach("package:igraph")
}

rq <- function(...) {
  require(..., quietly = TRUE)
}
rq(network) # network objects
rq(sna)     # placement and centrality

rq(ggplot2) # grammar of graphics
rq(grid)    # arrows
rq(scales)  # sizing

rq(intergraph)   # test igraph conversion
rq(RColorBrewer) # test ColorBrewer palettes

test_that("examples", {

  ### --- start: documented examples

  # random adjacency matrix
  x           <- 10
  ndyads      <- x * (x - 1)
  density     <- x / ndyads
  m           <- matrix(0, nrow = x, ncol = x)
  dimnames(m) <- list(letters[ 1:x ], letters[ 1:x ])
  m[ row(m) != col(m) ] <- runif(ndyads) < density
  m

  # random undirected network
  n <- network::network(m, directed = FALSE)
  n

  ggnet2(n, label = TRUE)
  # ggnet2(n, label = TRUE, shape = 15)
  # ggnet2(n, label = TRUE, shape = 15, color = "black", label.color = "white")

  # add vertex attribute
  x <- network.vertex.names(n) # nolint
  x <- ifelse(x %in% c("a", "e", "i"), "vowel", "consonant")
  n %v% "phono" <- x

  ggnet2(n, color = "phono")
  ggnet2(n, color = "phono", palette = c("vowel" = "gold", "consonant" = "grey"))
  ggnet2(n, shape = "phono", color = "phono")

  # random groups
  n %v% "group" <- sample(LETTERS[1:3], 10, replace = TRUE)
  ggnet2(n, color = "group", palette = "Set2")

  # random weights
  n %e% "weight" <- sample(1:3, network.edgecount(n), replace = TRUE)
  ggnet2(n, edge.size = "weight", edge.label = "weight")

  # Padgett's Florentine wedding data
  data(flo, package = "network")
  flo

  ggnet2(flo, label = TRUE)
  ggnet2(flo, label = TRUE, label.trim = 4, vjust = -1, size = 3, color = 1)
  # ggnet2(flo, label = TRUE, size = 12, color = "white")

  ### --- end: documented examples

  # test node assignment errors
  expect_error(ggnet2(n, color = NA))
  expect_error(ggnet2(n, color = -1))
  expect_error(ggnet2(n, color = rep("red", network.size(n) - 1)))

  # test node assignment
  ggnet2(n, color = rep("red", network.size(n)))

  # test node assignment errors
  expect_error(ggnet2(n, edge.color = NA))
  expect_error(ggnet2(n, edge.color = -1))
  expect_error(ggnet2(n, edge.color = rep("red", network.edgecount(n) - 1)))

  # test edge assignment
  ggnet2(n, edge.color = rep("red", network.edgecount(n)))
  # ggnet2(n, edge.color = "weight")

  # test mode = c("x", "y")
  ggnet2(n, mode = matrix(1, ncol = 2, nrow = 10))
  n %v% "x" <- sample(1:10)
  n %v% "y" <- sample(1:10)
  ggnet2(n, mode = c("x", "y"))
  expect_error(ggnet2(n, mode = c("xx", "yy")), "not found")
  expect_error(ggnet2(n, mode = c("phono", "phono")), "not numeric")
  expect_error(ggnet2(n, mode = matrix(1, ncol = 2, nrow = 9)), "coordinates length")

  # test arrow.size
  expect_error(ggnet2(n, arrow.size = -1), "incorrect arrow.size")
  expect_warning(ggnet2(n, arrow.size = 1), "arrow.size ignored")

  # test arrow.gap

  suppressWarnings(expect_error(
    ggnet(n, arrow.size = 12, arrow.gap = -1),
    "incorrect arrow.gap"
  ))
  suppressWarnings(expect_warning(
    ggnet(n, arrow.size = 12, arrow.gap = 0.1),
    "arrow.gap ignored" # network is undirected; arrow.gap ignored
  ))
  suppressWarnings(expect_warning(
    ggnet(n, arrow.size = 12, arrow.gap = 0.1),
    "arrow.size ignored" # network is undirected; arrow.size ignored
  ))

  m <- network::network(m, directed = TRUE)
  ggnet2(m, arrow.size = 12, arrow.gap = 0.05)

  # test max_size
  expect_error(ggnet2(n, max_size = NA), "incorrect max_size")

  # test na.rm
  expect_error(ggnet2(n, na.rm = 1:2), "incorrect na.rm")
  expect_error(ggnet2(n, na.rm = "xyz"), "not found")

  n %v% "missing" <- ifelse(n %v% "phono" == "vowel", NA, n %v% "phono")
  expect_message(ggnet2(n, na.rm = "missing"), "removed")

  n %v% "missing" <- NA
  expect_warning(ggnet2(n, na.rm = "missing"), "removed all nodes")

  # test size = "degree"
  ggnet2(n, size = "degree")

  # test size.min
  expect_error(ggnet2(n, size = "degree", size.min = -1), "incorrect size.min")
  expect_message(ggnet2(n, size = "degree", size.min = 1), "size.min removed")
  expect_warning(ggnet2(n, size = "abc", size.min = 1), "not numeric")
  expect_warning(ggnet2(n, size = 4, size.min = 5), "removed all nodes")

  # test size.max
  expect_error(ggnet2(n, size = "degree", size.max = -1), "incorrect size.max")
  expect_message(ggnet2(n, size = "degree", size.max = 99), "size.max removed")
  expect_warning(ggnet2(n, size = "abc", size.max = 1), "not numeric")
  expect_warning(ggnet2(n, size = 4, size.max = 3), "removed all nodes")

  # test size.cut
  ggnet2(n, size = 1:10, size.cut = 3)
  ggnet2(n, size = 1:10, size.cut = TRUE)
  expect_error(ggnet2(n, size = 1:10, size.cut = NA), "incorrect size.cut")
  expect_error(ggnet2(n, size = 1:10, size.cut = "xyz"), "incorrect size.cut")
  expect_warning(ggnet2(n, size = "abc", size.cut = 3), "not numeric")
  expect_warning(ggnet2(n, size = 1, size.cut = 3), "ignored")

  # test alpha.palette
  ggnet2(n, alpha = "phono", alpha.palette = c("vowel" = 1, "consonant" = 0.5))
  ggnet2(n, alpha = factor(1:10))
  expect_error(
    ggnet2(n, alpha = "phono", alpha.palette = c("vowel" = 1)),
    "no alpha.palette value"
  )

  # test color.palette
  # ggnet2(n, color = "phono", color.palette = c("vowel" = 1, "consonant" = 2))
  ggnet2(n, color = factor(1:10))
  ggnet2(n, color = "phono", palette = "Set1") # only 2 groups, palette has min. 3
  expect_error(ggnet2(n, color = factor(1:10), palette = "Set1"), "too many node groups")
  expect_error(
    ggnet2(n, color = "phono", color.palette = c("vowel" = 1)),
    "no color.palette value"
  )

  # test shape.palette
  ggnet2(n, shape = "phono", shape.palette = c("vowel" = 15, "consonant" = 19))
  expect_warning(ggnet2(n, shape = factor(1:10)), "discrete values")
  expect_error(
    ggnet2(n, shape = "phono", shape.palette = c("vowel" = 1)),
    "no shape.palette value"
  )

  # test size.palette
  ggnet2(n, size = "phono", size.palette = c("vowel" = 1, "consonant" = 2))
  ggnet2(n, size = factor(1:10))
  expect_error(ggnet2(n, size = "phono", size.palette = c("vowel" = 1)), "no size.palette value")

  # test node.label
  ggnet2(n, label = sample(letters, 10))
  ggnet2(n, label = "phono")

  # test label.alpha
  expect_error(ggnet2(n, label = TRUE, label.alpha = "xyz"), "incorrect label.alpha")

  # test label.color
  expect_error(ggnet2(n, label = TRUE, label.color = "xyz"), "incorrect label.color")

  # test label.size
  expect_error(ggnet2(n, label = TRUE, label.size = "xyz"), "incorrect label.size")

  # test label.trim
  expect_error(ggnet2(n, label = TRUE, label.trim = "xyz"), "incorrect label.trim")
  ggnet2(n, label = TRUE, label.trim = toupper)

  # test mode
  expect_error(ggnet2(n, mode = "xyz"), "unsupported")
  expect_error(ggnet2(n, mode = letters[1:3]), "incorrect mode")

  # test edge.node shared colors
  ggnet2(n, color = "phono", edge.color = c("color", "grey"))

  # test edge.color
  expect_error(ggnet2(n, edge.color = "xyz"), "incorrect edge.color")

  # test edge.label.alpha
  expect_error(
    ggnet2(n, edge.label = "xyz", edge.label.alpha = "xyz"),
    "incorrect edge.label.alpha"
  )

  # test edge.label.color
  expect_error(
    ggnet2(n, edge.label = "xyz", edge.label.color = "xyz"),
    "incorrect edge.label.color"
  )

  # test edge.label.size
  expect_error(ggnet2(n, edge.label = "xyz", edge.label.size = "xyz"), "incorrect edge.label.size")

  # test edge.size
  expect_error(ggnet2(n, edge.size = "xyz"), "incorrect edge.size")

  # test layout.exp
  expect_error(ggnet2(n, layout.exp = "xyz"))
  ggnet2(n, layout.exp = 0.1)

  ### --- test bipartite functionality

  # weighted adjacency matrix
  bip <- data.frame(
    event1 = c(1, 2, 1, 0),
    event2 = c(0, 0, 3, 0),
    event3 = c(1, 1, 0, 4),
    row.names = letters[1:4]
  )

  # weighted bipartite network
  bip <- network(
    bip,
    matrix.type = "bipartite",
    ignore.eval = FALSE,
    names.eval = "weights"
  )

  # test bipartite mode
  ggnet2(bip, color = "mode")

  ### --- test network coercion

  expect_warning(ggnet2(network(matrix(1, nrow = 2, ncol = 2), loops = TRUE)), "self-loops")

  expect_error(ggnet2(1:2), "network object")
  expect_error(ggnet2(network(data.frame(1:2, 3:4), hyper = TRUE)), "hyper graphs")
  expect_error(ggnet2(network(data.frame(1:2, 3:4), multiple = TRUE)), "multiplex graphs")

  ### --- test igraph functionality

  # test igraph conversion
  p <- ggnet2(asIgraph(n), color = "group")
  expect_null(p$guides$colour)

  # test igraph degree
  library(igraph)
  ggnet2(n, size = "degree")

  expect_true(TRUE)

})
