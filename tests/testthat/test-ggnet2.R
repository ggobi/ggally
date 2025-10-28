if ("package:igraph" %in% search()) {
  detach("package:igraph")
}

skip_if_not(rq(network)) # network objects
skip_if_not(rq(sna)) # placement and centrality

skip_if_not(rq(ggplot2)) # grammar of graphics
skip_if_not(rq(grid)) # arrows
skip_if_not(rq(scales)) # sizing

skip_if_not(rq(intergraph)) # test igraph conversion
skip_if_not(rq(RColorBrewer)) # test ColorBrewer palettes

test_that("examples", {
  skip_if_not_installed("network")
  ### --- start: documented examples
  set.seed(54321)

  # random adjacency matrix
  x <- 10
  ndyads <- x * (x - 1)
  density <- x / ndyads
  m <- matrix(0, nrow = x, ncol = x)
  dimnames(m) <- list(letters[1:x], letters[1:x])
  m[row(m) != col(m)] <- runif(ndyads) < density
  m

  # random undirected network
  n <- network::network(m, directed = FALSE)
  n

  ggnet2(n, label = TRUE)
  # ggnet2(n, label = TRUE, shape = 15)
  # ggnet2(n, label = TRUE, shape = 15, color = "black", label.color = "white")

  # add vertex attribute
  x <- network.vertex.names(n)
  x <- ifelse(x %in% c("a", "e", "i"), "vowel", "consonant")
  n %v% "phono" <- x

  ggnet2(n, color = "phono")
  ggnet2(
    n,
    color = "phono",
    palette = c("vowel" = "gold", "consonant" = "grey")
  )
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
  expect_snapshot(ggnet2(n, color = NA), error = TRUE)
  expect_snapshot(ggnet2(n, color = -1), error = TRUE)
  expect_snapshot(
    ggnet2(n, color = rep("red", network.size(n) - 1)),
    error = TRUE
  )

  # test node assignment
  ggnet2(n, color = rep("red", network.size(n)))

  # test node assignment errors
  expect_snapshot(ggnet2(n, edge.color = NA), error = TRUE)
  expect_snapshot(ggnet2(n, edge.color = -1), error = TRUE)
  expect_snapshot(
    ggnet2(n, edge.color = rep("red", network.edgecount(n) - 1)),
    error = TRUE
  )

  # test edge assignment
  ggnet2(n, edge.color = rep("red", network.edgecount(n)))
  # ggnet2(n, edge.color = "weight")

  # test mode = c("x", "y")
  ggnet2(n, mode = matrix(1, ncol = 2, nrow = 10))
  n %v% "x" <- sample(1:10)
  n %v% "y" <- sample(1:10)
  ggnet2(n, mode = c("x", "y"))
  expect_snapshot(ggnet2(n, mode = c("xx", "yy")), error = TRUE)
  expect_snapshot(ggnet2(n, mode = c("phono", "phono")), error = TRUE)
  expect_snapshot(ggnet2(n, mode = matrix(1, ncol = 2, nrow = 9)), error = TRUE)

  # test arrow.size
  expect_snapshot(ggnet2(n, arrow.size = -1), error = TRUE)
  expect_warning(ggnet2(n, arrow.size = 1), "`arrow.size` ignored")

  # test arrow.gap

  suppressWarnings(expect_snapshot(
    ggnet(n, arrow.size = 12, arrow.gap = -1),
    error = TRUE
  ))
  suppressWarnings(expect_warning(
    ggnet(n, arrow.size = 12, arrow.gap = 0.1),
    "`arrow.gap` ignored" # network is undirected; arrow.gap ignored
  ))
  suppressWarnings(expect_warning(
    ggnet(n, arrow.size = 12, arrow.gap = 0.1),
    "`arrow.size` ignored" # network is undirected; arrow.size ignored
  ))

  m <- network::network(m, directed = TRUE)
  ggnet2(m, arrow.size = 12, arrow.gap = 0.05)

  # test max_size
  expect_snapshot(ggnet2(n, max_size = NA), error = TRUE)

  # test na.rm
  expect_snapshot(ggnet2(n, na.rm = 1:2), error = TRUE)
  expect_snapshot(ggnet2(n, na.rm = "xyz"), error = TRUE)

  n %v% "missing" <- ifelse(n %v% "phono" == "vowel", NA, n %v% "phono")
  expect_message(ggnet2(n, na.rm = "missing"), "removed")

  n %v% "missing" <- NA
  suppressMessages({
    expect_warning(ggnet2(n, na.rm = "missing"), "removed all nodes")
  })

  # test size = "degree"
  ggnet2(n, size = "degree")

  # test size.min
  expect_snapshot(ggnet2(n, size = "degree", size.min = -1), error = TRUE)
  expect_message(ggnet2(n, size = "degree", size.min = 1), "`size.min` removed")
  suppressMessages({
    expect_warning(ggnet2(n, size = "abc", size.min = 1), "not numeric")
    expect_warning(ggnet2(n, size = 4, size.min = 5), "removed all nodes")
  })

  # test size.max
  expect_snapshot(ggnet2(n, size = "degree", size.max = -1), error = TRUE)
  expect_message(
    ggnet2(n, size = "degree", size.max = 99),
    "`size.max` removed"
  )
  suppressMessages({
    expect_warning(ggnet2(n, size = "abc", size.max = 1), "not numeric")
    expect_warning(ggnet2(n, size = 4, size.max = 3), "removed all nodes")
  })

  # test size.cut
  ggnet2(n, size = 1:10, size.cut = 3)
  ggnet2(n, size = 1:10, size.cut = TRUE)
  expect_snapshot(ggnet2(n, size = 1:10, size.cut = NA), error = TRUE)
  expect_snapshot(ggnet2(n, size = 1:10, size.cut = "xyz"), error = TRUE)
  expect_warning(ggnet2(n, size = "abc", size.cut = 3), "not numeric")
  expect_warning(ggnet2(n, size = 1, size.cut = 3), "ignored")

  # test alpha.palette
  ggnet2(n, alpha = "phono", alpha.palette = c("vowel" = 1, "consonant" = 0.5))
  ggnet2(n, alpha = factor(1:10))
  expect_snapshot(
    ggnet2(n, alpha = "phono", alpha.palette = c("vowel" = 1)),
    error = TRUE
  )

  # test color.palette
  # ggnet2(n, color = "phono", color.palette = c("vowel" = 1, "consonant" = 2))
  ggnet2(n, color = factor(1:10))
  ggnet2(n, color = "phono", palette = "Set1") # only 2 groups, palette has min. 3
  expect_snapshot(
    ggnet2(n, color = factor(1:10), palette = "Set1"),
    error = TRUE
  )
  expect_snapshot(
    ggnet2(n, color = "phono", color.palette = c("vowel" = 1)),
    error = TRUE
  )

  # test shape.palette
  ggnet2(n, shape = "phono", shape.palette = c("vowel" = 15, "consonant" = 19))
  expect_warning(ggnet2(n, shape = factor(1:10)), "discrete values")
  expect_snapshot(
    ggnet2(n, shape = "phono", shape.palette = c("vowel" = 1)),
    error = TRUE
  )

  # test size.palette
  ggnet2(n, size = "phono", size.palette = c("vowel" = 1, "consonant" = 2))
  ggnet2(n, size = factor(1:10))
  expect_snapshot(
    ggnet2(n, size = "phono", size.palette = c("vowel" = 1)),
    error = TRUE
  )

  # test node.label
  ggnet2(n, label = sample(letters, 10))
  ggnet2(n, label = "phono")

  # test label.alpha
  expect_snapshot(ggnet2(n, label = TRUE, label.alpha = "xyz"), error = TRUE)

  # test label.color
  expect_snapshot(ggnet2(n, label = TRUE, label.color = "xyz"), error = TRUE)

  # test label.size
  expect_snapshot(ggnet2(n, label = TRUE, label.size = "xyz"), error = TRUE)

  # test label.trim
  expect_snapshot(ggnet2(n, label = TRUE, label.trim = "xyz"), error = TRUE)
  ggnet2(n, label = TRUE, label.trim = toupper)

  # test mode
  expect_snapshot(ggnet2(n, mode = "xyz"), error = TRUE)
  expect_snapshot(ggnet2(n, mode = letters[1:3]), error = TRUE)

  # test edge.node shared colors
  ggnet2(n, color = "phono", edge.color = c("color", "grey"))

  # test edge.color
  expect_snapshot(ggnet2(n, edge.color = "xyz"), error = TRUE)

  # test edge.label.alpha
  expect_snapshot(
    ggnet2(n, edge.label = "xyz", edge.label.alpha = "xyz"),
    error = TRUE
  )

  # test edge.label.color
  expect_snapshot(
    ggnet2(n, edge.label = "xyz", edge.label.color = "xyz"),
    error = TRUE
  )

  # test edge.label.size
  expect_snapshot(
    ggnet2(n, edge.label = "xyz", edge.label.size = "xyz"),
    error = TRUE
  )

  # test edge.size
  expect_snapshot(ggnet2(n, edge.size = "xyz"), error = TRUE)

  # test layout.exp
  expect_snapshot(ggnet2(n, layout.exp = "xyz"), error = TRUE)
  ggnet2(n, layout.exp = 0.1)

  ### --- test bipartite functionality

  # weighted adjacency matrix
  bip <- data.frame(
    event1 = c(1, 2, 1),
    event2 = c(0, 0, 3),
    event3 = c(1, 1, 0),
    row.names = letters[1:3]
  )

  # weighted bipartite network
  bip <- network(
    bip,
    matrix.type = "bipartite",
    ignore.eval = FALSE,
    # names.eval = "weights"
  )

  # test bipartite mode
  ggnet2(bip, color = "mode")

  ### --- test network coercion

  expect_warning(
    ggnet2(network(matrix(1, nrow = 2, ncol = 2), loops = TRUE)),
    "self-loops"
  )

  expect_snapshot(ggnet2(1:2), error = TRUE)
  expect_snapshot(
    ggnet2(network(data.frame(1:2, 3:4), hyper = TRUE)),
    error = TRUE
  )
  expect_snapshot(
    ggnet2(network(data.frame(1:2, 3:4), multiple = TRUE)),
    error = TRUE
  )

  ### --- test igraph functionality
  if (rq(igraph) && rq(intergraph)) {
    # test igraph conversion
    p <- ggnet2(asIgraph(n), color = "group")
    expect_null(p$guides$colour)

    # test igraph degree
    ggnet2(n, size = "degree")

    expect_true(TRUE)
  }
})
