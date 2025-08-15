if ("package:igraph" %in% search()) {
  detach("package:igraph")
}

skip_if_not(rq(network)) # network objects
skip_if_not(rq(sna)) # placement and centrality

skip_if_not(rq(ggplot2)) # grammar of graphics
skip_if_not(rq(grid)) # arrows
skip_if_not(rq(scales)) # sizing

skip_if_not(rq(intergraph)) # test igraph conversion

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

  lifecycle::expect_deprecated(
    ggnet(n, label = TRUE, alpha = 1, color = "white", segment.color = "black")
  )

  # random groups
  g <- sample(letters[1:3], 10, replace = TRUE)

  # color palette
  p <- c("a" = "steelblue", "b" = "forestgreen", "c" = "tomato")

  lifecycle::expect_deprecated({
    p <- ggnet(n, node.group = g, node.color = p, label = TRUE, color = "white")
  })
  expect_equal(length(p$layers), 3)
  expect_true(!is.null(p$mapping$colour))

  ### --- end: documented examples

  ### --- test deprecations

  # test mode = "geo"
  xy <- gplot.layout.circle(n)
  n %v% "lon" <- xy[, 1]
  n %v% "lat" <- xy[, 2]
  lifecycle::expect_deprecated({
    # mode = "geo"
    lifecycle::expect_deprecated({
      # ggnet
      ggnet(n, mode = "geo")
    })
  })

  lifecycle::expect_deprecated({
    # names = c(x, y)
    lifecycle::expect_deprecated({
      # ggnet
      ggnet(n, names = c("a", "b"))
    })
  })

  # test quantize.weights
  with_options(list(warn = 2), {
    expect_error(ggnet(n, quantize.weights = TRUE))
  })

  lifecycle::expect_deprecated({
    # subset.threshold
    lifecycle::expect_deprecated({
      # ggnet
      suppressMessages({
        ggnet(n, subset.threshold = 2)
      })
    })
  })

  lifecycle::expect_deprecated({
    # top8.nodes
    lifecycle::expect_deprecated({
      # ggnet
      suppressMessages({
        ggnet(n, top8.nodes = TRUE)
      })
    })
  })

  lifecycle::expect_deprecated({
    # trim.labels
    lifecycle::expect_deprecated({
      # ggnet
      suppressMessages({
        ggnet(n, trim.labels = TRUE)
      })
    })
  })

  #   # test subset.threshold by removing all nodes
  #   expect_warning(
  #     expect_error(
  #       ggnet(n, subset.threshold = 11),
  #       "NA/NaN/Inf"
  #     ),
  #     "NaNs produced"
  #   )
  #
  #   p <- ggnet(n, mode = "geo")
  #   expect_equal(p$data$X1, xy[, 1])
  #   expect_equal(p$data$X2, xy[, 2])

  # Be quiet about lifecycle messages from here on
  old_opts <- options(lifecycle_verbosity = "quiet")
  on.exit(options(old_opts), add = TRUE)

  # test user-submitted weights
  ggnet(n, weight = sample(1:2, 10, replace = TRUE))

  # test segment.label
  x <- sample(letters, network.edgecount(n))
  p <- ggnet(n, segment.label = x)
  expect_true(mapping_string(p$layers[[2]]$mapping$x) == "midX")
  expect_true(mapping_string(p$layers[[2]]$mapping$y) == "midY")

  # test weight.cut
  n %v% "weights" <- 1:10
  ggnet(n, weight.method = "weights", weight.cut = TRUE)

  ### --- test errors in set_node

  expect_error(ggnet(n, group = NA), "incorrect")
  expect_error(ggnet(n, group = 1:3), "incorrect")
  expect_error(ggnet(n, label = TRUE, label.size = -10:-1), "incorrect")
  expect_error(ggnet(n, size = "phono"), "incorrect")

  ggnet(n, group = "weights")

  ### --- test errors in set_edges

  expect_error(ggnet(n, segment.label = NA), "incorrect")
  expect_error(ggnet(n, segment.label = 1:3), "incorrect")
  expect_error(ggnet(n, segment.label = -11:-1), "incorrect") # unnecessary
  # expect_error(ggnet(n, size = "phono"), "incorrect")

  n %e% "weights" <- sample(1:2, network.edgecount(n), replace = TRUE)
  ggnet(n, segment.label = "weights")
  ggnet(n, segment.label = "a")

  ### --- test mode = c(x, y)

  ggnet(n, mode = matrix(1, ncol = 2, nrow = 10))
  ggnet(n, mode = c("lon", "lat"))
  expect_error(ggnet(n, mode = c("xx", "yy")), "not found")
  n %v% "abc" <- "abc"
  expect_error(ggnet(n, mode = c("abc", "abc")), "not numeric")
  expect_error(
    ggnet(n, mode = matrix(1, ncol = 2, nrow = 9)),
    "coordinates length"
  )

  ### --- test arrow.size

  expect_error(ggnet(n, arrow.size = -1), "incorrect `arrow.size`")
  expect_warning(ggnet(n, arrow.size = 1), "`arrow.size` ignored")

  ### --- test arrow.gap

  suppressWarnings(expect_error(
    ggnet(n, arrow.size = 12, arrow.gap = -1),
    "incorrect `arrow.gap`"
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
  ggnet(m, arrow.size = 12, arrow.gap = 0.05)

  ### --- test degree centrality

  ggnet(n, weight = "degree")

  ### --- test weight.min, weight.max and weight.cut

  # test weight.min
  suppressMessages({
    expect_error(
      ggnet(n, weight = "degree", weight.min = -1),
      "incorrect `weight.min`"
    )
    expect_message(
      ggnet(n, weight = "degree", weight.min = 1),
      "`weight.min` removed"
    )
    expect_warning(
      ggnet(n, weight = "degree", weight.min = 99),
      "removed all nodes"
    )
  })

  # test weight.max
  expect_error(
    ggnet(n, weight = "degree", weight.max = -1),
    "incorrect `weight.max`"
  )
  expect_message(
    ggnet(n, weight = "degree", weight.max = 99),
    "`weight.max` removed"
  )
  suppressMessages({
    expect_warning(
      ggnet(n, weight = 1:10, weight.max = 0.5),
      "removed all nodes"
    )
  })
  expect_error(ggnet(n, weight = "abc"), "incorrect `weight.method`")

  # test weight.cut
  expect_error(ggnet(n, weight.cut = NA), "incorrect `weight.cut`")
  expect_error(ggnet(n, weight.cut = "a"), "incorrect `weight.cut`")
  expect_warning(ggnet(n, weight.cut = 3), "`weight.cut` ignored")
  ggnet(n, weight = "degree", weight.cut = 3)

  ### --- test node.group and node.color

  expect_warning(ggnet(n, group = 1:10, node.color = "blue"), "unequal length")

  ### --- test node labels and label sizes

  ggnet(n, label = letters[1:10], color = "white")
  ggnet(n, label = "abc", color = "white", label.size = 4, size = 12)
  expect_error(
    ggnet(n, label = letters[1:10], label.size = "abc"),
    "incorrect `label.size`"
  )

  ### --- test node placement

  expect_error(ggnet(n, mode = "xyz"), "unsupported")
  expect_error(ggnet(n, mode = letters[1:3]), "incorrect `mode`")

  ### --- test label.trim
  expect_error(
    ggnet(n, label = TRUE, label.trim = "xyz"),
    "incorrect `label.trim`"
  )
  ggnet(n, label = TRUE, color = "white", label.trim = 1)
  ggnet(n, label = TRUE, color = "white", label.trim = toupper)

  ### --- test layout.exp
  expect_error(ggnet(n, layout.exp = "xyz"))
  ggnet(n, layout.exp = 0.1)

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
    ignore.eval = FALSE
    # names.eval = "weights"
  )

  # test bipartite mode
  ggnet(bip, group = "mode")

  ### --- test network coercion

  expect_warning(
    ggnet(network(matrix(1, nrow = 2, ncol = 2), loops = TRUE)),
    "self-loops"
  )

  expect_error(ggnet(1:2), "network object")
  expect_error(ggnet(network(data.frame(1:2, 3:4), hyper = TRUE)), "hyper")
  expect_error(
    ggnet(network(data.frame(1:2, 3:4), multiple = TRUE)),
    "multiplex graphs"
  )

  ### --- test igraph functionality
  if (rq(igraph) && rq(intergraph)) {
    # test igraph conversion
    p <- ggnet(asIgraph(n))
    expect_null(p$guides$colour)
    expect_equal(length(p$layers), 2)

    # test igraph degree
    ggnet(n, weight = "degree")
    expect_true(TRUE)
  }
})
