
context("ggnet")

require(intergraph)
require(network)
require(sna)

# make toy random network
x                  <- 10
ndyads             <- x * (x - 1)
density            <- x / ndyads
nw.mat             <- matrix(0, nrow = x, ncol = x)
dimnames(nw.mat)   <- list(1:x, 1:x)
nw.mat[row(nw.mat) != col(nw.mat)] <- runif(ndyads) < density

rnd <- network::network(nw.mat)


test_that("examples", {

  # random network
  p <- ggnet(
    rnd,
    label.nodes = TRUE,
    alpha = 1,
    color = "white",
    segment.color = "grey10"
  )
  expect_equal(length(p$layers), 3)
  expect_true(is.null(p$mapping$colour))


  # random groups
  category = LETTERS[rbinom(x, 4, .5)]
  expect_warning(p <- ggnet(
    rnd,
    label.nodes = TRUE,
    color = "white",
    segment.color = "grey10",
    node.group = category
  ), "Node groups and node colors are ")
  expect_equal(length(p$layers), 3)
  expect_true(!is.null(p$mapping$colour))


  # # plot cities, firms and law firms
  # type = cityServiceFirms %v% "type"
  # type = ifelse(grepl("City|Law", type), gsub("I+", "", type), "Firm")
  # expect_warning(p <- ggnet(
  #   cityServiceFirms,
  #   mode = "kamadakawai",
  #   alpha = .2,
  #   node.group = type,
  #   label.nodes = c("Paris", "Beijing", "Chicago"),
  #   color = "darkred"
  # ), "Node groups and node colors are ")
  # expect_equal(length(p$layers), 3)
  # expect_true(!is.null(p$mapping$colour))


  # test igraph conversion
  rnd_igraph = asIgraph(rnd)
  expect_warning(p_igraph <- ggnet(
    rnd_igraph,
    node.group = category
  ), "Node groups and node colors are ")
  expect_equal(length(p_igraph$layers), 2)
  expect_true(!is.null(p_igraph$mapping$colour))


  # test subset.threshold by removing all nodes
  expect_warning(
    expect_error(
      ggnet(rnd, subset.threshold = 11),
      "NA/NaN/Inf"
    ),
    "NaNs produced"
  )


  # test mode = "geo"
  xy = gplot.layout.circle(rnd)
  rnd %v% "lon" = xy[, 1]
  rnd %v% "lat" = xy[, 2]
  p <- ggnet(rnd, mode = "geo")
  expect_equal(p$data$X1, xy[, 1])
  expect_equal(p$data$X2, xy[, 2])


  # test user-submitted weights
  expect_warning(
    p <- ggnet(rnd, weight = sample(1:2, 10, replace = TRUE)),
    "the condition has length > 1"
  )
  expect_equal(length(p$layers), 2)
  expect_true(p$layers[[2]]$mapping$size == "weight")

  # test vertex attribute weights
  rnd %v% "weights" = sample(1:2, 10, replace = TRUE)
  expect_output(
    p <- ggnet(rnd, weight = "weights"),
    "weighted by"
  )
  expect_equal(length(p$layers), 2)
  expect_true(p$layers[[2]]$mapping$size == "weight")


  # test top8.nodes
  p <- ggnet(rnd, top8.nodes = TRUE)
  expect_equal(length(p$layers), 2)
  expect_true(!is.null(p$mapping$colour))


  # test segment.label
  x = sample(letters, network.edgecount(rnd))
  p <- ggnet(rnd, segment.label = x)
  expect_equal(length(p$layers), 3)
  expect_true(p$layers[[2]]$mapping$x == "midX")
  expect_true(p$layers[[2]]$mapping$y == "midY")


  # test quantize.weights
  rnd %v% "weights" = 1:10
  expect_output(
    p <- ggnet(rnd, weight.method = "weights", quantize.weights = TRUE),
    "weighted by"
  )
  expect_equal(length(p$layers), 2)
  expect_true(p$layers[[2]]$mapping$size == "weight")


  expect_true(TRUE)
})
