
context("ggnet")

require(network)

# city and service firms data from the UCIrvine Network Data Repository
data(cityServiceFirms, package = "GGally")

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


  # plot cities, firms and law firms
  type = cityServiceFirms %v% "type"
  type = ifelse(grepl("City|Law", type), gsub("I+", "", type), "Firm")
  expect_warning(p <- ggnet(
    cityServiceFirms,
    mode = "kamadakawai",
    alpha = .2,
    node.group = type,
    label.nodes = c("Paris", "Beijing", "Chicago"),
    color = "darkred"
  ), "Node groups and node colors are ")
  expect_equal(length(p$layers), 3)
  expect_true(!is.null(p$mapping$colour))

  expect_true(TRUE)
})
