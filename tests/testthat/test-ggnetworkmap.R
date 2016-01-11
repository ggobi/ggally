
context("ggnetworkmap")

if ("package:igraph" %in% search()) {
  detach("package:igraph")
}

rq <- function(...) {
  require(..., quietly = TRUE)
}
rq(network)
rq(sna)
rq(maps)
rq(ggplot2)

rq(intergraph) # test igraph conversion

airports <- read.csv("http://datasets.flowingdata.com/tuts/maparcs/airports.csv", header = TRUE)
rownames(airports) <- airports$iata

# select some random flights
set.seed(1234)
flights <- data.frame(
  origin = sample(airports[200:400, ]$iata, 200, replace = TRUE),
  destination = sample(airports[200:400, ]$iata, 200, replace = TRUE)
)

# convert to network
flights <- network(flights, directed = TRUE)

# add geographic coordinates
flights %v% "lat" <- airports[ network.vertex.names(flights), "lat" ] # nolint
flights %v% "lon" <- airports[ network.vertex.names(flights), "long" ] # nolint

# drop isolated airports
delete.vertices(flights, which(degree(flights) < 2))

# compute degree centrality
flights %v% "degree" <- degree(flights, gmode = "digraph")

# add random groups
flights %v% "mygroup" <- sample(letters[1:4], network.size(flights), replace = TRUE)

# create a map of the USA
usa <- ggplot(map_data("usa"), aes(x = long, y = lat)) +
  geom_polygon(aes(group = group), color = "grey65",
               fill = "#f9f9f9", size = 0.2)

test_that("basic drawing", {
  # no map
  p <- ggnetworkmap(net = flights, size = 2)
  expect_true(is.null(nrow(p$data)))

  # overlay network data to map
  p <- ggnetworkmap(usa, flights, size = 2)
  expect_false(is.null(nrow(p$data)))
})

test_that("great circles", {
  p <- ggnetworkmap(usa, flights, size = 2, great.circles = TRUE)
  expect_equal(length(p$layers), 3)
  expect_equal(get("aes_params", envir = p$layers[[3]])$colour, "black")
})

test_that("node groups", {
  p <- ggnetworkmap(usa, flights, size = 2, great.circles = TRUE,
                    node.group = degree)
  expect_equal(length(p$layers), 3)
  expect_true(is.null(get("aes_params", envir = p$layers[[3]])$colour))
  expect_equal(as.character(get("mapping", envir = p$layers[[3]])$colour), ".ngroup")

  p <- ggnetworkmap(usa, flights, size = 2, great.circles = TRUE, node.color = "red")
  expect_equal(as.character(get("aes_params", envir = p$layers[[3]])$colour), "red")
})

test_that("ring groups", {
  p <- ggnetworkmap(usa, flights, size = 2, great.circles = TRUE,
                    node.group = degree, ring.group = mygroup)
  expect_equal(length(p$layers), 3)
  expect_true(is.null(get("aes_params", envir = p$layers[[3]])$colour))
  expect_equal(as.character(get("mapping", envir = p$layers[[3]])$colour), ".rgroup")
  expect_equal(as.character(get("mapping", envir = p$layers[[3]])$fill), ".ngroup")
})

test_that("segment color", {
  p <- ggnetworkmap(usa, flights, size = 2,
    great.circles = TRUE, node.group = degree,
    ring.group = mygroup,
    segment.color = "cornflowerblue"
  )
  expect_equal(length(p$layers), 3)
  expect_true(is.null(get("aes_params", envir = p$layers[[3]])$colour))
  expect_equal(as.character(get("mapping", envir = p$layers[[3]])$colour), ".rgroup")
  expect_equal(as.character(get("mapping", envir = p$layers[[3]])$fill), ".ngroup")
  expect_equal(as.character(get("aes_params", envir = p$layers[[2]])$colour), "cornflowerblue")

})

test_that("weight", {
  p <- ggnetworkmap(usa, flights, size = 2, great.circles = TRUE, node.group = degree,
    ring.group = mygroup,
    segment.color = "cornflowerblue",
    weight = degree
  )

  expect_equal(length(p$layers), 3)
  expect_true(is.null(get("aes_params", envir = p$layers[[3]])$colour))
  expect_equal(as.character(get("mapping", envir = p$layers[[3]])$colour), ".rgroup")
  expect_equal(as.character(get("mapping", envir = p$layers[[3]])$fill), ".ngroup")
  expect_equal(as.character(get("aes_params", envir = p$layers[[2]])$colour), "cornflowerblue")
  expect_equal(as.character(get("mapping", envir = p$layers[[3]])$size), ".weight")


})


test_that("labels", {
  p <- ggnetworkmap(usa, flights, size = 2, great.circles = TRUE,
                    node.group = degree, ring.group = mygroup,
                    segment.color = "cornflowerblue", weight = degree,
                    label.nodes = TRUE)

  expect_equal(length(p$layers), 4)
  expect_true(is.null(get("aes_params", envir = p$layers[[3]])$colour))
  expect_equal(as.character(get("mapping", envir = p$layers[[3]])$colour), ".rgroup")
  expect_equal(as.character(get("mapping", envir = p$layers[[3]])$fill), ".ngroup")
  expect_equal(as.character(get("aes_params", envir = p$layers[[2]])$colour), "cornflowerblue")
  expect_equal(as.character(get("mapping", envir = p$layers[[3]])$size), ".weight")
  expect_equal(as.character(get("mapping", envir = p$layers[[4]])$label), ".label")

  expect_true(is.null(get("aes_params", envir = p$layers[[2]])$arrow))
})

test_that("arrows", {
  p <- ggnetworkmap(usa, flights, size = 2, great.circles = TRUE,
                    node.group = degree, ring.group = mygroup,
                    segment.color = "cornflowerblue", weight = degree,
                    label.nodes = TRUE, arrow.size = 0.2)

  expect_equal(length(p$layers), 4)
  expect_true(is.null(get("aes_params", envir = p$layers[[3]])$colour))
  expect_equal(as.character(get("mapping", envir = p$layers[[3]])$colour), ".rgroup")
  expect_equal(as.character(get("mapping", envir = p$layers[[3]])$fill), ".ngroup")
  expect_equal(as.character(get("aes_params", envir = p$layers[[2]])$colour), "cornflowerblue")
  expect_equal(as.character(get("mapping", envir = p$layers[[3]])$size), ".weight")
  expect_equal(as.character(get("mapping", envir = p$layers[[4]])$label), ".label")

  # look at geom_params for arrow info
  expect_true(is.list(get("geom_params", envir = p$layers[[2]])$arrow))

})


test_that("labels", {
  expect_error(ggnetworkmap(usa, flights, label.nodes = c("A", "B")))
  testLabels <- paste("L", 1:network.size(flights), sep = "")

  # does logical check
  p <- ggnetworkmap(usa, flights, label.nodes = testLabels)
  ## PROBLEM HERE: why would vertex.names be equal to testLabels?
  ## expect_equal(get("data", p$layers[[4]])$.label, testLabels)

  # does vertex.names check
  p <- ggnetworkmap(usa, flights, label.nodes = TRUE)
  expect_true(!is.null(get("data", p$layers[[4]])$.label))

  # does id check
  flights2 <- flights
  flights2 %v% "id" <- testLabels
  p <- ggnetworkmap(usa, flights2, label.nodes = TRUE)
  expect_true(!is.null(get("data", p$layers[[4]])$.label))
})

### --- test arrow.size

test_that("arrow.size", {
  expect_error(ggnetworkmap(net = flights, arrow.size = -1), "incorrect arrow.size")
  expect_warning(ggnetworkmap(net = network(as.matrix(flights), directed = FALSE),
                              arrow.size = 1), "arrow.size ignored")
})

### --- test network coercion

test_that("network coercion", {
  expect_warning(
    ggnetworkmap(net = network(matrix(1, nrow = 2, ncol = 2), loops = TRUE)),
    "self-loops"
  )

  expect_error(ggnetworkmap(net = 1:2), "network object")
  expect_error(ggnetworkmap(net = network(data.frame(1:2, 3:4), hyper = TRUE)), "hyper graphs")
  expect_error(
    ggnetworkmap(net = network(data.frame(1:2, 3:4), multiple = TRUE)),
    "multiplex graphs"
  )
})

### --- test igraph functionality

test_that("igraph conversion", {
  n <- asIgraph(flights)
  p <- ggnetworkmap(net = n)
  expect_equal(length(p$layers), 2)
})

expect_true(TRUE)
