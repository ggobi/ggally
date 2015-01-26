
context("Building Example Data")

require(igraph)
require(maps)
require(ggplot2)

airports <- read.csv("http://datasets.flowingdata.com/tuts/maparcs/airports.csv", header=TRUE)
airports$lon <- airports$long

set.seed(124)
flights <- data.frame(origin = sample(airports[200:400,]$iata, 200, replace = TRUE),
                      destination = sample(airports[600:800,]$iata, 200, replace = TRUE))


graph <- igraph::graph.data.frame(flights, airports, directed = TRUE)
graph <- graph - V(graph)[igraph::degree(graph, mode = "total") < 2]
igraph::V(graph)$degree <- igraph::degree(graph, mode = "total")
igraph::V(graph)$mygroup <- sample(1:4, length(V(graph)), replace = TRUE)


us <- ggplot(ggplot2::map_data("usa"), aes(x = long, y = lat)) +
  geom_polygon(aes(group = group), color = "grey65",
               fill = "#f9f9f9", size = 0.2)

context("ggnetworkmap")

test_that("basic drawing", {
  # no map
  p <- ggnetworkmap(data = graph, size = 2)
  expect_true(is.null(nrow(p$data)))

  # map
  p <- ggnetworkmap(us, data = graph, size = 2)
  expect_false(is.null(nrow(p$data)))
})

test_that("great circles", {
  p <- ggnetworkmap(
    us, data = graph, size = 2,
    great.circles = TRUE
  )
  expect_equivalent(length(p$layers), 3)
  expect_equivalent(get("geom_params", envir = p$layers[[3]])$colour, "black")
})

test_that("node groups", {
  p <- ggnetworkmap(
    us, data = graph, size = 2,
    great.circles = TRUE,
    node.group = degree
  )
  expect_equivalent(length(p$layers), 3)
  expect_true(is.null(get("geom_params", envir = p$layers[[3]])$colour))
  expect_equivalent(as.character(get("mapping", envir = p$layers[[3]])$colour), ".ngroup")

  p <- ggnetworkmap(
    us, data = graph, size = 2,
    great.circles = TRUE,
    node.color = "red"
  )
  expect_equivalent(as.character(get("geom_params", envir = p$layers[[3]])$colour), "red")


})


test_that("ring groups", {
  p <- ggnetworkmap(
    us, data = graph, size = 2,
    great.circles = TRUE, node.group = degree,
    ring.group = mygroup
  )
  expect_equivalent(length(p$layers), 3)
  expect_true(is.null(get("geom_params", envir = p$layers[[3]])$colour))
  expect_equivalent(as.character(get("mapping", envir = p$layers[[3]])$colour), ".rgroup")
  expect_equivalent(as.character(get("mapping", envir = p$layers[[3]])$fill), ".ngroup")

})

test_that("segment color", {
  p <- ggnetworkmap(
    us, data = graph, size = 2,
    great.circles = TRUE, node.group = degree,
    ring.group = mygroup,
    segment.color = "cornflowerblue"
  )
  expect_equivalent(length(p$layers), 3)
  expect_true(is.null(get("geom_params", envir = p$layers[[3]])$colour))
  expect_equivalent(as.character(get("mapping", envir = p$layers[[3]])$colour), ".rgroup")
  expect_equivalent(as.character(get("mapping", envir = p$layers[[3]])$fill), ".ngroup")
  expect_equivalent(as.character(get("geom_params", envir = p$layers[[2]])$colour), "cornflowerblue")

})

test_that("weight", {
  p <- ggnetworkmap(
    us, data = graph, size = 2,
    great.circles = TRUE, node.group = degree,
    ring.group = mygroup,
    segment.color = "cornflowerblue",
    weight = degree
  )

  expect_equivalent(length(p$layers), 3)
  expect_true(is.null(get("geom_params", envir = p$layers[[3]])$colour))
  expect_equivalent(as.character(get("mapping", envir = p$layers[[3]])$colour), ".rgroup")
  expect_equivalent(as.character(get("mapping", envir = p$layers[[3]])$fill), ".ngroup")
  expect_equivalent(as.character(get("geom_params", envir = p$layers[[2]])$colour), "cornflowerblue")
  expect_equivalent(as.character(get("mapping", envir = p$layers[[3]])$size), ".weight")


})


test_that("labels", {
  p <- ggnetworkmap(
    us, data = graph, size = 2,
    great.circles = TRUE, node.group = degree,
    ring.group = mygroup,
    segment.color = "cornflowerblue",
    weight = degree,
    label.nodes = TRUE
  )

  expect_equivalent(length(p$layers), 4)
  expect_true(is.null(get("geom_params", envir = p$layers[[3]])$colour))
  expect_equivalent(as.character(get("mapping", envir = p$layers[[3]])$colour), ".rgroup")
  expect_equivalent(as.character(get("mapping", envir = p$layers[[3]])$fill), ".ngroup")
  expect_equivalent(as.character(get("geom_params", envir = p$layers[[2]])$colour), "cornflowerblue")
  expect_equivalent(as.character(get("mapping", envir = p$layers[[3]])$size), ".weight")
  expect_equivalent(as.character(get("mapping", envir = p$layers[[4]])$label), ".label")

  expect_true(is.null(get("geom_params", envir = p$layers[[2]])$arrow))
})

test_that("arrows", {
  p <- ggnetworkmap(
    us, data = graph, size = 2,
    great.circles = TRUE, node.group = degree,
    ring.group = mygroup,
    segment.color = "cornflowerblue",
    weight = degree,
    label.nodes = TRUE,
    arrow.size = 0.2
  )

  expect_equivalent(length(p$layers), 4)
  expect_true(is.null(get("geom_params", envir = p$layers[[3]])$colour))
  expect_equivalent(as.character(get("mapping", envir = p$layers[[3]])$colour), ".rgroup")
  expect_equivalent(as.character(get("mapping", envir = p$layers[[3]])$fill), ".ngroup")
  expect_equivalent(as.character(get("geom_params", envir = p$layers[[2]])$colour), "cornflowerblue")
  expect_equivalent(as.character(get("mapping", envir = p$layers[[3]])$size), ".weight")
  expect_equivalent(as.character(get("mapping", envir = p$layers[[4]])$label), ".label")

  expect_true(is.list(get("geom_params", envir = p$layers[[2]])$arrow))

})


test_that("labels", {
  expect_error(ggnetworkmap(us, data = graph, label.nodes = c("A", "B")))

  testLabels <- paste("L", 1:115, sep = "")

  # does logical check
  p <- ggnetworkmap(us, data = graph, label.nodes = testLabels)
  expect_equal(get("data", p$layers[[4]])$.label, testLabels)

  # does vertex.names check
  p <- ggnetworkmap(us, data = graph, label.nodes = TRUE)
  expect_true(!is.null(get("data", p$layers[[4]])$.label))

  # does id check
  graph2 <- graph
  igraph::V(graph2)$id <- testLabels
  p <- ggnetworkmap(us, data = graph2, label.nodes = TRUE)
  expect_true(!is.null(get("data", p$layers[[4]])$.label))
})


