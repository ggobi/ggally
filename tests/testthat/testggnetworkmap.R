library(ggplot2)
context("Building Example Data")
airports <- read.csv("http://datasets.flowingdata.com/tuts/maparcs/airports.csv", header=TRUE)
airports$lon <- airports$long

flights <- data.frame(origin = sample(airports[200:400,]$iata, 200, replace = TRUE),
											destination = sample(airports[200:400,]$iata, 200, replace = TRUE))
library(igraph)
graph <- igraph::graph.data.frame(flights, airports, directed = TRUE)
graph <- graph - V(graph)[igraph::degree(graph, mode = "total") < 2]
igraph::V(graph)$degree <- igraph::degree(graph, mode = "total")
igraph::V(graph)$mygroup <- sample(1:4, length(V(graph)), replace = TRUE)
library(ggmap)
us <- ggmap(ggmap = get_map(location = "United States", 5, color = "bw"))

context("ggnetworkmap")

test_that("basic drawing, no map", {
	expect_error(ggnetworkmap(data = graph, size = 2))
	})
test_that("basic drawing, map", {
	expect_error(ggnetworkmap(us, data = graph, size = 2))
})
test_that("great circles", {
	expect_error(ggnetworkmap(us, data = graph, size = 2,
														great.circles = TRUE))
})
test_that("node groups", {
	expect_error(ggnetworkmap(us, data = graph, size = 2,
														great.circles = TRUE, node.group = degree))
})
test_that("ring groups", {
	expect_error(ggnetworkmap(us, data = graph, size = 2,
														great.circles = TRUE, node.group = degree,
														ring.group = mygroup))
})
test_that("segment color", {
	expect_error(ggnetworkmap(us, data = graph, size = 2,
														great.circles = TRUE, node.group = degree,
														ring.group = mygroup,
														segment.color = "cornflowerblue"))
})
test_that("weight", {
	expect_error(ggnetworkmap(us, data = graph, size = 2,
														great.circles = TRUE, node.group = degree,
														ring.group = mygroup,
														segment.color = "cornflowerblue",
														weight = degree))
})
test_that("labels", {
	expect_error(ggnetworkmap(us, data = graph, size = 2,
														great.circles = TRUE, node.group = degree,
														ring.group = mygroup,
														segment.color = "cornflowerblue",
														weight = degree,
														label.nodes = TRUE))
})
test_that("arrows", {
	expect_error(ggnetworkmap(us, data = graph, size = 2,
														great.circles = TRUE, node.group = degree,
														ring.group = mygroup,
														segment.color = "cornflowerblue",
														weight = degree,
														label.nodes = TRUE,
														arrow.size = 0.2))
})
