
context("Building Example Data")
airports <- read.csv("http://datasets.flowingdata.com/tuts/maparcs/airports.csv", header=TRUE)
airports$lon <- airports$long

flights <- data.frame(origin = sample(airports[200:400,]$iata, 200, replace = TRUE),
											destination = sample(airports[200:400,]$iata, 200, replace = TRUE))
require(igraph)
require(maps)
graph <- igraph::graph.data.frame(flights, airports, directed = TRUE)
graph <- graph - V(graph)[igraph::degree(graph, mode = "total") < 2]
igraph::V(graph)$degree <- igraph::degree(graph, mode = "total")
igraph::V(graph)$mygroup <- sample(1:4, length(V(graph)), replace = TRUE)

us <- ggplot(ggplot2::map_data("usa"), aes(x = long, y = lat)) +
	geom_polygon(aes(group = group), color = "grey65",
							 fill = "#f9f9f9", size = 0.2)
context("ggnetworkmap")

test_that("basic drawing, no map", {
	throws_error(ggnetworkmap(data = graph, size = 2))
	})
test_that("basic drawing, map", {
	throws_error(ggnetworkmap(us, data = graph, size = 2))
})
test_that("great circles", {
	throws_error(ggnetworkmap(us, data = graph, size = 2,
														great.circles = TRUE))
})
test_that("node groups", {
	throws_error(ggnetworkmap(us, data = graph, size = 2,
														great.circles = TRUE, node.group = degree))
})
test_that("ring groups", {
	throws_error(ggnetworkmap(us, data = graph, size = 2,
														great.circles = TRUE, node.group = degree,
														ring.group = mygroup))
})
test_that("segment color", {
	throws_error(ggnetworkmap(us, data = graph, size = 2,
														great.circles = TRUE, node.group = degree,
														ring.group = mygroup,
														segment.color = "cornflowerblue"))
})
test_that("weight", {
	throws_error(ggnetworkmap(us, data = graph, size = 2,
														great.circles = TRUE, node.group = degree,
														ring.group = mygroup,
														segment.color = "cornflowerblue",
														weight = degree))
})
test_that("labels", {
	throws_error(ggnetworkmap(us, data = graph, size = 2,
														great.circles = TRUE, node.group = degree,
														ring.group = mygroup,
														segment.color = "cornflowerblue",
														weight = degree,
														label.nodes = TRUE))
})
test_that("arrows", {
	throws_error(ggnetworkmap(us, data = graph, size = 2,
														great.circles = TRUE, node.group = degree,
														ring.group = mygroup,
														segment.color = "cornflowerblue",
														weight = degree,
														label.nodes = TRUE,
														arrow.size = 0.2))
})
