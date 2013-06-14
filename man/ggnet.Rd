\name{ggnet}
\alias{ggnet}
\title{ggnet - Plot a network with ggplot2}
\usage{
  ggnet(data, mode = "fruchtermanreingold", size = 12, alpha = .75,
  weight.method = "none", names = c("", ""), 
  node.group = NULL, node.color = NULL, node.alpha = NULL,
  segment.alpha = NULL, segment.color = "grey", segment.size  = .25, arrow.size = 0,
  label.nodes = FALSE, quantize.weights = FALSE,
  legend.position = "right", ...)
}
\arguments{
  \item{net}{an object of class \code{igraph} or \code{network}. 
  	If the object is of class \code{igraph}, the \link{intergraph} package is used 
  	to convert it to class \code{network}.}
  \item{mode}{a placement method from the list of modes provided in the \link{sna} package. 
  	Defaults to the Fruchterman-Reingold force-directed algorithm.}
  \item{size}{size of the network nodes. Defaults to 12. 
  	If the nodes are weighted, their area is proportionally scaled up to the size set by \code{size}.}
  \item{alpha}{a level of transparency for nodes, vertices and arrows.
  	Defaults to 0.75.}
  \item{weight.method}{a weighting method for the nodes.
  	Accepts \code{"indegree"}, \code{"outdegree"} or \code{"degree"} (the default). 
  	Set to \code{"none"} to plot unweighted nodes.}
  \item{names}{a character vector of two elements to use as legend titles for the node groups and node weights. 
  	Defaults to empty strings.}
  \item{node.group}{a vector of character strings to label the nodes with, 
  	of the same length and order as the vertex names. 
  	Factors are converted to strings prior to plotting.}
  \item{node.color}{a vector of character strings to color the nodes with, 
  	holding as many colors as there are levels in \code{node.group}. 
  	Tries to default to \code{"Set1"} if missing.}
  \item{node.alpha}{transparency of the nodes. 
  	Inherits from \code{alpha}.}
  \item{segment.alpha}{transparency of the vertex links. 
  	Inherits from \code{alpha}.}
  \item{segment.color}{color of the vertex links. 
  	Defaults to \code{"grey".}}
  \item{segment.size}{size of the vertex links. 
  	Defaults to 0.25.}
  \item{arrow.size}{size of the vertex arrows for directed network plotting. 
  	Defaults to 0.}
  \item{label.nodes}{label nodes with their vertex attributes. 
  	If set to \code{TRUE}, all nodes are labelled. 
  	Also accepts a vector of character strings to match with vertex names.}
  \item{quantize.weights}{Break node weights to quartiles. 
  	Might fail if quartiles do not uniquely identify nodes.}
  \item{legend.position}{location of the captions for node colors and weights. 
  	Accepts all positions supported by ggplot2 themes. 
  	Defaults to "right".}
  \item{...}{other arguments supplied to geom_text for the node labels.
  	Arguments pertaining to the title or other items can be achieved through ggplot2 methods.}
}
\description{
  Function for making a network plot from an object of class \code{network} or \code{igraph}, using ggplot2.
}
\examples{
# Random network.
rnd = network(10)
ggnet(rnd, 
  label = TRUE,
  alpha = 1,
  color = "white",
  segment.color = "grey10")
# adding groups
cat = LETTERS[rbinom(10, 4, .5)]
ggnet(rnd,
  label = TRUE,
  node.group = cat,
  color = "white",
  segment.color = "grey10")
}
\author{
  Francois Briatte \email{f.briatte@ed.ac.uk} and
  Moritz Marbach \email{mmarbach@mail.uni-mannheim.de}  
}
\seealso{
  \code{\link[sna]{gplot}}
}

