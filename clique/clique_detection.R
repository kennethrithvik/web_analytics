#Load the package SNA (Social Network Analysis Library)
#Load the package igraph (Network analysis and visualization)
library(igraph)
library(sna)

# define function to plot a graph
plotG <- function(g) {
  plot(g, 
       # force-directed layout
       layout=layout.fruchterman.reingold,
       #vertex.label=NA,
       vertex.label.font=2, vertex.size=5, 
       vertex.color="blue",
       vertex.frame.color=FALSE, 
       edge.color="black")
}

# read in the data from a similar CSV file as follows
# link between singapore and malay
links <- read.csv("/Users/songyuhan/Documents/clique/Graph2.csv",header=T, as.is=T) 
head(links)
#g <- graph.data.frame(SampleGraph, directed=TRUE)
nrow(links)        #12164 number of links
nrow(unique(links[,c("From", "To")]))     #5199 unique links

#links <- aggregate(links[,3], links[,-3], sum)
links <- links[order(links$From, links$To),]

# Plot graph : the links and nodes 
net <- graph_from_data_frame(d=links, directed=T) 
net
E(net)       # The edges of the "net" object : 5199
V(net)       # The vertices of the "net" object : 488
plot(net, edge.arrow.size=.4)

#simplify the net by removing the loops in the graph
net1 <- simplify(net, remove.multiple = F, remove.loops = T) 
plot(net1,edge.arrow.size=.4)
E(net1) 
V(net1)   
# Plot with curved edges (edge.curved=.1) and reduce arrow size:
plot(net1, edge.arrow.size=.4, edge.curved=.1)

# Set edge color to gray, and the node color to orange. 

# Replace the vertex label with the node names 

plot(net1, edge.arrow.size=.2, edge.curved=0,
     
     vertex.color="orange", vertex.frame.color="#555555",
     
     vertex.label=V(net), vertex.label.color="black",
     
     vertex.label.cex=.7) 

#network density : 0.0218 The proportion of present edges from all possible edges in the network
edge_density(net1, loops=F)

#transitivity
transitivity(net1, type="global")  # net1 is treated as an undirected network
#transitivity(net1, type="local")
triad_census(net1) # for directed networks 

#diameter : longest geodesic distance is 6 
diameter(net1, directed=F, weights=NA)
#returns a vertex sequence : 8 naming as : LBP MUR MYY BKI CGK AMQ FKQ KNG
diam <- get_diameter(net1, directed=T)
diam
#color the nodes along the diameter 
vcol <- rep("gray40", vcount(net1))
vcol[diam] <- "gold"
ecol <- rep("gray80", ecount(net1))
ecol[E(net1, path=diam)] <- "orange" 
# E(net, path=diam) finds edges along a path, here 'diam'
plot(net1, vertex.color=vcol, edge.color=ecol, edge.arrow.mode=0)

#node degree 
deg <- degree(net1,v = V(net1), mode = "in")
plot(net1, vertex.size=deg*3)

#subgraph and community  : need to make network undirected first 
net1.sym <- as.undirected(net1, mode= "collapse")

#largest clique number  : 18 , within each the nodes number is 15
clique.number(net1.sym)
cliques(net1.sym, min =4) # list of cliques       
sapply(cliques(net1.sym), length) # clique sizes
largest_cliques(net1.sym) # cliques with max number of nodes

vcol <- rep("grey40", vcount(net1.sym))
vcol[unlist(largest_cliques(net1.sym))] <- "gold"
plot(as.undirected(net1.sym), vertex.label=V(net1.sym), vertex.color=vcol)

#Community detection based on edge betweenness (Newman-Girvan)
#High-betweenness edges are removed sequentially (recalculating at each step) and the best partitioning of the network is selected
ceb <- cluster_edge_betweenness(net1.sym) 
dendPlot(ceb, mode="hclust")
plot(ceb, net) 

length(ceb)     # number of communities :101
len= membership(ceb) # community membership for each node

modularity(ceb) # how modular the graph partitioning is


##########################################
# Clique detection
##########################################
# WE EXPLAIN TWO EXAMPLES

# The functions find cliques, ie. complete subgraphs in a graph
# clique.number calculates the size of the largest clique(s).
clique.number(net1.sym)    #we want to find how many clique are in the graph,this is equal to 6

#cliques find all complete subgraphs in the input graph, 
#obeying the size limitations given in the min and max arguments.
cliques(net1.sym, min=6)

# How about we want to find smaller cliques
cliques(net1.sym, min=4)    #much larger clique number than 6

#maximal.cliques finds all maximal cliques in the input graph.
#A clique in maximal if it cannot be extended to a larger clique. 
#The largest cliques (maximum cliques) are always maximal, but a maximal clique is not neccessarily the largest.
maximal.cliques(net1.sym)

# Maximum cliques: largest.cliques finds all largest cliques in the input 
# graph. A clique is largest if there is no other clique including more vertices.
largest.cliques(net1.sym)


