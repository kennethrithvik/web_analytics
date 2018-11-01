#Load the package SNA (Social Network Analysis Library)
#Load the package igraph (Network analysis and visualization)
library(sna)
library(igraph)
library(readr)
disease_sim <- read_csv("data/disease_sim.csv")
disease_symtoms <- read_csv("data/disease_data.csv")


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
colnames(disease_sim)<- c("from","to","weights")
g <- graph.data.frame(disease_sim, directed=FALSE)
plotG(g)
plot(g, edge.width=E(g)$weights)


#install.packages("disparityfilter")
library(disparityfilter)
G_backbone = get.backbone(g, weights = igraph::E(g)$weights,
         directed = igraph::is_directed(g), alpha = 0.05)





