#######################################
# DataWorksMD Social Network Analysis #
# Ian McCulloh, Ph.D.  15 May 2018    #
#######################################

# Please feel free to use this code and share with friends

# If you have not installed igraph, use the following command
install.packages("igraph")

# Each time you start a new evnironment, you need to load the package
library(igraph)

# For R specific applications, I recommend the following packages
library(data.table)  #this is a package for more efficient data management
library(cluster)     #this is a package for subgroup analysis

# For loading data
# dt<-read.table("mydata.csv", header=TRUE, sep = ",", quote = "\"")

# For generating networks
g1 <- graph( edges=c(1,2, 2,3, 3, 1), n=3, directed=F )
plot(g1)
class(g1)
g1

# For networks with more nodes
g2 <- graph( edges=c(1,2, 2,3, 3, 1), n=7 )
plot(g2)

# For naming nodes
g3 <- graph( edges = c("Alex", "Bill", "Bill", "Carl", "Carl", 
                       "Alex"))
plot(g3)

g4 <- graph( edges = c("Alex", "Bill", "Bill", "Carl", "Carl", 
                       "Alex"), isolates = c("Dave", "Eric", "Frank", "Greg"))
plot(g4)
plot(g4, edge.arrow.size=.5, vertex.color="red", vertex.size=10,
     vertex.frame.color="gray", vertex.label.color="black",
     vertex.label.cex=1.2, vertex.label.dist=2, edge.curved=0.2)

g5 <- graph_from_literal(a-b-c-d-e, a-h-g-i-b, a-g, h-i, d-j:k:l, e-l:k)
plot(g5)

# To inspect the graph
E(g5)    #edges
V(g5)    #vertices
g5[]     #adjacency matrix
V(g5)$name

V(g5)$gender <- c("m","f","m","f","m","m","f","f","m","m","m")
plot(g5, edge.arrow.size=.5, vertex.label.color="black", vertex.label.dist=1.5,
     vertex.color=c( "pink", "blue")[1+(V(g5)$gender=="m")] )

#Density
edge_density(g5, loops = FALSE)

#Diameter
diameter(g5, directed = FALSE, weights=NA)

d<-get_diameter(g5, directed = FALSE, weights=NA)
d

#Color nodes along the diameter
vcol <- rep("gray40", vcount(g5))
vcol[d] <- "red"
ecol <- rep("gray80", ecount(g5))
ecol[E(g5, path=d)] <- "orange"
plot(g5, vertex.color=vcol, edge.color=ecol, edge.arrow.mode=0)

#Centrality
deg <- degree(g5, mode="all")  #Degree Centrality
plot(g5, vertex.size=deg*5)
hist(deg, breaks=1:vcount(g5)-1, main="Histogram of Degree")

clo1<-closeness(g5, mode="all", weights=NA)   #closeness based on distance
clo2<-centr_clo(g5, mode="all", normalized=T) #inverse geodesic distance
plot(g5, vertex.size=clo1*300)
plot(g5, vertex.size=clo2$res*30)
clo2

e<-eigen_centrality(g5, directed=T, weights=NA) #Eigenvector Centrality
e$vector

b<-betweenness(g5, directed=FALSE, weights=NA)  #Betweenness Centrality
plot(g5, vertex.size=b)
b

#Clustering

#Cliques
cliq<-cliques(g5)
cliq
lg.cliq<-largest_cliques(g5)
lg.cliq
vcol <- rep("grey80", vcount(g5))
vcol[unlist(largest_cliques(g5))] <- "gold"
plot(as.undirected(g5), vertex.label=V(g5)$name, vertex.color=vcol)

#Newman Grouping
g5.new<-cluster_edge_betweenness(g5)
plot(g5.new, g5)
length(g5.new)
membership(g5.new)
modularity(g5.new)

#Louvain Grouping
cl<-cluster_louvain(g5, weights = NULL)
plot(cl, g5)
length(cl)
membership(cl)
modularity(cl)
