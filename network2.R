setwd(dir = "/home/nymphadora/Téléchargements/alexis/the-marvel-universe-social-network")
#data from https://www.kaggle.com/csanhueza/the-marvel-universe-social-network
#edges_df <- read.csv("edges.csv") ##Not used
#nodes_df <- read.csv("nodes.csv")
hero_df <- read.csv("hero-network.csv")
#head(edges_df)
#head(nodes_df)
library(igraph)
library(threejs)
library(sna)
library(network)
hero_n<-graph_from_data_frame(hero_df, directed = F)



##Explo
# Subset vertices and edges
V(hero_n)
E(hero_n)
# Count number of edges
gsize(hero_n)
# Count number of vertices
gorder(hero_n)
#We have multiple edge : weights should be adjusted to match nmbr of edge for x to y
E(hero_n)$weight <- 1 
hero_n <- simplify(hero_n, remove.loops=T) 
E(hero_n)$weight
is.weighted(hero_n)
is.simple(hero_n)

igraph::is.connected(hero_n) #do we have a connected graph?
composant<-decompose.graph(hero_n)
table(sapply(composant,vcount)) # one main graph
cut_vert<-articulation.points(hero_n)#do we have articulation point?
length(cut_vert)#not that many

summary(hero_n)
plot(hero_n, layout = layout.drl(hero_n), vertex.label=NA)





##general infos about this graph
#graph density
dens <- edge_density(hero_n)
dens

#Is average path length shorter or longer than expected?
# Get the average path length of the graph g
dist <- mean_distance(hero_n, directed = FALSE)
# Generate n random graphs
n<-20
gl <- vector('list', n)
for(i in 1:n){
  gl[[i]] <- erdos.renyi.game(n = gorder(hero_n), p.or.m = dens, type = "gnp")
}

# Calculate average path length of 1000 random graphs
gl.apl <- lapply(gl, mean_distance, directed = FALSE)
gl.aplu <- unlist(gl.apl)

# Plot the distribution of average path lengths
hist(gl.aplu)
abline(v = dist, col = "red", lty = 3, lwd=2)

# Calculate the proportion of graphs with an average path length lower than our observed
sum(gl.aplu < dist)/n
#Average path is really short 

#vertex strength histogram (degree*weight)
hist(graph.strength(hero_n),col="blue",xlab="VertexStrength",ylab="Frequency",main="", breaks = 1000, xlim=c(0, 200))
hist(graph.strength(hero_n),col="blue",xlab="VertexStrength",ylab="Frequency",main="", breaks = 1000)
#big difference, while most are low, some are pretty high








##who is more important (more central)
# Calculate the degree
all_deg <- degree(hero_n, mode = c("all"))
which.max(all_deg)
#top 5 most popular
top<-mean(all_deg)+11.65*sd(all_deg)
length(all_deg[all_deg>top])
all_deg[all_deg>top]

# Calculate betweenness of each vertex
betw <- betweenness(hero_n, directed = F)
which.max(betw)
#top 5 most popular
top<-mean(betw)+15*sd(betw)
length(betw[betw>top])
betw[betw>top]

# Identify key nodes using eigenvector centrality
g.ec <- eigen_centrality(hero_n)
which.max(g.ec$vector)
#top 5 most popular
top<-mean(g.ec$vector)+18*sd(g.ec$vector)
length(g.ec$vector[g.ec$vector>top])
g.ec$vector[g.ec$vector>top]

#Iron Man and Captain America among most popular
#who is around CAPTAIN AMERICA ?
g_america <- make_ego_graph(hero_n, diameter(hero_n), nodes = 'CAPTAIN AMERICA', mode = c("all"))[[1]]
plot(g_america, vertex.label=NA)

A<-get.adjacency(hero_n,sparse=FALSE)
g<-network::as.network.matrix(A)
#sna::gplot.target(g,degree(g),main="Degree",circ.lab=FALSE,circ.col="skyblue",usearrows=FALSE,edge.col="darkgray") #computer too slow






##Do we have structures like cliques?
#table(sapply(cliques(hero_n),length)) #computer too slow

##Are there communities? Graph partitioning
#using randomwalks
net_comm<-walktrap.community(hero_n, steps = 5)
sizes(net_comm)
net_c <- set_vertex_attr(hero_n, "community", value = membership(net_comm))
#where is the biggest community
which.max(sizes(net_comm))
#using fastgreedy
net_comm<-fastgreedy.community(hero_n)
sizes(net_comm)
net_c <- set_vertex_attr(hero_n, "community", value = membership(net_comm))
which.max(sizes(net_comm))

#plot communities in the graph
pal<-rainbow(47)
i<-membership(net_comm)
final<-set_vertex_attr(hero_n, "color", value = pal[i])
graphjs(final, vertex.size = 1)

#plot graph of communities
c_f<-as.factor(membership(net_comm))
hero_n_c<- contract.vertices(hero_n, membership(net_comm))
hero_n_c<-simplify(hero_n_c)
lab<-sort(unique(membership(net_comm)))
plot(hero_n_c, vertex.label=lab)
#communities 28, 17 and 33 are disconnected


#let's look at these disconnected communities
m<-28
sum(vertex_attr(net_c)$community==m)
subam <- subgraph.edges(graph=net_c, eids=which(vertex_attr(net_c)$community==m), delete.vertices = TRUE)
sum(vertex_attr(net_c)$community==m)==gsize(subam) #ok
plot(subam, vertex.label.color = "black",edge.color = 'black',layout=layout.fruchterman.reingold(subam))
m<-17
sum(vertex_attr(net_c)$community==m)
subam <- subgraph.edges(graph=net_c, eids=which(vertex_attr(net_c)$community==m), delete.vertices = TRUE)
sum(vertex_attr(net_c)$community==m)==gsize(subam) #ok
plot(subam, vertex.label.color = "black",edge.color = 'black',layout=layout.fruchterman.reingold(subam))
m<-33
sum(vertex_attr(net_c)$community==m)
subam <- subgraph.edges(graph=net_c, eids=which(vertex_attr(net_c)$community==m), delete.vertices = TRUE)
sum(vertex_attr(net_c)$community==m)==gsize(subam) #ok
plot(subam, vertex.label.color = "black",edge.color = 'black',layout=layout.fruchterman.reingold(subam))

#similar to the disconnected part of the graph?
gorder(composant[[2]])
V(composant[[4]])
#not really...

#is it a "normal" number of communities?
n<-10 #higher if faster computer
nv<-vcount(hero_n)
ne<-ecount(hero_n)
degs<-igraph::degree(hero_n)

num.comm.rg<-numeric(n)
#fixed sized simulation
for(i in (1:n)){
  g.rg<-erdos.renyi.game(nv,ne,type="gnm")
  c.rg<-fastgreedy.community(g.rg)
  num.comm.rg[i]<-length(c.rg)
}

#degree sequence simulation
num.comm.grg<-numeric(n)
for(i in (1:n)){
  g.grg<-degree.sequence.game(degs,method="vl")
  c.grg<-fastgreedy.community(g.grg)
  num.comm.grg[i]<-length(c.grg)
}

#results
rslts<-c(num.comm.rg,num.comm.grg)
indx<-c(rep(0,n),rep(1,n))
counts<-table(indx,rslts)/n
barplot(counts, beside=TRUE, col=c("blue","red"), xlab="Number of Communities",ylab="Relative Frequency",legend=c("Fixed Size", "Fixed Degree Sequence"))

## We are clearly on the higher side



