# Library
library(igraph)

# Specify an undirected graph by hand, using a numeric
# vector of the pairs of vertices sharing an edge.
dyads = c(1, 2, 1, 3, 1, 4, 2, 3, 2, 4, 2, 5, 5, 6, 5, 7, 7, 8, 7, 9, 7, 10, 8, 9, 8, 10, 9, 10)

# Created undirected network from dyads
G <- graph(dyads, directed = FALSE)

# Vizualize network (the "plot" function is overwritten by igraph, so make sure you load igraph last, not first)
plot(G)

# Vizualize with new options
plot(G, layout = layout.fruchterman.reingold,
     vertex.size = 25,
     vertex.color = "darkblue",
     vertex.frame.color = "white",
     vertex.label.color = "white",
     vertex.label.family = "sans",
     edge.width=2,
     edge.color = "black")

# Create a data frame
data = data.frame(id = 1:10)

# Compute degree and betweenness
data$degree  = degree(G)
data$between = betweenness(G)

# Compute and scale closeness in a way that makes more sense (to me)
data$close   = 1 / closeness(G, normalized = TRUE)

# Print results
print(data)
