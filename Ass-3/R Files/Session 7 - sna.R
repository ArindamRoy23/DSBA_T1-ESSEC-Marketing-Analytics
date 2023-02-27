# Library
# You may also need install.packages('statnet.common', repos = 'http://statnet.org')
library(sna)

# Cluster social network
network = matrix(c(0,1,1,1,0,0,0,0,0,0,
                   1,0,1,1,1,0,0,0,0,0,
                   1,1,0,0,0,0,0,0,0,0,
                   1,1,0,0,0,0,0,0,0,0,
                   0,1,0,0,0,1,1,0,0,0,
                   0,0,0,0,1,0,0,0,0,0,
                   0,0,0,0,1,0,0,1,1,1,
                   0,0,0,0,0,0,1,0,1,1,
                   0,0,0,0,0,0,1,1,0,1,
                   0,0,0,0,0,0,1,1,1,0),
                 nrow=10, ncol=10)
cores = kcores(network, mode = "graph")
print(cores)
plot(G, vertex.color = cores)
