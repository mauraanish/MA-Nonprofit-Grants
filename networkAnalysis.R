# load libraries
library(readxl)
library(tidyverse)
library(statnet)

# load in data
nodes <- read_excel("MAGrantsSample.xlsx", sheet = "nodeatts")
edges <- read_excel("MAGrantsSample.xlsx", sheet = "edges")

# adjust node data as necessary
nodes <- mutate(nodes,
                granteeEIN = na_if(granteeEIN, "NA"),
                granteeCity = na_if(granteeCity, "NA"),
                granteeSite = na_if(granteeSite, "NA"),
                granteeType = na_if(granteeType, "NA"),
                granteeFounded = na_if(granteeFounded, "NA"),
                grantorEIN = na_if(grantorEIN, "NA"),
                grantorCity = na_if(grantorCity, "NA"),
                grantorSite = na_if(grantorSite, "NA"),
                grantorType = na_if(grantorType, "NA"),
                grantorFounded = na_if(grantorFounded, "NA"))
nodes <- select(nodes, orgname, granteeEIN, granteeCity, granteeType, granteeFounded,
                grantorEIN, grantorCity, grantorType, grantorFounded)
nodes <- mutate(nodes,
                granteeFounded = as.integer(granteeFounded),
                grantorFounded = as.integer(grantorFounded))

# create network object
edge_matrix <- as.matrix(table(edges$granteeein, edges$grantorein))
node_attrs_list <- do.call(list, nodes)
ma_net <- network(edge_matrix, bipartite = T, directed = F, vertex.attr = node_attrs_list)
summary(ma_net, print.adj = FALSE)
# Network attributes:
#  vertices = 98
#  directed = FALSE
#  hyper = FALSE
#  loops = FALSE
#  multiple = FALSE
#  bipartite = 48
# total edges = 102 
#   missing edges = 0 
#   non-missing edges = 102 
# density = 0.02146013 

# visualize network
node_color <- ifelse(is.na(nodes$grantorEIN), "black", "green")
gplot(ma_net, gmode = "twomode", usearrows = F,
      vertex.col = node_color, jitter = T, edge.col = "gray80",
      main = "MA Non-Profit Grant Network in 2015")
legend("bottomright", c("Grantor", "Grantee"),
       col = c("green", "black"), pch = c(15, 16), text.font = 1)
