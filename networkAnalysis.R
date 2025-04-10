# load libraries
library(readxl)
library(tidyverse)
library(ggplot2)
library(statnet)

# load in data
nodes <- read_excel("MAGrantsSample.xlsx", sheet = "nodeatts")
edges <- read_excel("MAGrantsSample.xlsx", sheet = "edges")

# adjust node data as necessary
nodes <- mutate(nodes,
                granteeEIN = na_if(granteeEIN, "NA"),
                granteeCity = na_if(granteeCity, "NA"),
                granteeCounty = na_if(granteeCounty, "NA"),
                granteeSite = na_if(granteeSite, "NA"),
                granteeType = na_if(granteeType, "NA"),
                granteeFounded = na_if(granteeFounded, "NA"),
                granteeDecFounded = na_if(granteeDecFounded, "NA"),
                grantorEIN = na_if(grantorEIN, "NA"),
                grantorCity = na_if(grantorCity, "NA"),
                grantorCounty = na_if(grantorCounty, "NA"),
                grantorSite = na_if(grantorSite, "NA"),
                grantorType = na_if(grantorType, "NA"),
                grantorFounded = na_if(grantorFounded, "NA"),
                grantorDecFounded = na_if(grantorDecFounded, "NA"),)
nodes <- select(nodes, orgName, orgNickname, granteeEIN, granteeCity, granteeCounty, granteeType, 
                granteeFounded, granteeDecFounded, grantorEIN, grantorCity, grantorCounty, 
                grantorType, grantorFounded, grantorDecFounded)
nodes <- mutate(nodes,
                granteeFounded = as.integer(granteeFounded),
                grantorFounded = as.integer(grantorFounded))
nodes <- mutate(nodes, granteeCentFounded = case_when(
  granteeFounded < 1800 ~ "1700s",
  granteeFounded < 1900 ~ "1800s",
  granteeFounded < 2000 ~ "1900s",
  granteeFounded < 2100 ~ "2000s",
  TRUE ~ "NA"
))
nodes$granteeCentFounded <- na_if(nodes$granteeCentFounded, "NA")
nodes <- mutate(nodes, grantorCentFounded = case_when(
  grantorFounded < 1700 ~ "1600s",
  grantorFounded < 1800 ~ "1700s",
  grantorFounded < 1900 ~ "1800s",
  grantorFounded < 2000 ~ "1900s",
  grantorFounded < 2100 ~ "2000s",
  TRUE ~ "NA"
))
nodes$grantorCentFounded <- na_if(nodes$grantorCentFounded, "NA")
nodes <- select(nodes, orgName:granteeDecFounded, granteeCentFounded, 
                grantorEIN:grantorDecFounded, grantorCentFounded)
nodes <- mutate(nodes, granteeCause = case_when(
  granteeType == "College" | granteeType == "School" ~ "Education",
  TRUE ~ as.character(granteeType)
))
nodes <- mutate(nodes, grantorCause = case_when(
  grantorType == "College" | grantorType == "School" ~ "Education",
  TRUE ~ as.character(grantorType)
))
nodes$granteeType <- NULL
nodes$grantorType <- NULL

# create network object
edge_matrix <- as.matrix(table(edges$granteeName, edges$grantorName))
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

# explore degrees
degrees <- sna::degree(ma_net, gmode = "graph")
grantees_degs <- degrees[1:48]
summary(grantees_degs)
grantors_degs <- degrees[49:98]
summary(grantors_degs)
