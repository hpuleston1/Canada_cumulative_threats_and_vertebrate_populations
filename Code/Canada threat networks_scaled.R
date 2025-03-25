########## CANADA THREAT NETWORKS ##########

##### PLOTS #####


### All marine

library(igraph)
setwd("~/R/Canada/Scaled_split")

nodes_marineS = nodes_marine
colnames(nodes_marineS)[colnames(nodes_marineS) == 'values'] <- 'values_old'
nodes_marineS$values <- nodes_marineS$values_old / (nrow(canada_pops_threatened_marine)) * 100

edges_marineS = edges_marine2
colnames(edges_marineS)[colnames(edges_marineS) == 'values'] <- 'values_old'
edges_marineS$values <- edges_marineS$values_old / (nrow(canada_pops_threatened_marine)) * 100

g_marineS = graph_from_data_frame(edges_marineS, vertices=nodes_marineS, directed=FALSE)

node.size_marineS <- setNames(c(nodes_marineS$values),c(nodes_marineS$ind))
E(g_marineS)$width <- edges_marineS$values * 0.8

pdf("threats_drl_marine_scaled.pdf",10,10)
igraph.options(
  plot.layout = layout.drl,
  vertex.size = node.size_marineS * 0.8,
  vertex.label.dist = 0,
  vertex.label.color = "darkslategray",
  vertex.frame.color = NA,
  V(g_marineS)$color <- "deepskyblue"
)
plot(g_marineS)
dev.off()

write.csv(nodes_marineS, "nodes_marineS_counts.csv")



### All terrfw

library(igraph)
setwd("~/R/Canada/Scaled_split")

nodes_terrfwS = nodes_terrfw
colnames(nodes_terrfwS)[colnames(nodes_terrfwS) == 'values'] <- 'values_old'
nodes_terrfwS$values <- nodes_terrfwS$values_old / (nrow(canada_pops_threatened_terrfw)) * 100

edges_terrfwS = edges_terrfw2
colnames(edges_terrfwS)[colnames(edges_terrfwS) == 'values'] <- 'values_old'
edges_terrfwS$values <- edges_terrfwS$values_old / (nrow(canada_pops_threatened_terrfw)) * 100

g_terrfwS = graph_from_data_frame(edges_terrfwS, vertices=nodes_terrfwS, directed=FALSE)

node.size_terrfwS <- setNames(c(nodes_terrfwS$values),c(nodes_terrfwS$ind))
E(g_terrfwS)$width <- edges_terrfwS$values * 0.4

pdf("threats_drl_terrfw_scaled.pdf",10,10)
igraph.options(
  plot.layout = layout.drl,
  vertex.size = node.size_terrfwS * 0.4,
  vertex.label.dist = 0,
  vertex.label.color = "darkslategray",
  vertex.frame.color = NA,
  V(g_terrfwS)$color <- "gold"
)
plot(g_terrfwS)
dev.off()

write.csv(nodes_terrfwS, "nodes_terrfwS_counts.csv")




# Marine, class

#library(plyr)
library(dplyr)
library(igraph)

# Fish
nodes_marine_fishS = nodes_marine_fish
colnames(nodes_marine_fishS)[colnames(nodes_marine_fishS) == 'values'] <- 'values_old'
nodes_marine_fishS$values <- nodes_marine_fishS$values_old / (nrow(canada_pops_threatened_marine_fish)) * 100

edges_marine_fishS = edges_marine_fish2
colnames(edges_marine_fishS)[colnames(edges_marine_fishS) == 'values'] <- 'values_old'
edges_marine_fishS$values <- edges_marine_fishS$values_old / (nrow(canada_pops_threatened_marine_fish)) * 100

g_marine_fishS = graph_from_data_frame(edges_marine_fishS, vertices = nodes_marine_fishS, directed =
                                         FALSE)
node.size_marine_fishS <- setNames(c(nodes_marine_fishS$values), c(nodes_marine_fishS$ind))
E(g_marine_fishS)$width <- edges_marine_fishS$values * 0.8

pdf("threats_drl_marine_fish_scaled.pdf", 10, 10)
igraph.options(
  plot.layout = layout.drl,
  vertex.size = node.size_marine_fishS * 0.8,
  vertex.label.dist = 0,
  vertex.frame.color = NA,
  vertex.label.color = "darkslategray",
  V(g_marine_fishS)$color <- "deepskyblue"
)
plot(g_marine_fishS)
dev.off()

write.csv(nodes_marine_fishS, "nodes_marine_fishS_counts.csv")

# Mammals
nodes_marine_mammalsS = nodes_marine_mammals
colnames(nodes_marine_mammalsS)[colnames(nodes_marine_mammalsS) == 'values'] <- 'values_old'
nodes_marine_mammalsS$values <- nodes_marine_mammalsS$values_old / (nrow(canada_pops_threatened_marine_mammals)) * 100

edges_marine_mammalsS = edges_marine_mammals2
colnames(edges_marine_mammalsS)[colnames(edges_marine_mammalsS) == 'values'] <- 'values_old'
edges_marine_mammalsS$values <- edges_marine_mammalsS$values_old / (nrow(canada_pops_threatened_marine_mammals)) * 100

g_marine_mammalsS = graph_from_data_frame(edges_marine_mammalsS, vertices = nodes_marine_mammalsS, directed =
                                            FALSE)
node.size_marine_mammalsS <- setNames(c(nodes_marine_mammalsS$values), c(nodes_marine_mammalsS$ind))
E(g_marine_mammalsS)$width <- edges_marine_mammalsS$values * 0.8

pdf("threats_drl_marine_mammals_scaled.pdf", 10, 10)
igraph.options(
  plot.layout = layout.drl,
  vertex.size = node.size_marine_mammalsS * 0.8,
  vertex.label.dist = 0,
  vertex.frame.color = NA,
  vertex.label.color = "darkslategray",
  V(g_marine_mammalsS)$color <- "deepskyblue"
)
plot(g_marine_mammalsS)
dev.off()

write.csv(nodes_marine_mammalsS, "nodes_marine_mammalsS_counts.csv")

# Herps
nodes_marine_herpsS = nodes_marine_herps
colnames(nodes_marine_herpsS)[colnames(nodes_marine_herpsS) == 'values'] <- 'values_old'
nodes_marine_herpsS$values <- nodes_marine_herpsS$values_old / (nrow(canada_pops_threatened_marine_herps)) * 100

edges_marine_herpsS = edges_marine_herps2
colnames(edges_marine_herpsS)[colnames(edges_marine_herpsS) == 'values'] <- 'values_old'
edges_marine_herpsS$values <- edges_marine_herpsS$values_old / (nrow(canada_pops_threatened_marine_herps)) * 100

g_marine_herpsS = graph_from_data_frame(edges_marine_herpsS, vertices = nodes_marine_herpsS, directed =
                                          FALSE)
node.size_marine_herpsS <- setNames(c(nodes_marine_herpsS$values), c(nodes_marine_herpsS$ind))
E(g_marine_herpsS)$width <- edges_marine_herpsS$values * 0.8

pdf("threats_drl_marine_herps_scaled.pdf", 10, 10)
igraph.options(
  plot.layout = layout.drl,
  vertex.size = node.size_marine_herpsS * 0.8,
  vertex.label.dist = 0,
  vertex.frame.color = NA,
  vertex.label.color = "darkslategray",
  V(g_marine_herpsS)$color <- "deepskyblue"
)
plot(g_marine_herpsS)
dev.off()

write.csv(nodes_marine_herpsS, "nodes_marine_herpsS_counts.csv")


# Birds
nodes_marine_birdsS = nodes_marine_birds
colnames(nodes_marine_birdsS)[colnames(nodes_marine_birdsS) == 'values'] <- 'values_old'
nodes_marine_birdsS$values <- nodes_marine_birdsS$values_old / (nrow(canada_pops_threatened_marine_birds)) * 100

edges_marine_birdsS = edges_marine_birds2
colnames(edges_marine_birdsS)[colnames(edges_marine_birdsS) == 'values'] <- 'values_old'
edges_marine_birdsS$values <- edges_marine_birdsS$values_old / (nrow(canada_pops_threatened_marine_birds)) * 100

g_marine_birdsS = graph_from_data_frame(edges_marine_birdsS, vertices = nodes_marine_birdsS, directed =
                                          FALSE)
node.size_marine_birdsS <- setNames(c(nodes_marine_birdsS$values), c(nodes_marine_birdsS$ind))
E(g_marine_birdsS)$width <- edges_marine_birdsS$values * 0.8

pdf("threats_drl_marine_birds_scaled.pdf", 10, 10)
igraph.options(
  plot.layout = layout.drl,
  vertex.size = node.size_marine_birdsS * 0.8,
  vertex.label.dist = 0,
  vertex.frame.color = NA,
  vertex.label.color = "darkslategray",
  V(g_marine_birdsS)$color <- "deepskyblue"
)
plot(g_marine_birdsS)
dev.off()

write.csv(nodes_marine_birdsS, "nodes_marine_birdsS_counts.csv")


nodes_marine_classS <- list(nodes_marine_fishS, nodes_marine_herpsS, nodes_marine_mammalsS, nodes_marine_birdsS)
nodes_marine_classS  <- nodes_marine_fishS %>% full_join(nodes_marine_herpsS,  by = "ind") %>% full_join(nodes_marine_mammalsS,  by = "ind") %>% full_join(nodes_marine_birdsS,  by = "ind")
colnames(nodes_marine_classS) <- c("Threat", "Marine_fish", "Marine_fishS", "Marine_herps", "Marine_herpsS", "Marine_mammals", "Marine_mammalsS", "Marine_birds", "Marine_birdsS")
write.csv(nodes_marine_classS, "nodes_marine_classS_counts.csv")


# Terrfw, class

#library(plyr)
library(dplyr)
library(igraph)

# Fish
nodes_terrfw_fishS = nodes_terrfw_fish
colnames(nodes_terrfw_fishS)[colnames(nodes_terrfw_fishS) == 'values'] <- 'values_old'
nodes_terrfw_fishS$values <- nodes_terrfw_fishS$values_old / (nrow(canada_pops_threatened_terrfw_fish)) * 100

edges_terrfw_fishS = edges_terrfw_fish2
colnames(edges_terrfw_fishS)[colnames(edges_terrfw_fishS) == 'values'] <- 'values_old'
edges_terrfw_fishS$values <- edges_terrfw_fishS$values_old / (nrow(canada_pops_threatened_terrfw_fish)) * 100

g_terrfw_fishS = graph_from_data_frame(edges_terrfw_fishS, vertices = nodes_terrfw_fishS, directed =
                                         FALSE)
node.size_fish_terrfwS <- setNames(c(nodes_terrfw_fishS$values), c(nodes_terrfw_fishS$ind))
E(g_terrfw_fishS)$width <- edges_terrfw_fishS$values * 0.4

pdf("threats_drl_terrfw_fish_scaled.pdf", 10, 10)
igraph.options(
  plot.layout = layout.drl,
  vertex.size = node.size_fish_terrfwS * 0.4,
  vertex.label.dist = 0,
  vertex.frame.color = NA,
  vertex.label.color = "darkslategray",
  V(g_terrfw_fishS)$color <- "gold"
)
plot(g_terrfw_fishS)
dev.off()

write.csv(nodes_terrfw_fishS, "nodes_terrfw_fishS_counts.csv")

# Mammals
nodes_terrfw_mammalsS = nodes_terrfw_mammals
colnames(nodes_terrfw_mammalsS)[colnames(nodes_terrfw_mammalsS) == 'values'] <- 'values_old'
nodes_terrfw_mammalsS$values <- nodes_terrfw_mammalsS$values_old / (nrow(canada_pops_threatened_terrfw_mammals)) * 100

edges_terrfw_mammalsS = edges_terrfw_mammals2
colnames(edges_terrfw_mammalsS)[colnames(edges_terrfw_mammalsS) == 'values'] <- 'values_old'
edges_terrfw_mammalsS$values <- edges_terrfw_mammalsS$values_old / (nrow(canada_pops_threatened_terrfw_mammals)) * 100

g_terrfw_mammalsS = graph_from_data_frame(edges_terrfw_mammalsS, vertices = nodes_terrfw_mammalsS, directed =
                                            FALSE)
node.size_terrfw_mammalsS <- setNames(c(nodes_terrfw_mammalsS$values), c(nodes_terrfw_mammalsS$ind))
E(g_terrfw_mammalsS)$width <- edges_terrfw_mammalsS$values * 0.4

pdf("threats_drl_terrfw_mammals_scaled.pdf", 10, 10)
igraph.options(
  plot.layout = layout.drl,
  vertex.size = node.size_terrfw_mammalsS * 0.4,
  vertex.label.dist = 0,
  vertex.frame.color = NA,
  vertex.label.color = "darkslategray",
  V(g_terrfw_mammalsS)$color <- "gold"
)
plot(g_terrfw_mammalsS)
dev.off()

write.csv(nodes_terrfw_mammalsS, "nodes_terrfw_mammalsS_counts.csv")

# Herps
nodes_terrfw_herpsS = nodes_terrfw_herps
colnames(nodes_terrfw_herpsS)[colnames(nodes_terrfw_herpsS) == 'values'] <- 'values_old'
nodes_terrfw_herpsS$values <- nodes_terrfw_herpsS$values_old / (nrow(canada_pops_threatened_terrfw_herps)) * 100

edges_terrfw_herpsS = edges_terrfw_herps2
colnames(edges_terrfw_herpsS)[colnames(edges_terrfw_herpsS) == 'values'] <- 'values_old'
edges_terrfw_herpsS$values <- edges_terrfw_herpsS$values_old / (nrow(canada_pops_threatened_terrfw_herps)) * 100

g_terrfw_herpsS = graph_from_data_frame(edges_terrfw_herpsS, vertices = nodes_terrfw_herpsS, directed =
                                          FALSE)
node.size_terrfw_herpsS <- setNames(c(nodes_terrfw_herpsS$values), c(nodes_terrfw_herpsS$ind))
E(g_terrfw_herpsS)$width <- edges_terrfw_herpsS$values * 0.4

pdf("threats_drl_terrfw_herps_scaled.pdf", 10, 10)
igraph.options(
  plot.layout = layout.drl,
  vertex.size = node.size_terrfw_herpsS * 0.4,
  vertex.label.dist = 0,
  vertex.frame.color = NA,
  vertex.label.color = "darkslategray",
  V(g_terrfw_herpsS)$color <- "gold"
)
plot(g_terrfw_herpsS)
dev.off()

write.csv(nodes_terrfw_herpsS, "nodes_terrfw_herpsS_counts.csv")


# Birds
nodes_terrfw_birdsS = nodes_terrfw_birds
colnames(nodes_terrfw_birdsS)[colnames(nodes_terrfw_birdsS) == 'values'] <- 'values_old'
nodes_terrfw_birdsS$values <- nodes_terrfw_birdsS$values_old / (nrow(canada_pops_threatened_terrfw_birds)) * 100

edges_terrfw_birdsS = edges_terrfw_birds2
colnames(edges_terrfw_birdsS)[colnames(edges_terrfw_birdsS) == 'values'] <- 'values_old'
edges_terrfw_birdsS$values <- edges_terrfw_birdsS$values_old / (nrow(canada_pops_threatened_terrfw_birds)) * 100

g_terrfw_birdsS = graph_from_data_frame(edges_terrfw_birdsS, vertices = nodes_terrfw_birdsS, directed =
                                          FALSE)
node.size_terrfw_birdsS <- setNames(c(nodes_terrfw_birdsS$values), c(nodes_terrfw_birdsS$ind))
E(g_terrfw_birdsS)$width <- edges_terrfw_birdsS$values * 0.4

pdf("threats_drl_terrfw_birds_scaled.pdf", 10, 10)
igraph.options(
  plot.layout = layout.drl,
  vertex.size = node.size_terrfw_birdsS * 0.4,
  vertex.label.dist = 0,
  vertex.frame.color = NA,
  vertex.label.color = "darkslategray",
  V(g_terrfw_birdsS)$color <- "gold"
)
plot(g_terrfw_birdsS)
dev.off()

write.csv(nodes_terrfw_birdsS, "nodes_terrfw_birdsS_counts.csv")


nodes_terrfw_classS <- list(nodes_terrfw_fishS, nodes_terrfw_herpsS, nodes_terrfw_mammalsS, nodes_terrfw_birdsS)
nodes_terrfw_classS  <- nodes_terrfw_fishS %>% full_join(nodes_terrfw_herpsS,  by = "ind") %>% full_join(nodes_terrfw_mammalsS,  by = "ind") %>% full_join(nodes_terrfw_birdsS,  by = "ind")
colnames(nodes_terrfw_classS) <- c("Threat", "Terrfw_fish", "Terrfw_fishS", "Terrfw_herps", "Terrfw_herpsS", "Terrfw_mammals", "Terrfw_mammalsS", "Terrfw_birds", "Terrfw_birdsS")
write.csv(nodes_terrfw_classS, "nodes_terrfw_classS_counts.csv")


## Counts
threatened_pop_n <- nrow(canada_pops_threatened)
threatened_spp_n <- length(unique(canada_pops_threatened$Binomial))

threatened_marine_pop_n <- nrow(canada_pops_threatened_marine)
threatened_marine_spp_n <- length(unique(canada_pops_threatened_marine$Binomial))

threatened_terrfw_pop_n <- nrow(canada_pops_threatened_terrfw)
threatened_terrfw_spp_n <- length(unique(canada_pops_threatened_terrfw$Binomial))

threatened_marine_fish_pop_n <- nrow(canada_pops_threatened_marine_fish)
threatened_marine_fish_spp_n <- length(unique(canada_pops_threatened_marine_fish$Binomial))
threatened_marine_mammals_pop_n <- nrow(canada_pops_threatened_marine_mammals)
threatened_marine_mammals_spp_n <- length(unique(canada_pops_threatened_marine_mammals$Binomial))
threatened_marine_herps_pop_n <- nrow(canada_pops_threatened_marine_herps)
threatened_marine_herps_spp_n <- length(unique(canada_pops_threatened_marine_herps$Binomial))
threatened_marine_birds_pop_n <- nrow(canada_pops_threatened_marine_birds)
threatened_marine_birds_spp_n <- length(unique(canada_pops_threatened_marine_birds$Binomial))

threatened_terrfw_fish_pop_n <- nrow(canada_pops_threatened_terrfw_fish)
threatened_terrfw_fish_spp_n <- length(unique(canada_pops_threatened_terrfw_fish$Binomial))
threatened_terrfw_mammals_pop_n <- nrow(canada_pops_threatened_terrfw_mammals)
threatened_terrfw_mammals_spp_n <- length(unique(canada_pops_threatened_terrfw_mammals$Binomial))
threatened_terrfw_herps_pop_n <- nrow(canada_pops_threatened_terrfw_herps)
threatened_terrfw_herps_spp_n <- length(unique(canada_pops_threatened_terrfw_herps$Binomial))
threatened_terrfw_birds_pop_n <- nrow(canada_pops_threatened_terrfw_birds)
threatened_terrfw_birds_spp_n <- length(unique(canada_pops_threatened_terrfw_birds$Binomial))

n_values_threatened_marine <- matrix(c(threatened_marine_fish_pop_n, threatened_marine_mammals_pop_n, threatened_marine_herps_pop_n, threatened_marine_birds_pop_n, threatened_marine_pop_n, threatened_marine_fish_spp_n, threatened_marine_mammals_spp_n, threatened_marine_herps_spp_n, threatened_marine_birds_spp_n, threatened_marine_spp_n), ncol=2)
rownames(n_values_threatened_marine) <- c('Fish', 'Mammals', 'Herptiles', 'Birds', 'TOTAL')
colnames(n_values_threatened_marine) <- c('Populations', 'Species')
n_values_threatened_marine <- as.table(n_values_threatened_marine)

n_values_threatened_terrfw <- matrix(c(threatened_terrfw_fish_pop_n, threatened_terrfw_mammals_pop_n, threatened_terrfw_herps_pop_n, threatened_terrfw_birds_pop_n, threatened_terrfw_pop_n, threatened_terrfw_fish_spp_n, threatened_terrfw_mammals_spp_n, threatened_terrfw_herps_spp_n, threatened_terrfw_birds_spp_n, threatened_terrfw_spp_n), ncol=2)
rownames(n_values_threatened_terrfw) <- c('Fish', 'Mammals', 'Herptiles', 'Birds', 'TOTAL')
colnames(n_values_threatened_terrfw) <- c('Populations', 'Species')
n_values_threatened_terrfw <- as.table(n_values_threatened_terrfw)