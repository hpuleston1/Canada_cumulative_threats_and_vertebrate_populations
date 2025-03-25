########## CANADA THREAT NETWORKS ##########


##### PREP #####

### Load libraries
#library(tidyr)
#library(plyr)
library(dplyr)
library(ggplot2)

### Set working directory
setwd("~/R/Canada/Prep")

### Read in LPI data
canada_data_all <- read.csv("FINAL_WWF_Canada_dataset_140824_updated.csv", header = T)
canada_data = canada_data_all
canada_data <- subset(canada_data_all,canada_data_all$Replicate == '0')

### Read in Threat data
threat_data <- read.csv("Merged_threat_network_wideform_dataset_20250314.csv", header = T)
colnames(threat_data)

names(threat_data) = gsub(pattern = "_R_threat_grid.csv", replacement = "", x = names(threat_data))
names(threat_data) = gsub(pattern = "_proj_threat_grid.csv", replacement = "", x = names(threat_data))
names(threat_data) = gsub(pattern = "_proj_add_threat_grid.csv", replacement = "", x = names(threat_data))
names(threat_data) = gsub(pattern = "_threat_grid.csv", replacement = "", x = names(threat_data))

names(threat_data) = gsub(pattern = "access_ports_proj", replacement = "AccPorts", x = names(threat_data))
names(threat_data) = gsub(pattern = "access_proj", replacement = "AccCities", x = names(threat_data))
names(threat_data) = gsub(pattern = "artisanal_fishing", replacement = "FishArt", x = names(threat_data))
names(threat_data) = gsub(pattern = "cattle_density", replacement = "Cattle", x = names(threat_data))
names(threat_data) = gsub(pattern = "chemgrids_total_sum", replacement = "Pest", x = names(threat_data))
names(threat_data) = gsub(pattern = "clim_diff_map", replacement = "Precip", x = names(threat_data))
names(threat_data) = gsub(pattern = "clim_diff_mat", replacement = "Temp", x = names(threat_data))
names(threat_data) = gsub(pattern = "coastal_pollution", replacement = "PollCoast", x = names(threat_data))
names(threat_data) = gsub(pattern = "dams", replacement = "Dams", x = names(threat_data))
names(threat_data) = gsub(pattern = "fishing_proj", replacement = "FishComm", x = names(threat_data))
names(threat_data) = gsub(pattern = "forest_loss_intensity", replacement = "ForestLoss", x = names(threat_data))
names(threat_data) = gsub(pattern = "human_pop", replacement = "PopDens", x = names(threat_data))
names(threat_data) = gsub(pattern = "invasives", replacement = "Invasives", x = names(threat_data))
names(threat_data) = gsub(pattern = "lc_crop_intensity", replacement = "Crops", x = names(threat_data))
names(threat_data) = gsub(pattern = "lc_urb_intensity", replacement = "Urban", x = names(threat_data))
names(threat_data) = gsub(pattern = "marine_light", replacement = "PollLight", x = names(threat_data))
names(threat_data) = gsub(pattern = "N_appliction_total_sum", replacement = "Fert", x = names(threat_data))
names(threat_data) = gsub(pattern = "ocean_acidification", replacement = "OceanAcid", x = names(threat_data))
names(threat_data) = gsub(pattern = "ocean_pollution", replacement = "PollOcean", x = names(threat_data))
names(threat_data) = gsub(pattern = "oil_gas", replacement = "Extract", x = names(threat_data))
names(threat_data) = gsub(pattern = "oil_rigs", replacement = "OilRigs", x = names(threat_data))
names(threat_data) = gsub(pattern = "rail_network", replacement = "Rail", x = names(threat_data))
names(threat_data) = gsub(pattern = "road_intensity", replacement = "Roads", x = names(threat_data))
names(threat_data) = gsub(pattern = "shipping", replacement = "Shipping", x = names(threat_data))
names(threat_data) = gsub(pattern = "slr", replacement = "SeaLevelRise", x = names(threat_data))
names(threat_data) = gsub(pattern = "sst", replacement = "AnomSST", x = names(threat_data))
names(threat_data) = gsub(pattern = "uv", replacement = "AnomUV", x = names(threat_data))

### Add sum of threats
colnames(threat_data)
threat_data$Sum_threats = rowSums(threat_data[,c(10:36)])

### Merge threat data with Canada data
canada_pops <- merge(x = canada_data, y = threat_data, by = "ID", all.x=TRUE)

### Subset to threatened pops
canada_pops_threatened <- subset(canada_pops, Sum_threats != "0")
write.csv(canada_pops_threatened, "canada_pops_threatened.csv")

### Subset to threatened marine pops
canada_pops_threatened_marine <- subset(canada_pops_threatened, marine_threat != "0")
write.csv(canada_pops_threatened_marine, "canada_pops_threatened_marine.csv")

### Subset to threatened terr/fw pops
canada_pops_threatened_terrfw <- subset(canada_pops_threatened, terr_threat != "0")
write.csv(canada_pops_threatened_terrfw, "canada_pops_threatened_terrfw.csv")

### n value check
nrow(canada_pops_threatened)
nrow(canada_pops_threatened_marine) + nrow(canada_pops_threatened_terrfw)



##### NETWORKS PREP #####

setwd("~/R/Canada/Network_counts")

### Counting nodes

## Nodes - all
nodes <- stack(colSums(canada_pops_threatened[,c(108:134)]))[2:1]
nodes <- nodes[nodes$values != 0, ]
write.csv(nodes, "nodes_counts.csv")

## Nodes - Class
canada_pops_threatened_fish <- subset(canada_pops_threatened, canada_pops_threatened$Taxonomic_group == 'Fish')
canada_pops_threatened_herps <- subset(canada_pops_threatened, canada_pops_threatened$Taxonomic_group == 'Herps')
canada_pops_threatened_mammals <- subset(canada_pops_threatened, canada_pops_threatened$Taxonomic_group == 'Mammals')
canada_pops_threatened_birds <- subset(canada_pops_threatened, canada_pops_threatened$Taxonomic_group == 'Birds')

nodes_fish <- stack(colSums(canada_pops_threatened_fish[,c(108:134)]))[2:1]
nodes_fish <- nodes_fish[nodes_fish$values != 0, ]
nodes_herps <- stack(colSums(canada_pops_threatened_herps[,c(108:134)]))[2:1]
nodes_herps <- nodes_herps[nodes_herps$values != 0, ]
nodes_mammals <- stack(colSums(canada_pops_threatened_mammals[,c(108:134)]))[2:1]
nodes_mammals <- nodes_mammals[nodes_mammals$values != 0, ]
nodes_birds<- stack(colSums(canada_pops_threatened_birds[,c(108:134)]))[2:1]
nodes_birds <- nodes_birds[nodes_birds$values != 0, ]

nodes_class <- list(nodes_fish, nodes_herps, nodes_mammals, nodes_birds)
nodes_class  <- nodes_fish %>% full_join(nodes_herps,  by = "ind") %>% full_join(nodes_mammals,  by = "ind") %>% full_join(nodes_birds,  by = "ind")
colnames(nodes_class) <- c("Threat", "Fish", "Herps", "Mammals", "Birds")
write.csv(nodes_class, "nodes_class_counts.csv")


## Nodes - all, marine
nodes_marine <- stack(colSums(canada_pops_threatened_marine[,c(108:134)]))[2:1]
nodes_marine <- nodes_marine[nodes_marine$values != 0, ]
write.csv(nodes_marine, "nodes_marine_counts.csv")

## Nodes - all, fw/terr
nodes_terrfw <- stack(colSums(canada_pops_threatened_terrfw[,c(108:134)]))[2:1]
nodes_terrfw <- nodes_terrfw[nodes_terrfw$values != 0, ]
write.csv(nodes_terrfw, "nodes_terrfw_counts.csv")

## Nodes - marine, Class
canada_pops_threatened_marine_fish <- subset(canada_pops_threatened_marine, canada_pops_threatened_marine$Taxonomic_group == 'Fish')
canada_pops_threatened_marine_herps <- subset(canada_pops_threatened_marine, canada_pops_threatened_marine$Taxonomic_group == 'Herps')
canada_pops_threatened_marine_mammals <- subset(canada_pops_threatened_marine, canada_pops_threatened_marine$Taxonomic_group == 'Mammals')
canada_pops_threatened_marine_birds <- subset(canada_pops_threatened_marine, canada_pops_threatened_marine$Taxonomic_group == 'Birds')

nodes_marine_fish <- stack(colSums(canada_pops_threatened_marine_fish[,c(108:134)]))[2:1]
nodes_marine_fish <- nodes_marine_fish[nodes_marine_fish$values != 0, ]
nodes_marine_herps <- stack(colSums(canada_pops_threatened_marine_herps[,c(108:134)]))[2:1]
nodes_marine_herps <- nodes_marine_herps[nodes_marine_herps$values != 0, ]
nodes_marine_mammals <- stack(colSums(canada_pops_threatened_marine_mammals[,c(108:134)]))[2:1]
nodes_marine_mammals <- nodes_marine_mammals[nodes_marine_mammals$values != 0, ]
nodes_marine_birds<- stack(colSums(canada_pops_threatened_marine_birds[,c(108:134)]))[2:1]
nodes_marine_birds <- nodes_marine_birds[nodes_marine_birds$values != 0, ]

nodes_marine_class <- list(nodes_marine_fish, nodes_marine_herps, nodes_marine_mammals, nodes_marine_birds)
nodes_marine_class  <- nodes_marine_fish %>% full_join(nodes_marine_herps,  by = "ind") %>% full_join(nodes_marine_mammals,  by = "ind") %>% full_join(nodes_marine_birds,  by = "ind")
colnames(nodes_marine_class) <- c("Threat", "Fish", "Herps", "Mammals", "Birds")
write.csv(nodes_marine_class, "nodes_marine_class_counts.csv")

## Nodes - Class, fw/terr
canada_pops_threatened_terrfw_fish <- subset(canada_pops_threatened_terrfw, canada_pops_threatened_terrfw$Taxonomic_group == 'Fish')
canada_pops_threatened_terrfw_herps <- subset(canada_pops_threatened_terrfw, canada_pops_threatened_terrfw$Taxonomic_group == 'Herps')
canada_pops_threatened_terrfw_mammals <- subset(canada_pops_threatened_terrfw, canada_pops_threatened_terrfw$Taxonomic_group == 'Mammals')
canada_pops_threatened_terrfw_birds <- subset(canada_pops_threatened_terrfw, canada_pops_threatened_terrfw$Taxonomic_group == 'Birds')

nodes_terrfw_fish <- stack(colSums(canada_pops_threatened_terrfw_fish[,c(108:134)]))[2:1]
nodes_terrfw_fish <- nodes_terrfw_fish[nodes_terrfw_fish$values != 0, ]
nodes_terrfw_herps <- stack(colSums(canada_pops_threatened_terrfw_herps[,c(108:134)]))[2:1]
nodes_terrfw_herps <- nodes_terrfw_herps[nodes_terrfw_herps$values != 0, ]
nodes_terrfw_mammals <- stack(colSums(canada_pops_threatened_terrfw_mammals[,c(108:134)]))[2:1]
nodes_terrfw_mammals <- nodes_terrfw_mammals[nodes_terrfw_mammals$values != 0, ]
nodes_terrfw_birds <- stack(colSums(canada_pops_threatened_terrfw_birds[,c(108:134)]))[2:1]
nodes_terrfw_birds <- nodes_terrfw_birds[nodes_terrfw_birds$values != 0, ]

nodes_terrfw_class <- list(nodes_terrfw_fish, nodes_terrfw_herps, nodes_terrfw_mammals, nodes_terrfw_birds)
nodes_terrfw_class  <- nodes_terrfw_fish %>% full_join(nodes_terrfw_herps,  by = "ind") %>% full_join(nodes_terrfw_mammals,  by = "ind") %>% full_join(nodes_terrfw_birds,  by = "ind")
colnames(nodes_terrfw_class) <- c("Threat", "Fish", "Herps", "Mammals", "Birds")
write.csv(nodes_terrfw_class, "nodes_terrfw_class_counts.csv")


### Counting edges

## Edges - all

count.or <- function(canada_pops_threatened, n = 2) {
  or.sum <- function(cols) sum(rowSums(canada_pops_threatened[cols]) > 1)
  counts <- combn(colnames(canada_pops_threatened[,c(108:134)]), n, FUN = or.sum)
  names  <- combn(colnames(canada_pops_threatened[,c(108:134)]), n, FUN = paste, collapse = "-")
  setNames(counts, names)
}

edges <- stack(count.or(canada_pops_threatened, 2))[2:1]
edges <- edges[edges$values != 0, ]
edges2 <- tidyr::separate(edges, ind, c('Threat 1', 'Threat 2'), sep = '-')
write.csv(edges2, "edges_all.csv")


## Edges - class
# Fish
count.or_fish <- function(canada_pops_threatened_fish, n = 2) {
  or.sum <- function(cols) sum(rowSums(canada_pops_threatened_fish[cols]) > 1)
  counts <- combn(colnames(canada_pops_threatened_fish[,c(108:134)]), n, FUN = or.sum)
  names  <- combn(colnames(canada_pops_threatened_fish[,c(108:134)]), n, FUN = paste, collapse = "-")
  setNames(counts, names)
}

edges_fish <- stack(count.or_fish(canada_pops_threatened_fish, 2))[2:1]
edges_fish <- edges_fish[edges_fish$values != 0, ]
edges_fish2 <- tidyr::separate(edges_fish, ind, c('Threat 1', 'Threat 2'), sep = '-')

# Mammals
count.or_mammals <- function(canada_pops_threatened_mammals, n = 2) {
  or.sum <- function(cols) sum(rowSums(canada_pops_threatened_mammals[cols]) > 1)
  counts <- combn(colnames(canada_pops_threatened_mammals[,c(108:134)]), n, FUN = or.sum)
  names  <- combn(colnames(canada_pops_threatened_mammals[,c(108:134)]), n, FUN = paste, collapse = "-")
  setNames(counts, names)
}

edges_mammals <- stack(count.or_mammals(canada_pops_threatened_mammals, 2))[2:1]
edges_mammals <- edges_mammals[edges_mammals$values != 0, ]
edges_mammals2 <- tidyr::separate(edges_mammals, ind, c('Threat 1', 'Threat 2'), sep = '-')

# Herps
count.or_herps <- function(canada_pops_threatened_herps, n = 2) {
  or.sum <- function(cols) sum(rowSums(canada_pops_threatened_herps[cols]) > 1)
  counts <- combn(colnames(canada_pops_threatened_herps[,c(108:134)]), n, FUN = or.sum)
  names  <- combn(colnames(canada_pops_threatened_herps[,c(108:134)]), n, FUN = paste, collapse = "-")
  setNames(counts, names)
}

edges_herps <- stack(count.or_herps(canada_pops_threatened_herps, 2))[2:1]
edges_herps <- edges_herps[edges_herps$values != 0, ]
edges_herps2 <- tidyr::separate(edges_herps, ind, c('Threat 1', 'Threat 2'), sep = '-')

# Birds
count.or_birds <- function(canada_pops_threatened_birds, n = 2) {
  or.sum <- function(cols) sum(rowSums(canada_pops_threatened_birds[cols]) > 1)
  counts <- combn(colnames(canada_pops_threatened_birds[,c(108:134)]), n, FUN = or.sum)
  names  <- combn(colnames(canada_pops_threatened_birds[,c(108:134)]), n, FUN = paste, collapse = "-")
  setNames(counts, names)
}

edges_birds <- stack(count.or_birds(canada_pops_threatened_birds, 2))[2:1]
edges_birds <- edges_birds[edges_birds$values != 0, ]
edges_birds2 <- tidyr::separate(edges_birds, ind, c('Threat 1', 'Threat 2'), sep = '-')


edges_class <- list(edges_fish, edges_herps, edges_mammals, edges_birds)
edges_class  <- edges_fish %>% full_join(edges_herps,  by = "ind") %>% full_join(edges_mammals,  by = "ind") %>% full_join(edges_birds,  by = "ind")
edges_class2 <- tidyr::separate(edges_class, ind, c('Threat 1', 'Threat 2'), sep = '-')
colnames(edges_class2) <- c("Threat1", "Threat2", "Fish", "Herps", "Mammals", "Birds")
write.csv(edges_class2, "edges_class_counts.csv")


## Edges - marine, all

count.or_marine <- function(canada_pops_threatened_marine, n = 2) {
  or.sum <- function(cols) sum(rowSums(canada_pops_threatened_marine[cols]) > 1)
  counts <- combn(colnames(canada_pops_threatened_marine[,c(108:134)]), n, FUN = or.sum)
  names  <- combn(colnames(canada_pops_threatened_marine[,c(108:134)]), n, FUN = paste, collapse = "-")
  setNames(counts, names)
}

edges_marine <- stack(count.or_marine(canada_pops_threatened_marine, 2))[2:1]
edges_marine <- edges_marine[edges_marine$values != 0, ]
edges_marine2 <- tidyr::separate(edges_marine, ind, c('Threat 1', 'Threat 2'), sep = '-')
write.csv(edges_marine2, "edges_marine_counts.csv")


## Edges - fw/terr, all

count.or_terrfw <- function(canada_pops_threatened_terrfw, n = 2) {
  or.sum <- function(cols) sum(rowSums(canada_pops_threatened_terrfw[cols]) > 1)
  counts <- combn(colnames(canada_pops_threatened_terrfw[,c(108:134)]), n, FUN = or.sum)
  names  <- combn(colnames(canada_pops_threatened_terrfw[,c(108:134)]), n, FUN = paste, collapse = "-")
  setNames(counts, names)
}

edges_terrfw <- stack(count.or_terrfw(canada_pops_threatened_terrfw, 2))[2:1]
edges_terrfw <- edges_terrfw[edges_terrfw$values != 0, ]
edges_terrfw2 <- tidyr::separate(edges_terrfw, ind, c('Threat 1', 'Threat 2'), sep = '-')
write.csv(edges_terrfw2, "edges_terrfw_counts.csv")


## Edges - marine, class
# Marine, fish
count.or_marine_fish <- function(canada_pops_threatened_marine_fish, n = 2) {
  or.sum <- function(cols) sum(rowSums(canada_pops_threatened_marine_fish[cols]) > 1)
  counts <- combn(colnames(canada_pops_threatened_marine_fish[,c(108:134)]), n, FUN = or.sum)
  names  <- combn(colnames(canada_pops_threatened_marine_fish[,c(108:134)]), n, FUN = paste, collapse = "-")
  setNames(counts, names)
}

edges_marine_fish <- stack(count.or_marine_fish(canada_pops_threatened_marine_fish, 2))[2:1]
edges_marine_fish <- edges_marine_fish[edges_marine_fish$values != 0, ]
edges_marine_fish2 <- tidyr::separate(edges_marine_fish, ind, c('Threat 1', 'Threat 2'), sep = '-')

# Marine, mammals
count.or_marine_mammals <- function(canada_pops_threatened_marine_mammals, n = 2) {
  or.sum <- function(cols) sum(rowSums(canada_pops_threatened_marine_mammals[cols]) > 1)
  counts <- combn(colnames(canada_pops_threatened_marine_mammals[,c(108:134)]), n, FUN = or.sum)
  names  <- combn(colnames(canada_pops_threatened_marine_mammals[,c(108:134)]), n, FUN = paste, collapse = "-")
  setNames(counts, names)
}

edges_marine_mammals <- stack(count.or_marine_mammals(canada_pops_threatened_marine_mammals, 2))[2:1]
edges_marine_mammals <- edges_marine_mammals[edges_marine_mammals$values != 0, ]
edges_marine_mammals2 <- tidyr::separate(edges_marine_mammals, ind, c('Threat 1', 'Threat 2'), sep = '-')

# Marine, herps
count.or_marine_herps <- function(canada_pops_threatened_marine_herps, n = 2) {
  or.sum <- function(cols) sum(rowSums(canada_pops_threatened_marine_herps[cols]) > 1)
  counts <- combn(colnames(canada_pops_threatened_marine_herps[,c(108:134)]), n, FUN = or.sum)
  names  <- combn(colnames(canada_pops_threatened_marine_herps[,c(108:134)]), n, FUN = paste, collapse = "-")
  setNames(counts, names)
}

edges_marine_herps <- stack(count.or_marine_herps(canada_pops_threatened_marine_herps, 2))[2:1]
edges_marine_herps <- edges_marine_herps[edges_marine_herps$values != 0, ]
edges_marine_herps2 <- tidyr::separate(edges_marine_herps, ind, c('Threat 1', 'Threat 2'), sep = '-')

# Marine, birds
count.or_marine_birds <- function(canada_pops_threatened_marine_birds, n = 2) {
  or.sum <- function(cols) sum(rowSums(canada_pops_threatened_marine_birds[cols]) > 1)
  counts <- combn(colnames(canada_pops_threatened_marine_birds[,c(108:134)]), n, FUN = or.sum)
  names  <- combn(colnames(canada_pops_threatened_marine_birds[,c(108:134)]), n, FUN = paste, collapse = "-")
  setNames(counts, names)
}

edges_marine_birds <- stack(count.or_birds(canada_pops_threatened_marine_birds, 2))[2:1]
edges_marine_birds <- edges_marine_birds[edges_marine_birds$values != 0, ]
edges_marine_birds2 <- tidyr::separate(edges_marine_birds, ind, c('Threat 1', 'Threat 2'), sep = '-')


edges_marine_class <- list(edges_marine_fish, edges_marine_herps, edges_marine_mammals, edges_marine_birds)
edges_marine_class  <- edges_marine_fish %>% full_join(edges_marine_herps,  by = "ind") %>% full_join(edges_marine_mammals,  by = "ind") %>% full_join(edges_marine_birds,  by = "ind")
edges_marine_class2 <- tidyr::separate(edges_marine_class, ind, c('Threat 1', 'Threat 2'), sep = '-')
colnames(edges_marine_class2) <- c("Threat1", "Threat2", "Fish", "Herps", "Mammals", "Birds")
write.csv(edges_marine_class2, "edges_marine_class_counts.csv")



## Edges - Fw/terr, class
# Terrfw, fish
count.or_terrfw_fish <- function(canada_pops_threatened_terrfw_fish, n = 2) {
  or.sum <- function(cols) sum(rowSums(canada_pops_threatened_terrfw_fish[cols]) > 1)
  counts <- combn(colnames(canada_pops_threatened_terrfw_fish[,c(108:134)]), n, FUN = or.sum)
  names  <- combn(colnames(canada_pops_threatened_terrfw_fish[,c(108:134)]), n, FUN = paste, collapse = "-")
  setNames(counts, names)
}

edges_terrfw_fish <- stack(count.or_terrfw_fish(canada_pops_threatened_terrfw_fish, 2))[2:1]
edges_terrfw_fish <- edges_terrfw_fish[edges_terrfw_fish$values != 0, ]
edges_terrfw_fish2 <- tidyr::separate(edges_terrfw_fish, ind, c('Threat 1', 'Threat 2'), sep = '-')

# Terrfw, mammals
count.or_terrfw_mammals <- function(canada_pops_threatened_terrfw_mammals, n = 2) {
  or.sum <- function(cols) sum(rowSums(canada_pops_threatened_terrfw_mammals[cols]) > 1)
  counts <- combn(colnames(canada_pops_threatened_terrfw_mammals[,c(108:134)]), n, FUN = or.sum)
  names  <- combn(colnames(canada_pops_threatened_terrfw_mammals[,c(108:134)]), n, FUN = paste, collapse = "-")
  setNames(counts, names)
}

edges_terrfw_mammals <- stack(count.or_terrfw_mammals(canada_pops_threatened_terrfw_mammals, 2))[2:1]
edges_terrfw_mammals <- edges_terrfw_mammals[edges_terrfw_mammals$values != 0, ]
edges_terrfw_mammals2 <- tidyr::separate(edges_terrfw_mammals, ind, c('Threat 1', 'Threat 2'), sep = '-')

# Terrfw, herps
count.or_terrfw_herps <- function(canada_pops_threatened_terrfw_herps, n = 2) {
  or.sum <- function(cols) sum(rowSums(canada_pops_threatened_terrfw_herps[cols]) > 1)
  counts <- combn(colnames(canada_pops_threatened_terrfw_herps[,c(108:134)]), n, FUN = or.sum)
  names  <- combn(colnames(canada_pops_threatened_terrfw_herps[,c(108:134)]), n, FUN = paste, collapse = "-")
  setNames(counts, names)
}

edges_terrfw_herps <- stack(count.or_terrfw_herps(canada_pops_threatened_terrfw_herps, 2))[2:1]
edges_terrfw_herps <- edges_terrfw_herps[edges_terrfw_herps$values != 0, ]
edges_terrfw_herps2 <- tidyr::separate(edges_terrfw_herps, ind, c('Threat 1', 'Threat 2'), sep = '-')

# Terrfw, birds
count.or_terrfw_birds <- function(canada_pops_threatened_terrfw_birds, n = 2) {
  or.sum <- function(cols) sum(rowSums(canada_pops_threatened_terrfw_birds[cols]) > 1)
  counts <- combn(colnames(canada_pops_threatened_terrfw_birds[,c(108:134)]), n, FUN = or.sum)
  names  <- combn(colnames(canada_pops_threatened_terrfw_birds[,c(108:134)]), n, FUN = paste, collapse = "-")
  setNames(counts, names)
}

edges_terrfw_birds <- stack(count.or_birds(canada_pops_threatened_terrfw_birds, 2))[2:1]
edges_terrfw_birds <- edges_terrfw_birds[edges_terrfw_birds$values != 0, ]
edges_terrfw_birds2 <- tidyr::separate(edges_terrfw_birds, ind, c('Threat 1', 'Threat 2'), sep = '-')


edges_terrfw_class <- list(edges_terrfw_fish, edges_terrfw_herps, edges_terrfw_mammals, edges_terrfw_birds)
edges_terrfw_class  <- edges_terrfw_fish %>% full_join(edges_terrfw_herps,  by = "ind") %>% full_join(edges_terrfw_mammals,  by = "ind") %>% full_join(edges_terrfw_birds,  by = "ind")
edges_terrfw_class2 <- tidyr::separate(edges_terrfw_class, ind, c('Threat 1', 'Threat 2'), sep = '-')
colnames(edges_terrfw_class2) <- c("Threat1", "Threat2", "Fish", "Herps", "Mammals", "Birds")
write.csv(edges_terrfw_class2, "edges_terrfw_class_counts.csv")



############### NOTE: Now run Canada threats networks_scaled ###############