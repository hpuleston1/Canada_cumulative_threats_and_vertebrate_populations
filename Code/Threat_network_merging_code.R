#load packages
library(dplyr)
library(tidyr)
library(sf)

#NOTE I changed the ID column name in threat network data to index manually in excel before uploading the folders
#If needing to change in r, use ThreatNetworkData_2 <- ThreatNetworkData %>% rename(index=ID)

#load in threatnetworkdata and wwf_canada with index datasets
ThreatNetworkData_111024<-read.csv("threat_grids_long_with_thresh_theat_20250314.csv")
head(ThreatNetworkData_111024)
nrow(ThreatNetworkData_111024)
length(unique(ThreatNetworkData_111024$index))
#ThreatNetworkData_2 <- ThreatNetworkData %>% rename(index=ID)
ThreatNetworkData_111024_2<- ThreatNetworkData_111024 %>% filter(is_high==TRUE)
length(unique(ThreatNetworkData_111024_2$index))
length(unique(ThreatNetworkData_111024_2$basename))

WWF_Canada_dataset_20241011<-read.csv("WWF_Canada_threat_df_20250314_withThreat_100km2.csv")

head(WWF_Canada_dataset_20241011)
nrow(WWF_Canada_dataset_20241011)
select_WWF_Canada_dataset_20241011<-WWF_Canada_dataset_20241011%>% select(ID, index, Latitude, Longitude, sum_hgh, sm_qntl,hmn_pp_v, hmn_pp_q)

length(unique(select_WWF_Canada_dataset_20241011$index))

# plot points
my_sf <- st_as_sf(select_WWF_Canada_dataset_20241011 , coords = c('Longitude', 'Latitude'))
my_sf <- sf::st_set_crs(my_sf, sf::st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

plot(my_sf['ID'])

# my_sf_proj <- sf::st_transform(my_sf, sf::st_crs(threats))
# 
# test <- plot(my_sf_proj['ID'])
# 
# ggplot(my_sf_proj) + 
  #   geom_sf()


#Merge by index (longform - should be a many to many match warning, but thats okay in this case?)
#merged_longform_threat_network_dataset<-left_join(select_WWF_Canada_dataset_080724, ThreatNetworkData, by = "index")
#write.csv(merged_longform_threat_network_dataset, "Merged_threat_network_longform_dataset_080724.csv")

#Merge by index (wideform - need to make threat network data wideform first, although would be omitting several columns in that dataset in the process)
select_ThreatNetworkData<-ThreatNetworkData_111024_2 %>% select(index, basename)
# 
# write.csv(select_ThreatNetworkData, "test_1.csv")
# #check for duplicates
# select_ThreatNetworkData_test <- select_ThreatNetworkData %>%
#   distinct() %>%
#   group_by(basename, index) %>%
#   count()
# 
# test <- select_ThreatNetworkData_test[which(select_ThreatNetworkData_test$n ==2),]
# write.csv(select_ThreatNetworkData_test,"test.csv")

wideform_ThreatNetworkData<- select_ThreatNetworkData %>%
  # distinct() %>%
  mutate(n = 1) %>%
  pivot_wider(names_from = basename, values_from = n, values_fill = 0)

head(wideform_ThreatNetworkData)
colnames(wideform_ThreatNetworkData)

merged_wideform_threat_network_dataset<-left_join(select_WWF_Canada_dataset_20241011, wideform_ThreatNetworkData, by = "index")
nrow(merged_wideform_threat_network_dataset)

#Make any that returned NA in basename columns into 0
merged_wideform_threat_network_dataset_2 <- merged_wideform_threat_network_dataset %>% mutate_at(c(9:35), ~replace_na(.,0))

merged_wideform_threat_network_dataset_2$terr_threat <- rowSums(merged_wideform_threat_network_dataset_2[ , c(9,10,12:15,17,19,20,22,23,25,28,30,31)], na.rm=TRUE)
merged_wideform_threat_network_dataset_2$marine_threat <- rowSums(merged_wideform_threat_network_dataset_2[ , c(11,16,18,21,24,26,27,29,32:35)], na.rm=TRUE)

table(merged_wideform_threat_network_dataset_2$terr_threat, merged_wideform_threat_network_dataset_2$marine_threat)

merged_wideform_threat_network_dataset_2_test <- 
  merged_wideform_threat_network_dataset_2 %>% 
  filter(
    (marine_threat > 0) & (terr_threat > 0)
  )

write.csv(merged_wideform_threat_network_dataset_2, "Merged_threat_network_wideform_dataset_20250314.csv")

# 
# 
# my_sf <- st_as_sf(merged_wideform_threat_network_dataset_2 , coords = c('Longitude', 'Latitude'))
# my_sf <- sf::st_set_crs(my_sf, sf::st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
# 
# plot(my_sf['ID'])
