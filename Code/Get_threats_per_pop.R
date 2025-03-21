# R code

# Load sf
library(sf)
library(ggplot2)
library(dplyr)

# Load threat shapefile (this has the sum_high - total number of high intensity threats - per cell)
threats <- read_sf("combined_threat_map_canada_100km2_20250314.shp")
plot(threats)
head(threats)

#ecozones <- sf::st_read("<file_path>/canada_ecozones.geojson")

# loc_df (assuming columns of "Longitude" and "Latitude" available, could/should include other population info too like ID, or loc_id)
#loc_df <- read.csv("FINAL_WWF_Canada_dataset_250724_2.csv")
loc_df <- read.csv("FINAL_WWF_Canada_dataset_140824_updated.csv")

nrow(loc_df)
loc_df <- loc_df %>% 
  dplyr::select(-c(index,sum_high)) # this is to remove the outcome of a previous extraction that is no longer valid


# plot points
my_sf <- st_as_sf(loc_df , coords = c('Longitude', 'Latitude'))
my_sf <- sf::st_set_crs(my_sf, sf::st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

#plot(threats['sum_high'])
#plot(my_sf['ID'], add= TRUE)

my_sf_proj <- sf::st_transform(my_sf, sf::st_crs(threats))

plot(my_sf_proj['ID'], add= TRUE)
#Plot it:
# ggplot(my_sf) + 
#   geom_sf()
#plot(my_sf)

# Join threat info to location info
loc_threat_sf <- sf::st_join(my_sf_proj,threats)
head(loc_threat_sf)
#plot(threats['sum_high'])
#plot(loc_threat_sf['sum_high'], add=TRUE)

# Can drop geometry to get a df back
loc_threat_df <- sf::st_drop_geometry(loc_threat_sf)
nrow(loc_threat_df)

# join lat and long back
loc_threat_df <- merge(loc_threat_df, loc_df[,c("ID", "Latitude","Longitude")])
sum(is.na(loc_threat_df$index))

test <- loc_threat_df[which(is.na(loc_threat_df$index)==TRUE),]
unique(test$ID)

# Write output...
# write.csv(loc_threat_df, "WWF_Canada_threat_df_20240726_withThreat_100km2.csv", row.names = FALSE)
write.csv(loc_threat_df, "WWF_Canada_threat_df_20250314_withThreat_100km2.csv", row.names = FALSE)

# test <- subset(loc_threat_df,is.na(sum_high))
# my_sf_test <- st_as_sf(loc_threat_df , coords = c('Longitude', 'Latitude'))
# my_sf_test <- sf::st_set_crs(loc_threat_df, sf::st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
