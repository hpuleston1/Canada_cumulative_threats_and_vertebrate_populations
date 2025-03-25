### Purpose of this script is to
### a) extract threat information from global or regional datasets
### b) reproject the layer and save it
### c) extract mean value of each layer in each cell of the grid and save results as csv

# ### Load required packages
library(terra) # replaces raster
library(sf) # replaces sp
library(ncdf4)
library(dplyr)
library(rworldmap)
library(tidytable)
library(raster)
library(qtl2)

### set working directory
setwd("Z:/GIS_PRODUCTION/USERS/Chris/Map_requests/SpatialThreats_ZSL")
getwd()

### Get desired projection
f2 <- "./Data/input_data/roads_aea.tif" # this is just a template dataset with the correct resolution, extent and projection
s2 <- rast(f2)
crs(s2)
plot(s2)
#desired_crs <- crs(s2)

### Processing threat layers, reproject and save reprojected layers

###Terrestrial

### CLIMATE
### Data downloaded and metadata available from https://adaptwest.databasin.org/pages/adaptwest-climatena/
### Climate normals MAT and MAP historical (1960-1990) and current (1991-2020) at 1km resolution

# Mean Annual Temperature
climate_mat <- "./Data/input_data/Normal_1961_1990_MAT.tif"
climate_mat <- rast(climate_mat)
crs(climate_mat)
plot(climate_mat)

climate_mat2 <- "./Data/input_data/Normal_1991_2020_MAT.tif"
climate_mat2 <- rast(climate_mat2)
crs(climate_mat2)
plot(climate_mat2)

clim_mat_dif <- abs(climate_mat2 - climate_mat)
plot(clim_mat_dif)

climate_mat_proj <- project(clim_mat_dif,s2)
plot(climate_mat_proj)
writeRaster(climate_mat_proj, filename="./Data/can_threat_layers_v2/Reprojected rasters/clim_diff_mat_proj.tif", overwrite=TRUE)

# Mean Annual Precipitation
climate_map <- "./Data/input_data/Normal_1961_1990_MAP.tif"
climate_map <- rast(climate_map)
crs(climate_map)
plot(climate_map)

climate_map2 <- "./Data/input_data/Normal_1991_2020_MAP.tif"
climate_map2 <- rast(climate_map2)
crs(climate_map2)
plot(climate_map2)

clim_map_dif <- abs(climate_map2 - climate_map)
plot(clim_map_dif)

climate_map_proj <- project(clim_map_dif,s2)
plot(climate_map_proj)
writeRaster(climate_map_proj, filename="./Data/can_threat_layers_v2/Reprojected rasters/clim_diff_map_proj.tif", overwrite=TRUE)

### CROPLAND (+PASTURE) AND URBAN COVER
# From http://www.cec.org/north-american-environmental-atlas/land-cover-30m-2020/
land_cover <- "./Data/threat_extraction/Rasters/Land cover/can_land_cover_2020_30m_tif/CAN_NALCMS_landcover_2020_30m/data/CAN_NALCMS_landcover_2020_30m.tif"
land_cover <- rast(land_cover)
head(land_cover)
nrow(land_cover)
plot(land_cover)
# only keep cropland/pastures and urban
#land_cover%>% filter(CAN_NALCMS_landcover_2020_30m == 15|CAN_NALCMS_landcover_2020_30m == 17)
land_cover_crop <- land_cover == 15
land_cover_urban <- land_cover == 17

plot(land_cover_crop)
plot(land_cover_urban)

crs(land_cover_crop)
land_cover_proj <- project(land_cover_crop,s2)
plot(land_cover_proj)
writeRaster(land_cover_proj, filename="./Data/can_threat_layers_v2/Reprojected rasters/land_cover_crop_proj.tif", overwrite=TRUE)

crs(land_cover_urban)
land_cover_proj_urb <- project(land_cover_urban,s2)
plot(land_cover_proj_urb)
writeRaster(land_cover_proj_urb, filename="./Data/can_threat_layers_v2/Reprojected rasters/land_cover_urban_proj.tif", overwrite=TRUE)

# intensity
lc_crop_intensity <- aggregate(land_cover_crop, fact = 10, fun = sum)
lc_crop_intensity_proj <- project(lc_crop_intensity,s2)
writeRaster(lc_crop_intensity_proj, filename="./Data/can_threat_layers_v2/Reprojected rasters/lc_crop_intensity_proj.tif", overwrite=TRUE)

lc_urb_intensity <- aggregate(land_cover_urban, fact = 10, fun = sum)
lc_urb_intensity_proj <- project(lc_urb_intensity,s2)
writeRaster(lc_urb_intensity_proj, filename="./Data/can_threat_layers_v2/Reprojected rasters/lc_urb_intensity_proj.tif", overwrite=TRUE)


### CATTLE DENSITY
# From https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/LHBICE
# Metadata in the 1_Ct_2015_Metadata.html file
cattle_density <- "./Data/threat_extraction/Rasters/Cattle density/6_Ct_2015_Aw.tif"
cattle_density<- rast(cattle_density)
crs(cattle_density)

cattle_density_rescaled <- cattle_density/10
plot(cattle_density_rescaled)

cattle_density_mask <- cattle_density_rescaled >= 2
cattle_density_masked <- cattle_density_rescaled*cattle_density_mask
plot(cattle_density_masked)

cattle_density_proj <- project(cattle_density_masked,s2)
crs(cattle_density_proj)
plot(cattle_density_proj)
writeRaster(cattle_density_proj, filename="./Data/can_threat_layers_v2/Reprojected rasters/cattle_density_proj.tif", overwrite=TRUE)

### FOREST LOSS
# From https://storage.googleapis.com/earthenginepartners-hansen/GFC-2023-v1.11/download.html
# converted to 0/1 for forest loss (1 = loss)
forest_loss <- "./Data/input_data/Hansen_GFC-2023-v.1.11_loss.tif"
forest_loss<- rast(forest_loss)
crs(forest_loss)

forest_loss_intensity <- aggregate(forest_loss, fact = 10, fun = sum)

forest_loss_proj <- project(forest_loss_intensity,s2)
plot(forest_loss_proj)
forest_loss_proj
writeRaster(forest_loss_proj, filename="./Data/can_threat_layers_v2/Reprojected rasters/forest_loss_intensity_proj.tif", overwrite=TRUE)

### POPULATION DENSITY
library(geodata)
human_pop <- population(2020, 0.5, path=tempdir())
#human_pop <- rast("./Data/threat_extraction/Rasters/Global population/gpw_v4_population_density_rev11_2pt5_min.nc")
plot(human_pop)
crs(human_pop)
human_pop_proj <- project(human_pop,s2)
plot(human_pop_proj)
writeRaster(human_pop_proj, filename="./Data/can_threat_layers_v2/Reprojected rasters/human_pop.tif", overwrite=TRUE)

### INVASIONS/ACCESSIBILITY

#The nine layers represent travel times to human settlements of different 
# population ranges. Two or more layers can be combined into one layer by 
# recording the minimum pixel value across the layers. For example, a map 
#of travel time to the nearest settlement of 5,000 to 50,000 people could 
# be generated by taking the minimum of the three layers that represent the 
# travel time to settlements with populations between 5,000 and 10,000, 10,000 
# and 20,000 and, 20,000 and 50,000 people.
#Travel time to cities
filenames = list.files('./Data/threat_extraction/Rasters/Access/Time to cities', full.names = TRUE, pattern = "*.tif") # directory
stack <- raster::stack(filenames)
plot(stack)
access <- min(stack)
plot(access)

crs(access)
access <- rast(access)
access_proj <- project(access,s2)
plot(access_proj)
writeRaster(access_proj, filename="./Data/can_threat_layers/Reprojected rasters/access_proj_R.tif", overwrite=TRUE)


# Travel time to ports
filenames_ports = list.files('./Data/threat_extraction/Rasters/Access/Time to ports', full.names = TRUE, pattern = "*.tif") # directory
#names(filenames) <- seq_along(filenames)
stack_ports <- raster::stack(filenames_ports)
terra::plot(stack_ports[[1]], colNA ="black")
access_ports <- min(stack_ports)
#plot(access_ports)

crs(access_ports)
access_ports <- rast(access_ports)
access_ports_proj <- project(access_ports,s2)
plot(access_ports_proj)
writeRaster(access_ports_proj, filename="./Data/can_threat_layers/Reprojected rasters/access_ports_proj_R.tif", overwrite=TRUE)

# # Combined
# filenames = list.files('./Rasters/Access/Time to cities', full.names = TRUE, pattern = "*.tif") # directory
# stack <- raster::stack(filenames)
# plot(stack)
# access <- min(stack)
# plot(access)
# 
# filenames_ports = list.files('./Rasters/Access/Time to ports', full.names = TRUE, pattern = "*.tif") # directory
# stack_ports <- raster::stack(filenames_ports)
# plot(stack_ports)
# access_ports <- min(stack_ports)
# plot(access_ports)
# 
# access_proj <- rast(acces_proj)
# access_ports_proj <- rast(access_ports_proj)
# r_list <- list(access, access_ports)
# r_c <- rast(r_list)
# stack_ports_cities <- raster::stack(r_c)
# plot(stack_ports_cities)
# access_all <- min(stack_ports_cities)
# plot(access_all, colNA = "black")
# writeRaster(access_all, filename="./Reprojected rasters/access_all_R.tif", overwrite=TRUE)


### FERTILIZER APPLICATION
# From https://data.tpdc.ac.cn/en/data/1e31ad1f-5904-4b49-8796-b379c37e7dba
# N_deposition <- "./Rasters/N deposition/global_N_deposition_maps_830/data/N-deposition1993.tif"
# N_deposition<- rast(N_deposition)
# crs(N_deposition)
# N_deposition_proj <- project(N_deposition,s2)
# plot(N_deposition_proj)
# writeRaster(N_deposition_proj, filename="./Reprojected rasters/N_deposition_proj.tif", overwrite=TRUE)

install.packages("BiocManager")
BiocManager::install("rhdf5")
library(rhdf5)
n_data_file = "./Data/input_data/Harvested_area_1961-2020.h5"
layer_info = h5ls(n_data_file)

# Testing individual crops...
# barley = h5read(n_data_file,"Barley")
# barley_r = rast(barley)
# plot(barley_r)
# barley_sum = app(barley_r, sum)
# plot(barley_sum)
# 
# cassava = h5read(n_data_file, "Cassava")
# cassava_r = rast(cassava)
# plot(cassava_r)
# cassava_sum = app(cassava_r, sum)
# plot(cassava_sum)
#
##plot(barley_r + cassava_r)
# plot(barley_sum + cassava_sum)

# Loop over crops in dataset, calculating total N applicaiton over all years, storing in list of rasters
layer_sum_list = list()
for (name in layer_info$name) {
  print(sprintf("Processing layer: %s", name))
  layer_data = h5read(n_data_file,name)
  layer_raster = rast(layer_data)
  layer_sum = app(layer_raster, sum, na.rm=TRUE)
  layer_sum_list[[name]] = layer_sum
}

# Then sum over all crop types
total_sum_rast = rast(layer_sum_list)
total_sum = app(total_sum_rast, sum, na.rm=TRUE)
plot(total_sum)

writeRaster(total_sum, filename="./Data/can_threat_layers/N_appliction_total_sum_unproj.tif", overwrite=TRUE)

# Load unprojected raster
total_sum = rast("Z:/GIS_PRODUCTION/USERS/Chris/Map_requests/SpatialThreats_ZSL/Data/can_threat_layers/N_appliction_total_sum_unproj.tif")
ext(total_sum) <- c(-180, 180, -90, 90)
crs(total_sum) <- "+proj=longlat +datum=WGS84 +no_defs +type=crs"
total_sum
plot(total_sum)

# Project into Canada crs
total_sum_proj <- project(total_sum,s2)
plot(total_sum_proj)

writeRaster(total_sum_proj, filename="./Data/can_threat_layers/Reprojected rasters/N_appliction_total_sum_proj.tif", overwrite=TRUE)


### PESTICIDES
# Load the total set of chemgrids pesticde application rates
chemgrids_files = list.files("./Data/threat_extraction/Rasters/Pesticides/ferman-v1-pest-chemgrids-v1-01-geotiff/ApplicationRate/GEOTIFF", full.names = TRUE, pattern = "*.tif")

# Get the information about each checmical from the filename
file_data = data.frame(filename = gsub(".tif", "", basename(chemgrids_files)))
file_data = tidyr::separate_wider_delim(file_data, filename, delim = "_", names = c("type", "crop", "chemical", "date", "quality"))
file_data$source = chemgrids_files

# Also load the area of each crop
crops_files = list.files("./Data/threat_extraction/Rasters/Pesticides/ferman-v1-pest-chemgrids-v1-01-geotiff/CROPS/GEOTIFF", full.names = TRUE, pattern = "*.tif")
crops_data = data.frame(filename = gsub(".tif", "", basename(crops_files)))
crops_data$crop = stringr::word(crops_data$filename, sep = "_", 1)
crops_data$area_source = crops_files

# Join these together - so that each row has chemical info and a link to the assocated area file
file_data = left_join(file_data, crops_data, by = "crop")

# subset for high-quality, 2015
file_data_2015_H = subset(file_data, quality == "H" & date == 2015)

# 
# f = file_data_2015_H$source
# r = rast(f)
# r = clamp(r, lower = 0, values = FALSE)
# plot(r)

# For each file, we need to mulitply the application rate by the area
chem_amount = list()
for (k in 1:nrow(file_data_2015_H)) {
  print(sprintf("[%d of %d] Processing %s, %s, %s", k, nrow(file_data_2015_H), file_data_2015_H$crop[k], file_data_2015_H$chemical[k], file_data_2015_H$date[k]))
  this_chem_layer = rast(file_data_2015_H$source[k])
  this_chem_layer = clamp(this_chem_layer, lower = 0, values = FALSE)
  this_area_layer = rast(file_data_2015_H$area_source[k])
  this_area_layer_res <- resample(this_area_layer, this_chem_layer)
  ext(this_area_layer_res) = ext(this_chem_layer)
  
  # This should be the total Kg of the chemical applied
  chem_amount[[k]] = (this_chem_layer*this_area_layer_res)
}

# Combine all 
all_chem_amount = rast(chem_amount)
# Add up in each cell
total_sum = app(all_chem_amount, sum, na.rm=TRUE)
plot(total_sum)
# Write unprojected to file
writeRaster(total_sum, filename="./Data/can_threat_layers/chemgrids_total_sum_unproj.tif", overwrite=TRUE)

# Load unprojected raster
total_sum = rast("./Data/can_threat_layers/chemgrids_total_sum_unproj.tif")

# Project into Canada crs
total_sum_proj <- project(total_sum,s2)
plot(total_sum_proj)

writeRaster(total_sum_proj, filename="./Data/can_threat_layers/Reprojected rasters/chemgrids_total_sum_proj.tif", overwrite=TRUE)

### OIL, GAS AND MINING from https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP2/EVKAVL
oil_gas <- "./Data/threat_extraction/Rasters/Oil gas mining/HF_oilgas_mining.tif" # This is already in the correct projection
oil_gas <- rast(oil_gas)
crs(oil_gas)
oil_gas_proj <- project(oil_gas,s2, method = 'near')
plot(oil_gas_proj)
writeRaster(oil_gas_proj, filename="./Data/can_threat_layers/Reprojected rasters/oil_gas_proj.tif", overwrite=TRUE)

### RAIL NETWORK from https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP2/EVKAVL
rail_network <- "./Data/threat_extraction/Rasters/Rail network/Rail.tif"
rail_network <- rast(rail_network)
crs(rail_network)
rail_network_proj <- project(rail_network,s2, method = 'near')
plot(rail_network_proj)
writeRaster(rail_network_proj, filename="./Data/can_threat_layers/Reprojected rasters/rail_network_proj.tif", overwrite=TRUE)

### ROAD NETWORK from Poley et al. 2022
road_network <- "./Data/input_data/Road_intensity_100m.tif"
road_network <- rast(road_network)
crs(road_network)
road_network_proj <- project(road_network,s2)
plot(road_network_proj)
crs(road_network_proj)
writeRaster(road_network_proj, filename="./Data/can_threat_layers_v2/Reprojected rasters/road_intensity_proj.tif", overwrite=TRUE)

### DAMS https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP2/EVKAVL
dams <- "./Data/threat_extraction/Rasters/Dams/dam_and_associated_reservoir.tif"
dams <- rast(dams)
crs(dams)
dams_proj <- project(dams,s2, method = 'near')
plot(dams_proj)
writeRaster(dams_proj, filename="./Data/can_threat_layers/Reprojected rasters/dams_proj.tif", overwrite=TRUE)


###Marine

###IMPACT ON OCEANS
### LIGHT POLLUTION
light <- "./Data/input_data/marine_human_pressure/data/night_lights_combo.tif"
light <- rast(light)
crs(light)
light_proj <- project(light,s2)
plot(light_proj)
writeRaster(light_proj, filename="./Data/can_threat_layers_v2/Reprojected rasters/marine_light_proj.tif", overwrite=TRUE)

### COASTAL POLLUTION
pollution_1 <- "./Data/input_data/marine_human_pressure/data/plumes_pest_combo.tif"
pollution_2 <- "./Data/input_data/marine_human_pressure/data/plumes_fert_combo.tif"
pollution_3 <- "./Data/input_data/marine_human_pressure/data/inorganic_combo.tif"

# scale before adding up
# a <- rast(pollution_1)
# a_test <- scale(a)
# plot(a_test)
# 
# r <- clamp(a, 0, 10)
# plot(a)
# 
# plot(r)
# plot(rast(pollution_1))
# plot(rast(pollution_2))
# plot(rast(pollution_3))

# reprojecting separately as the stack function complains about different extents
# reprojecting separately as the stack function complains about different extents
pollution_1 <- rast(pollution_1)
ext(pollution_1)
crs(pollution_1)
pollution_1_proj <- project(pollution_1,s2)
plot(pollution_1_proj)

pollution_2 <- rast(pollution_2)
ext(pollution_2)
crs(pollution_2)
pollution_2_proj <- project(pollution_2,s2)

pollution_3 <- rast(pollution_3)
ext(pollution_3)
crs(pollution_3)
pollution_3_proj <- project(pollution_3,s2)

#resample 3 to 1
pollution_1r <- resample(pollution_1, pollution_3)
pollution_2r <- resample(pollution_2, pollution_3)
stack <- rast(list(pollution_1r, pollution_2r, pollution_3))
coastal_pollution <- sum(stack, na.rm = TRUE)

plot(coastal_pollution)
coastal_pollution_proj <- project(coastal_pollution,s2)
plot(coastal_pollution_proj)
writeRaster(coastal_pollution_proj, filename="./Data/can_threat_layers_v2/Reprojected rasters/coastal_pollution.tif", overwrite=TRUE)

### to change plot as values are small and not easy to see on the map
plot(log10(coastal_pollution))
plot(log10(coastal_pollution_proj))

### FISHING STRESSORS
### COMMERCIAL FISHING
commercial_fishing_1 <- "./Data/input_data/marine_human_pressure/data/demersal_nondest_high_bycatch_combo.tif"
commercial_fishing_2 <- "./Data/input_data/marine_human_pressure/data/demersal_nondest_low_bycatch_combo.tif"
commercial_fishing_3 <- "./Data/input_data/marine_human_pressure/data/pelagic_low_bycatch_combo.tif"
commercial_fishing_4 <- "./Data/input_data/marine_human_pressure/data/pelagic_high_bycatch_combo.tif"

filenames = c(commercial_fishing_4, commercial_fishing_3, commercial_fishing_2, commercial_fishing_1) # directory
stack <- raster::stack(filenames)
plot(stack)
fishing <- sum(stack)
plot(fishing)

crs(fishing)
fishing <- rast(fishing)
fishing_proj <- project(fishing,s2)
plot(fishing_proj)
writeRaster(fishing_proj, filename="./Data/can_threat_layers_v2/Reprojected rasters/fishing_proj_R.tif", overwrite=TRUE)

### ARTISANAL FISHING
artisanal_fishing <- "./Data/input_data/marine_human_pressure/data/artisanal_fishing_combo.tif"
artisanal_fishing <- rast(artisanal_fishing)
crs(artisanal_fishing)
artisanal_fishing_proj <- project(artisanal_fishing,s2)
plot(artisanal_fishing_proj)
writeRaster(artisanal_fishing_proj, filename="./Data/can_threat_layers_v2/Reprojected rasters/artisanal_fishing_proj.tif", overwrite=TRUE)

### CLIMATE CHANGE STRESSORS

### OCEAN ACIDIFICATION
ocean_acidification <- "./Data/input_data/marine_human_pressure/data/ocean_acidification_combo.tif"
ocean_acidification <- rast(ocean_acidification)
crs(ocean_acidification)
ocean_acidification_proj <- project(ocean_acidification,s2)
plot(ocean_acidification_proj)
writeRaster(ocean_acidification_proj, filename="./Data/can_threat_layers_v2/Reprojected rasters/ocean_acidification_proj.tif", overwrite=TRUE)

### SURFACE TEMPERATURE ANOMALIES 
sst <- "./Data/input_data/marine_human_pressure/data/sst_combo.tif"
sst <- rast(sst)
crs(sst)
sst_proj <- project(sst,s2)
plot(sst_proj)
writeRaster(sst_proj, filename="./Data/can_threat_layers_v2/Reprojected rasters/sst_proj.tif", overwrite=TRUE)

### SEA LEVEL RISE 
slr <- "./Data/input_data/marine_human_pressure/data/slr_combo.tif"
slr <- rast(slr)
crs(slr)
slr_proj <- project(slr,s2)
plot(slr_proj)
writeRaster(slr_proj, filename="./Data/can_threat_layers_v2/Reprojected rasters/slr_proj.tif", overwrite=TRUE)

### UV ANOMALIES
uv <- "./Data/input_data/marine_human_pressure/data/uv_combo.tif"
uv <- rast(uv)
crs(uv)
uv_proj <- project(uv,s2)
plot(uv_proj)
writeRaster(uv_proj, filename="./Data/can_threat_layers_v2/Reprojected rasters/uv_proj.tif", overwrite=TRUE)


### OCEAN BASED STRESSORS
### OIL RIGS
oil_rigs <- "./Data/input_data/marine_human_pressure/data/oil_rigs_combo.tif"
oil_rigs <- rast(oil_rigs)
crs(oil_rigs)
oil_rigs_proj <- project(oil_rigs,s2)
plot(oil_rigs_proj)
writeRaster(oil_rigs_proj, filename="./Data/can_threat_layers_v2/Reprojected rasters/oil_rigs_proj.tif", overwrite=TRUE)

### SHIPPING POLLUTION
shipping <- "./Data/input_data/marine_human_pressure/data/shipping_combo.tif"
shipping <- rast(shipping)
crs(shipping)
shipping_proj <- project(shipping,s2)
plot(shipping_proj)
writeRaster(shipping_proj, filename="./Data/can_threat_layers_v2/Reprojected rasters/shipping_proj.tif", overwrite=TRUE)

### INVASIONS
invasives <- "./Data/input_data/marine_human_pressure/data/invasives_combo.tif"
invasives <- rast(invasives)
crs(invasives)
invasives_proj <- project(invasives,s2)
plot(invasives_proj)
writeRaster(invasives_proj, filename="./Data/can_threat_layers_v2/Reprojected rasters/invasives_proj.tif", overwrite=TRUE)

### OCEAN POLLUTION
ocean_pollution <- "./Data/input_data/marine_human_pressure/data/ocean_pollution_combo.tif"
ocean_pollution <- rast(ocean_pollution)
crs(ocean_pollution)
ocean_pollution_proj <- project(ocean_pollution,s2)
plot(ocean_pollution_proj)
writeRaster(ocean_pollution_proj, filename="./Data/can_threat_layers_v2/Reprojected rasters/ocean_pollution_proj.tif", overwrite=TRUE)





### PLUMES PEST https://knb.ecoinformatics.org/view/doi:10.5063/F1S180FS
# plumes_pest <- "./Data/threat_extraction/Rasters/Coastal marine cumulative impact/plumes_pest.tif"
# plumes_pest <- rast(plumes_pest)
# crs(plumes_pest)
# plumes_pest_proj <- project(plumes_pest,s2)
# plot(plumes_pest_proj)
# writeRaster(plumes_pest_proj, filename="./Data/can_threat_layers/Reprojected rasters/plumes_pest_proj.tif", overwrite=TRUE)
# 

### PLUMES PEST https://knb.ecoinformatics.org/view/doi:10.5063/F1S180FS
# plumes_pest <- "./Data/threat_extraction/Rasters/Coastal marine cumulative impact/plumes_pest.tif"
# plumes_pest <- rast(plumes_pest)
# crs(plumes_pest)
# plumes_pest_proj <- project(plumes_pest,s2)
# plot(plumes_pest_proj)
# writeRaster(plumes_pest_proj, filename="./Data/can_threat_layers/Reprojected rasters/plumes_pest_proj.tif", overwrite=TRUE)

### COASTAL/MARINE POLLUTION AND CUMULATIVE IMPACT
# marine_impact <- "./Data/threat_extraction/Rasters/Coastal marine cumulative impact/global_cumul_impact_2013_all_layers.tif"
# marine_impact <- rast(marine_impact)
# crs(marine_impact)
# marine_impact_proj <- project(marine_impact,s2)
# plot(marine_impact_proj)
# writeRaster(marine_impact_proj, filename="./Data/can_threat_layers/Reprojected rasters/marine_impact_proj.tif", overwrite=TRUE)
