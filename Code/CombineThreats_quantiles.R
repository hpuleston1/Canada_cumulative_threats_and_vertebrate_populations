###
### Calculate those cells in top quantiles of different threats and aggregate
###

# ### Load required packages
library(terra) # replaces raster
library(sf) # replaces sp
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(data.table)
library(exactextractr)
library(readr)
library(viridis)

### set working directory
#setwd("Z:/GIS_PRODUCTION/USERS/Chris/Map_requests/SpatialThreats_ZSL")
#setwd("../canada_threat_extraction")
getwd()

###########

GRID_RES <- 100 # in km2

#############################################################################################

# Get reference crs from one of the layers
f2 <- "./Data/input_data/roads_aea.tif"
s2 <- rast(f2)
crs(s2)


###check light pollution layer
# Get reference crs from one of the layers
light_pollution <- "./Data/can_threat_layers/Reprojected rasters/ocean_pollution_proj.tif"
light_pollution <- rast(light_pollution)
crs(light_pollution)
plot(light_pollution)

###check oil rigs layer
oil_rigs <- "./Data/can_threat_layers/Reprojected rasters/oil_rigs_proj.tif"
oil_rigs <- rast(oil_rigs)
crs(oil_rigs)
plot(oil_rigs)

min_val <- min(oil_rigs[], na.rm = T)
max_val <- max(oil_rigs[], na.rm = T)
normalize_ras <- (oil_rigs - min_val)/(max_val - min_val)

oil_gas <- "./Data/can_threat_layers/Reprojected rasters/oil_gas_proj.tif"
oil_gas <- rast(oil_gas)
crs(oil_gas)
plot(oil_gas)

####
# Create grid...
####

# Get EEZ data
eez <- st_read("./Data/input_data/eez_canada_v12_simp_aea.shp")

# We want to create hexagonal cells with a defined area, so work out spacing
cell_area <- units::as_units(GRID_RES, "km^2") # target grid size
grid_spacing <- sqrt(2 * cell_area / sqrt(3)) # size of hexagon calculated from area


# Create hexagonal grid inside hull
can_hex_grid_p <- st_make_grid(
  eez,
  # cellsize = 1000, #100km
  cellsize = grid_spacing,
  # n = c(50, 50), # grid granularity
  # crs = st_crs(eez),
  what = "polygons",
  # square = TRUE # square!
  square = FALSE # hex!
)
# Save the hex grid at the defined resolution as R object
saveRDS(can_hex_grid_p, paste0("can_hex_grid_p_", GRID_RES, "km2_area.rds"))
can_hex_grid_p <- readRDS(paste0("can_hex_grid_p_", GRID_RES, "km2_area.rds")) # 1km

can_ext <- ext(st_bbox(can_hex_grid_p))
# plot(can_hex_grid_p)

can_hex_grid_eez <- can_hex_grid_p[eez]
# plot(can_hex_grid_eez)

# Convert multiploygon to set of polygons
can_hex_grid_p_st <- st_sf(index = 1:length(lengths(can_hex_grid_eez)), can_hex_grid_eez)

# Write out shapefile of grid as defined resolution
st_write(can_hex_grid_p_st, paste0("can_hex_grid_p_st_", GRID_RES, "km2.shp")) # 1km
can_hex_grid_p_st_threat <- st_read(paste0("./can_hex_grid_p_st_", GRID_RES, "km2.shp")) # 100km

cells_to_process <- can_hex_grid_p_st_threat
# plot(cells_to_process)

# Quick plot to show hex grid
ggplot() +
  geom_sf(data = cells_to_process) # +
# geom_sf(data = canada_outline_proj, alpha = 0.4)

# Extract mean value of each layer in each cell of the grid (as the layers have different resolution)
# Create function
get_threatened_cells_poly <- function(r, grid) {
  # extracted_vals = terra::extract(r, grid, fun = mean, na.rm=TRUE)
  extracted_vals <- exactextractr::exact_extract(r, grid, fun = "mean", progress = TRUE, force_df = TRUE)
  extracted_vals$index <- grid$index
  return(extracted_vals)
}

# Loop over reprojected files for given resolution creating a CSV for
# each raster where the extract value for that cell is stored with the cell ID
filenames <- list.files("./Data/can_threat_layers/Reprojected rasters", full.names = TRUE, pattern = "*.tif") # directory

#f <- "./Data/can_threat_layers/Reprojected rasters/slr_proj.tif"

for (f in filenames) {
  start.time <- Sys.time()
  out.file <- paste0("./Data/can_threat_layers/Output rasters/", GRID_RES, "km2_Area/", gsub(".tif", "_threat_grid.csv", basename(f)))
  print(f)
  s <- rast(f)
  threatened_cells <- get_threatened_cells_poly(s, cells_to_process)
  write.csv(threatened_cells, out.file)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  cat("Time taken", time.taken)
}

# raster01 = function(r){
#   # get the min max values
#   minmax_r = range(values(r), na.rm=TRUE)
#   # rescale
#   return( (r-minmax_r[1]) / (diff(minmax_r)))
# }
# for (f in filenames[1]) {
#   start.time <- Sys.time()
#   #out.file = paste0("./Data/can_threat_layers/Output rasters/", GRID_RES, "km2_Area/",gsub(".tif","_threat_grid.csv", basename(f)))
#   print(f)
#   s = rast(f)
#   sr = raster01(s)
#   plot(sr)
# }

# This might be much faster with just a 1km raster for each threat (but would need to mask cells)?
# s_r = resample(s, r_temp)
# s_rc = crop(s_r, can_hex_grid_p_st_threat)
# plot(s_rc)

###
# Load exported CSVs, work out threasholds and find cells above threshold
# ***Need to work out which layers need to use low rather then hig****
###

# Calculate cells with values above the 90% quantile (or reverse if lower values mean more threat)
# Read in output files
threat_files <- list.files(paste0("./Data/can_threat_layers/Output rasters/", GRID_RES, "km2_Area/"), full.names = TRUE, pattern = "*.csv")
# threat_files = list.files('./output_rasters_coarse', full.names = TRUE, pattern = "*.csv")

threat_grids <- list()
for (f in threat_files) {
  threat_data <- fread(f)
  threat_grids[[f]] <- threat_data
}

threat_grids <- rbindlist(threat_grids, idcol = "filename", use.names = FALSE)
threat_grids$basename <- basename(threat_grids$filename)

# check
# colnames(threat_grids) = c("filename", "V1", "ID", "value", "basename")
colnames(threat_grids) <- c("filename", "V1", "value", "index", "basename")

##
# Inversion of some layers
##
# Invert those layers that need inverted (i.e. access value we want should be high when closest, but layer is distance/travel time to places)
# threat_grids$access = threat_grids$access*-1
threat_grids$value[threat_grids$basename == "access_proj_R_threat_grid.csv"] <-
  threat_grids$value[threat_grids$basename == "access_proj_R_threat_grid.csv"] * -1
threat_grids$value[threat_grids$basename == "access_ports_proj_R_threat_grid.csv"] <-
  threat_grids$value[threat_grids$basename == "access_ports_proj_R_threat_grid.csv"] * -1

# Set colnames to somethings easier to undestand
# colnames(threat_grids) = c("V1", "ID", "access", "cattle_density", "pesticides", "climate_map", "climate_mat", "forest_loss", "oilgas", "N_application")


# Pivot to long form
# threat_grids_long <- as.data.frame(t(threat_grids))
threat_grids_long <- threat_grids %>%
  dplyr::select(-c(filename, V1)) # %>%  # This varies with extraction function above 'exactextractr' vs. terra::extract as an extra column is added
# mutate(across(c(basename, value), as.character)) %>%
# tidyr::pivot_longer(cols = -c(index,basename))

### *new*
# Work out the terrestrial area of cells and add that to the threat grid data
###

##### HANNAH AND VALE ADDED THIS BIT HERE FROM LOWER DOWN IN THE CODE AS WE NEEDED THE can_prov_proj FILE
# # Get province data and project
can_prov <- st_read("./province_indiv_Dissolve.shp")
plot(can_prov)
can_prov_proj <- st_transform(can_prov, crs = crs(s2))
#can_hex_grid_p_st_threat_proj <- st_transform(can_hex_grid_p_st_threat, crs = crs(can_prov))
#eez_proj <- st_transform(eez, crs = crs(can_prov))


# Estimate the spatial intersection of the hex grid and the terrestrial outline
start.time <- Sys.time()
intersect_pct <- st_intersection(can_hex_grid_p_st_threat, can_prov_proj) %>% 
  mutate(terr_area_km2 = units::drop_units(st_area(.)) / 1e6) %>%
  group_by(index) %>% 
  summarise(terr_area_km2 = sum(terr_area_km2)) # Need to sum over all intersecting polygons!!
end.time <- Sys.time()
time.taken <- end.time - start.time
  
# Get a table of the intersection area
intersect_tab <- intersect_pct %>% # create new column with shape area
  dplyr::select(index, terr_area_km2) %>% # only select columns needed to merge
  st_drop_geometry()

threat_grids_long_with_terr_area <- left_join(threat_grids_long, intersect_tab, by = "index")

### *new*
# Also add the 'threat system' associated with each threat...
###

###### NEW 04/03/2025 assigned M/T threats to either Marine or Terrestrial
threat_names <- data.frame(basename = unique(threat_grids_long_with_terr_area$basename))

threat_names$threat_system <- "terrestrial"
threat_names$threat_system[threat_names$basename %in% c(
  "artisanal_fishing_proj_threat_grid.csv",
  "coastal_pollution_threat_grid.csv",
  "fishing_proj_R_threat_grid.csv",
  "invasives_proj_threat_grid.csv",
  "ocean_acidification_proj_threat_grid.csv",
  "ocean_pollution_proj_threat_grid.csv",
  "sst_proj_threat_grid.csv",
  "oil_rigs_proj_threat_grid.csv",
  "uv_proj_threat_grid.csv",
  "marine_light_proj_threat_grid.csv",
  "shipping_proj_threat_grid.csv",
  "slr_proj_threat_grid.csv"
)] <- "marine"

# threat_names$threat_system[threat_names$basename %in% c(
#     "slr_proj_threat_grid.csv"
# )] <- "both"

threat_grids_long_with_terr_area <- left_join(threat_grids_long_with_terr_area, threat_names, by = "basename")

###### NEW 04/03/2025 assigned cells affected by sea level rise to to either Marine or Terrestrial depending on proportion of land
###### so we have sea level rise as both a marine and a terrestrial threat separately
###### replaces lines 310-314 (filtering out non-terrestrial cells for sea level rise)
# threat_grids_long_with_terr_area <- threat_grids_long_with_terr_area %>% 
#   mutate(threat_system = case_when((terr_area_km2 < 1 | is.na(terr_area_km2) == TRUE) ~ 'marine'),
#          threat_system = case_when(terr_area_km2 >= 1 ~ 'terrestrial'))
# 
# library(rescale)
# threat_grids_long_with_terr_area$percent_area <- rescale(threat_grids_long_with_terr_area$terr_area_km2, to = c(0,100))

# threat_grids_long_with_terr_area <- threat_grids_long_with_terr_area %>% 
#   mutate(threat_system = case_when(percent_area < 1 ~ 'marine'))


### *new*
# Filtering out terrestrial cells from marine threats and vice versa
###
# Filter to keep cells where the threat_system is 'both' (e.g. the threat covers land and sea)
# OR keep terrestrial threats (if the terrestrial area is > 1%)
# OR keep marine threats in marine, as long as the terrestrial area is < 99%)
# OR keep marine threats with no calculated terrestrial area (as there was no land)
#
# threat_grids_long_filtered <- threat_grids_long_with_terr_area %>%
#   filter((threat_system == "terrestrial" & terr_area_km2 > 1) |
#     (threat_system == "marine" & terr_area_km2 < 99) |
#     (threat_system == "marine" & is.na(terr_area_km2)))

threat_grids_long_filtered <- threat_grids_long_with_terr_area %>%
  filter((threat_system == "terrestrial" & terr_area_km2 > 1) |
           (threat_system == "marine" & terr_area_km2 < 1) |
           (threat_system == "marine" & is.na(terr_area_km2)))
# 
# ### Visualising sea-level rise
# temp = filter(threat_grids_long_filtered, basename == "slr_proj_threat_grid.csv")
# temp_cells <- left_join(cells_to_process, temp, join_by(index == index))
# temp_cells_proj <- st_transform(temp_cells, crs = crs(can_prov))
# temp_cells_proj = filter(temp_cells_proj, !is.na(value) & (value > 0) & (terr_area_km2 > 1))
# ggplot() +
#   geom_sf(data = temp_cells_proj, aes(fill = value), color = NA) +
#   geom_sf(data = filter(temp_cells_proj, (value > quantile(temp_cells_proj$value, na.rm=TRUE, probs = 0.9))), aes(fill = value), color = "red") +
#   geom_sf(data = can_prov, fill = NA) + 
#   theme_minimal() +
#   labs("Title Sea-level rise in terrestrial areas", 
#        subtitle = "Sea-level rise > 0mm, only over terrestrial cells, top 10% highlighted")+
#   scale_fill_stepsn(
#     breaks = as.numeric(quantile(temp_cells_proj$value, probs = seq(0, 1, 0.1))),
#     labels = quantile(temp_cells_proj$value, probs = seq(0, 1, 0.1)),
#     name = "Threat Intensity",
#     colours = viridis(11)
#   ) 
# ggsave("sea_level_riseA.png")
# 
# temp = filter(threat_grids_long_filtered, basename == "slr_proj_threat_grid.csv")
# temp_cells <- left_join(cells_to_process, temp, join_by(index == index))
# temp_cells_proj <- st_transform(temp_cells, crs = crs(can_prov))
# temp_cells_proj = filter(temp_cells_proj, !is.na(value) & (value > 0))
# ggplot() +
#   geom_sf(data = temp_cells_proj, aes(fill = value), color = NA) +
#   geom_sf(data = filter(temp_cells_proj, !is.na(value) & (value > 0)), aes(fill = value), color = NA) +
#   geom_sf(data = filter(temp_cells_proj, (value > quantile(temp_cells_proj$value, na.rm=TRUE, probs = 0.9))), aes(fill = value), color = "red") +
#   geom_sf(data = can_prov, fill = NA) + 
#   theme_minimal() +
#   labs("Title Sea-level rise in terrestrial areas", 
#        subtitle = "Sea-level rise > 0mm, top 10% highlighted") + 
#   scale_fill_stepsn(
#     breaks = as.numeric(quantile(temp_cells_proj$value, probs = seq(0, 1, 0.1))),
#     labels = quantile(temp_cells_proj$value, probs = seq(0, 1, 0.1)),
#     name = "Threat Intensity",
#     colours = viridis(11)
#   ) 
# ggsave("sea_level_rise_valueGT0.png")
# 
# temp = filter(threat_grids_long_filtered, basename == "slr_proj_threat_grid.csv")
# temp_cells <- left_join(cells_to_process, temp, join_by(index == index))
# temp_cells_proj <- st_transform(temp_cells, crs = crs(can_prov))
# ggplot() +
#   geom_sf(data = temp_cells_proj, aes(fill = value), color = NA) +
#   #geom_sf(data = filter(temp_cells_proj, !is.na(value) & (value > 0)), aes(fill = value), color = NA) +
#   geom_sf(data = filter(temp_cells_proj, !is.na(value) & (value > quantile(temp_cells_proj$value, na.rm=TRUE, probs = 0.9))), aes(fill = value), color = "red") +
#   geom_sf(data = can_prov, fill = NA) + 
#   theme_minimal() +
#   labs("Title Sea-level rise in terrestrial areas", 
#        subtitle = "Sea-level rise, top 10% highlighted") + 
#   scale_fill_stepsn(
#     breaks = as.numeric(quantile(temp_cells_proj$value, probs = seq(0, 1, 0.1))),
#     labels = quantile(temp_cells_proj$value, probs = seq(0, 1, 0.1)),
#     name = "Threat Intensity",
#     colours = viridis(11)
#   ) 
# ggsave("sea_level_rise_valueGT.png")

# For the SLR threat, filter out those non-terrestrial cells,
# threat_grids_long_filtered = threat_grids_long_filtered %>%
#   group_by(basename) %>%
#   filter((!is.na(value) & (value > 0) & (terr_area_km2 > 1)) | cur_group()$basename != "slr_proj_threat_grid.csv") %>%
#   ungroup

# Check terrestrial area for each threat
# View(threat_grids_long_filtered %>% group_by(basename) %>%
#   summarise(terr_area = mean(terr_area_km2, na.rm = T)))

# Determine the high and low thresholds for each raster type (overall quantiles across all cells for each basename)
## As we've filtered the cells, this should only calculate them for retainted cells
thresh <- threat_grids_long_filtered %>%
  dplyr::group_by(basename) %>%
  dplyr::summarise(
    q_low = stats::quantile(value, prob = c(0.1), na.rm = TRUE),
    q_high = stats::quantile(value, prob = c(0.9), na.rm = TRUE),
    max_val = max(abs(value), na.rm = TRUE)
  )

##### *new* #####
### still needs to be edited
# code to add quantile 0-100 for each threat column
# add this line with the ecdf function to get the percentile of each value, for each threat column
# thresh$[threat1]_perc <- ecdf(thresh$[threat1])(thresh$[threat1])

thresh2 <- threat_grids_long_filtered %>%
  dplyr::group_by(basename) %>%
  dplyr::mutate(
    quantile= ntile(value, 100)
  )


### *new*
# I've removed this and instead add +1 to the normalised values below
# This means that a threat that starts as [0 - 10000] (e.g. distance to cities), becomes
# [-10000 - 0] when inverted, then [-1 - 0] when normalised, then [0 - 1] when +1 is added.
# 
# I think this prevents an issue that if we normalise by the negative max_val, we get the 
# threat re-inverted, so that 0 is 'high' and 1 is 'low'
###
# thresh$max_val[thresh$basename == "access_proj_R_threat_grid.csv"] =
#   thresh$max_val[thresh$basename == "access_proj_R_threat_grid.csv"]*-1
# thresh$max_val[thresh$basename == "access_ports_proj_R_threat_grid.csv"] =
#   thresh$max_val[thresh$basename == "access_ports_proj_R_threat_grid.csv"]*-1

# Join thresholds back onto original data frame
#threat_grids_long_with_thresh <- left_join(threat_grids_long_filtered, thresh, by = "basename")
threat_grids_long_with_thresh <- left_join(thresh2, thresh, by = "basename")

### *new*
# This now does > or < rather then <= or >=
###
# For each cell, for each layer work out if it exceeds threshold
threat_grids_long_with_thresh_theat <- threat_grids_long_with_thresh %>% # group_by(ID, basename) %>%
  mutate(
    is_high = value > q_high, ### Changed from >= to >
    is_low = value < q_low, ### Changed from <= to <
    norm_val = value / max_val
  )

# Add one to inverted threats to get them into 0-1 range
threat_grids_long_with_thresh_theat$norm_val[threat_grids_long_with_thresh_theat$basename == "access_proj_R_threat_grid.csv"] <-
  threat_grids_long_with_thresh_theat$norm_val[threat_grids_long_with_thresh_theat$basename == "access_proj_R_threat_grid.csv"] + 1
threat_grids_long_with_thresh_theat$norm_val[threat_grids_long_with_thresh_theat$basename == "access_ports_proj_R_threat_grid.csv"] <-
  threat_grids_long_with_thresh_theat$norm_val[threat_grids_long_with_thresh_theat$basename == "access_ports_proj_R_threat_grid.csv"] + 1

head(as.data.frame(threat_grids_long_with_thresh_theat))

write_csv(threat_grids_long_with_thresh_theat, "threat_grids_long_with_thresh_theat_20250314.csv")

##### return to wide format to plot individual threats
threat_grids_wide <- select(threat_grids_long_with_thresh_theat, norm_val, basename, index, terr_area_km2, quantile) %>%
  tidyr::pivot_wider(names_from = basename, values_from = norm_val)
# join back to hex grid
can_hex_grid_p_st_threat_ind <- left_join(cells_to_process, threat_grids_wide, join_by(index == index))
can_hex_grid_p_st_threat_ind_proj <- st_transform(can_hex_grid_p_st_threat_ind, crs = crs(can_prov))

######

### *new*
# RF: Calculating this separately for marine and terrestrial threats
###
# continue working with cumulative threats
# Sum those cells exceeding high threshold

smry <- threat_grids_long_with_thresh_theat %>%
  dplyr::group_by(index, threat_system) %>%
  summarise(sum_threat = sum(is_high, na.rm = TRUE), sum_quantile = sum(quantile, na.rm = TRUE))

###
# Trying to find duplicated rows....
###
# smry %>% ungroup() %>% group_by(threat_system) %>%
#   slice_max(n = 5, order_by = sum_threat, with_ties = FALSE)
# 
# temp = smry %>% ungroup() %>% group_by(index) %>%
#   summarise(sum_threat = sum(sum_threat, na.rm = TRUE)) %>%
#   slice_max(n = 5, order_by = sum_threat, with_ties = FALSE)
# 
# high_cells = smry %>% ungroup() %>% group_by(index) %>%
#   summarise(sum_threat = sum(sum_threat, na.rm = TRUE)) %>%
#   filter(temp, sum_threat == 28)
# 
# View(filter(threat_grids_long_with_thresh_theat, index %in% high_cells$index))
# 
# View(filter(threat_grids_wide, index %in% high_cells$index))
# 
# View(filter(threat_grids_long_with_thresh_theat, index %in% high_cells$index))
# 
# View(filter(threat_grids_long_with_terr_area, index %in% high_cells$index))
# 
# View(filter(threat_grids_long,  index %in% high_cells$index))
# 
# # In here, two rows...with same index?????
# # 128971
# # 128971
# # 21.534375
# # 128971.1
# # 128971
# # 65.319042
# View(filter(intersect_tab, index %in% high_cells$index))
# 
# # It's here! intersecting with mutliple polygons!!
# View(filter(intersect_pct, index %in% high_cells$index))

# high_threats = threat_grids_long_with_thresh_theat %>% group_by(ID) %>%
#   filter(is_high)

# Join back to hex grid
# names(can_hex_grid_p_st) <- c("index", "can_hex_grid_p")
# can_hex_grid_p_st_threat = merge(can_hex_grid_p_st, smry, by.x = "index", by.y = "ID")
# cells_to_process_threat = merge(cells_to_process, smry, by.x = "index", by.y = "ID")
can_hex_grid_p_st_threat_system <- left_join(cells_to_process, smry, join_by(index == index))

### *new*
# Plot threat richness within each system
###
ggplot() +
  geom_sf(data = can_hex_grid_p_st_threat_system, aes(fill = sum_threat), color = NA) +
  geom_sf(data = eez, color = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.2, fill = NA) +
  geom_sf(data = can_prov, color = "black", linewidth = 0.1, alpha = 0.2, fill = NA) +
  scale_fill_gradientn(
    colours = rev(viridis(6, begin = 0, end = 0.85, option = "D")),
    name = "Threat Richness",
    na.value = "grey100"
  ) +
  theme(legend.position = c(0.85, 0.8)) +
  theme(legend.background = element_rect(fill = NA), legend.position = "bottom") +
  labs(title = "Threat Richness", face = "bold", wjust = 0.5) +
  facet_wrap(~threat_system)

ggsave(filename = paste0("can_threat_", GRID_RES, "_km2_lambert_20250314_SYSTEM_slr_fix.png"), width = 12, height = 8)
ggsave(filename = paste0("can_threat_", GRID_RES, "km2_lambert_20250314_SYSTEM_slr_fix.pdf"), width = 12, height = 8)

##same plot but with the new threat intensity
# Plot threat richness within each system
###
ggplot() +
  geom_sf(data = can_hex_grid_p_st_threat_system, aes(fill = sum_quantile), color = NA) +
  geom_sf(data = eez, color = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.2, fill = NA) +
  geom_sf(data = can_prov, color = "black", linewidth = 0.1, alpha = 0.2, fill = NA) +
  scale_fill_gradientn(
    colours = rev(viridis(6, begin = 0, end = 0.85, option = "D")),
    name = "Threat Intensity",
    na.value = "grey100"
  ) +
  theme(legend.position = c(0.85, 0.8)) +
  theme(legend.background = element_rect(fill = NA), legend.position = "bottom") +
  labs(title = "Threat Intensity", face = "bold", wjust = 0.5) +
  facet_wrap(~threat_system)

ggsave(filename = paste0("can_threat_intensity_", GRID_RES, "_km2_lambert_20250314_SYSTEM_slr_fix.png"), width = 12, height = 8)
ggsave(filename = paste0("can_threat_intensity_", GRID_RES, "km2_lambert_20250314_SYSTEM_slr_fix.pdf"), width = 12, height = 8)


### *new*
# Now sum threat richness across systems
###
smry2 <- can_hex_grid_p_st_threat_system %>%
  st_drop_geometry() %>%
  group_by(index) %>%
  summarise(sum_high = sum(sum_threat, na.rm = TRUE), sum_quantile = sum(sum_quantile, na.rm = TRUE))

can_hex_grid_p_st_threat <- left_join(cells_to_process, smry2, join_by(index == index))

### join human pop quantile for models
human_pop_values <- threat_grids_long_with_thresh_theat %>%
  filter(basename == "human_pop_threat_grid.csv")
can_hex_grid_p_st_threat_pop <- left_join(can_hex_grid_p_st_threat, human_pop_values[,c("index","value", "quantile")] , join_by(index == index))

can_hex_grid_p_st_threat_pop <- can_hex_grid_p_st_threat_pop %>%
  rename(human_pop_value = colnames(can_hex_grid_p_st_threat_pop)[4],
         human_pop_quantile = colnames(can_hex_grid_p_st_threat_pop)[5])


# Save combined map to file
# st_write(can_hex_grid_p_st_threat, paste0("combined_threat_map_canada_", GRID_RES, "km2.shp"))
st_write(can_hex_grid_p_st_threat_pop, paste0("combined_threat_map_canada_", GRID_RES, "km2_20250314.shp"), delete_dsn = TRUE)
# can_hex_grid_p_st_threat = st_read(paste0("combined_threat_map_canada_", GRID_RES, "km2.shp"))
can_hex_grid_p_st_threat_test <- st_read(paste0("combined_threat_map_canada_", GRID_RES, "km2_20250314.shp"))


####NOT RUN FROM HERE



# # Get Canada outline from rworldmap and project
canada_outline <- st_as_sf(subset(rworldmap::countriesLow, NAME %in% "Canada"))
canada_outline_proj <- st_transform(st_union(canada_outline), crs = crs(s2))
# plot(canada_outline)

# # Get province data and project
can_prov <- st_read("./province_indiv_Dissolve.shp")
# plot(can_prov)
can_prov_proj <- st_transform(can_prov, crs = crs(s2))
can_hex_grid_p_st_threat_proj <- st_transform(can_hex_grid_p_st_threat, crs = crs(can_prov))
eez_proj <- st_transform(eez, crs = crs(can_prov))

library(ggplot2)
library(viridis)
library(ggnewscale)
g <- ggplot() +
  # geom_sf(data = can_prov, aes(fill = PRENAME), alpha = 0.4, show.legend = F) +
  scale_fill_viridis(discrete = TRUE) +
  new_scale("fill") +
  labs(colour = "Provence") +
  # geom_sf(data = subset(can_hex_grid_p_st_theat, sum_high > 0), aes(fill = sum_high), alpha = 0.2) +
  geom_sf(data = subset(can_hex_grid_p_st_threat_proj, sum_high > -1), aes(fill = sum_high), color = NA) +
  # geom_sf(data = eez, color = "black", alpha = 0.7) +
  geom_sf(data = eez_proj, color = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.2, fill = NA) +
  geom_sf(data = can_prov, color = "black", linewidth = 0.1, alpha = 0.2, fill = NA) +
  # geom_sf(data = eez_can_proj_hull, color = "red", linetype = "dashed", alpha = 0.2) +
  scale_fill_gradientn(
    colours = rev(magma(6)),
    name = "Frequency of threats",
    na.value = "grey100"
  ) +
  # labs(title = "Anthopogenic Theat Intensity for Canada", subtitle = "Cumulative frequency of high intensity threats", colour = "Frequency of threats") +
  coord_sf(crs = st_crs(eez_proj)) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  ggspatial::annotation_scale()

g

ggsave(filename = paste0("can_threat_", GRID_RES, "_km2_lambert_20241128.png"), width = 12, height = 12, plot = g)
ggsave(filename = paste0("can_threat_", GRID_RES, "km2_lambert_20241128.pdf"), width = 12, height = 12, plot = g)

## Trying out fast rasterisation
# library(tmap)
# tm_shape(can_hex_grid_p_st_threat_ind) +
#   tm_fill("access_proj_R_threat_grid.csv", palette = terrain.colors(10))
#
# r_temp <- rast(ext(can_hex_grid_p_st_threat), resolution = 1000, crs = st_crs(can_hex_grid_p_st_threat)$proj4string)
# r_data <- rasterize(can_hex_grid_p_st_threat, r_temp, field = "sum_high", fun = mean)


### *new*
## Individual threat maps.
## 1) Rename threats
###


cnames <- colnames(can_hex_grid_p_st_threat_ind_proj)

cnames <- gsub(pattern = "_R_threat_grid.csv", replacement = "", x = cnames)
cnames <- gsub(pattern = "_proj_threat_grid.csv", replacement = "", x = cnames)
cnames <- gsub(pattern = "_threat_grid.csv", replacement = "", x = cnames)

cnames <- gsub(pattern = "access_proj", replacement = "AccessCities", x = cnames)
cnames <- gsub(pattern = "access_ports_proj", replacement = "AccessPorts", x = cnames)
cnames <- gsub(pattern = "artisanal_fishing", replacement = "FishingArtisanal", x = cnames)
cnames <- gsub(pattern = "cattle_density", replacement = "CattleDensity", x = cnames)
cnames <- gsub(pattern = "chemgrids_total_sum", replacement = "Pesticides", x = cnames)
cnames <- gsub(pattern = "climate_map", replacement = "Precipitation", x = cnames)
cnames <- gsub(pattern = "climate_mat", replacement = "Temperature", x = cnames)
cnames <- gsub(pattern = "coastal_pollution", replacement = "PollutionCoastal", x = cnames)
cnames <- gsub(pattern = "dams", replacement = "Dams", x = cnames)
cnames <- gsub(pattern = "fishing_proj", replacement = "FishingCommercial", x = cnames)
cnames <- gsub(pattern = "forest_loss", replacement = "ForestLoss", x = cnames)
cnames <- gsub(pattern = "human_pop", replacement = "PopulationDensity", x = cnames)
cnames <- gsub(pattern = "invasives", replacement = "Invasives", x = cnames)
cnames <- gsub(pattern = "land_cover", replacement = "LandCover", x = cnames)
cnames <- gsub(pattern = "light", replacement = "PollutionLight", x = cnames)
cnames <- gsub(pattern = "N_appliction_total_sum", replacement = "Fertilizer", x = cnames)
cnames <- gsub(pattern = "ocean_acidification", replacement = "OceanAcidification", x = cnames)
cnames <- gsub(pattern = "ocean_pollution", replacement = "PollutionOcean", x = cnames)
cnames <- gsub(pattern = "oil_gas", replacement = "Extractions", x = cnames)
cnames <- gsub(pattern = "oil_rigs", replacement = "OilRigs", x = cnames)
cnames <- gsub(pattern = "rail_network", replacement = "Railways", x = cnames)
cnames <- gsub(pattern = "road_network", replacement = "Roads", x = cnames)
cnames <- gsub(pattern = "shipping", replacement = "Shipping", x = cnames)
cnames <- gsub(pattern = "slr", replacement = "SeaLevelRise", x = cnames)
cnames <- gsub(pattern = "sst", replacement = "AnomaliesSST", x = cnames)
cnames <- gsub(pattern = "uv", replacement = "AnomaliesUV", x = cnames)

colnames(can_hex_grid_p_st_threat_ind_proj) <- cnames

#col <- "LandCover"
all_plots <- list()
for (col in colnames(select(st_drop_geometry(can_hex_grid_p_st_threat_ind_proj), AccessPorts:AnomaliesUV))) {
  
  ### *new*
  # For each threat, work out the quantiles of that threat and a colour scale
  ###
  
  #plot_dat <- subset(can_hex_grid_p_st_threat_ind_proj, can_hex_grid_p_st_threat_ind_proj[[col]] >= -1)
  plot_dat <- can_hex_grid_p_st_threat_ind_proj
  fill_values_quantiles <- seq(0, 1, length.out = 11)
  ## use this for a vector of your quantile breaks for the labels (!)
  quants <- quantile(plot_dat[[col]], fill_values_quantiles, na.rm = T)
  ## convert every value in your fill to quantiles
  plot_dat$ptile_var <- ecdf(plot_dat[[col]])(plot_dat[[col]])

  fill_colours <- viridis(11)

  ### *new*
  # Plot threats, coloured by quantile
  # with the 'high' regions of the threat outlined in red (looks like red cells unless you zoom right in, but thats ok)
  ###
  g <- ggplot() +
    # geom_sf(data = can_prov, aes(fill = PRENAME), alpha = 0.4, show.legend = F) +
    scale_fill_viridis(discrete = TRUE) +
    new_scale("fill") +
    labs(colour = "Provence") +
    # geom_sf(data = subset(can_hex_grid_p_st_theat, sum_high > 0), aes(fill = sum_high), alpha = 0.2) +
    geom_sf(data = plot_dat, aes(fill = ptile_var), color = NA) +
    geom_sf(data = subset(can_hex_grid_p_st_threat_ind_proj, can_hex_grid_p_st_threat_ind_proj[[col]] == min(can_hex_grid_p_st_threat_ind_proj[[col]], na.rm = T)), fill = viridis(1), color = NA) +
    geom_sf(data = subset(can_hex_grid_p_st_threat_ind_filtered_proj, can_hex_grid_p_st_threat_ind_filtered_proj[[col]] >= 0), fill = NA, color = "red") +

    # geom_sf(data = subset(can_hex_grid_p_st_threat_ind_proj, can_hex_grid_p_st_threat_ind_proj[[col]] > 0), aes(fill = .data[[col]]), color = NA) +
    # geom_sf(data = eez, color = "black", alpha = 0.7) +
    geom_sf(data = eez_proj, color = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.2, fill = NA) +
    geom_sf(data = can_prov, color = "black", linewidth = 0.1, alpha = 0.2, fill = NA) +
    # geom_sf(data = eez_can_proj_hull, color = "red", linetype = "dashed", alpha = 0.2) +
    # scale_fill_gradientn(colours=rev(viridis(6, begin = 0, end = 0.85, option = 'D')),
    #                      name="Area",
    #                      na.value = "grey100") +
    scale_fill_stepsn(
      name = "Threat Intensity",
      ## use your vectors from above for breaks and labels
      colours = fill_colours,
      breaks = fill_values_quantiles,
      limits = c(0, 1),
      labels = quants,
      na.value = NA
    ) +
    # labs(title = "Anthopogenic Theat Intensity for Canada", subtitle = "Cumulative frequency of high intensity threats", colour = "Frequency of threats") +
    coord_sf(crs = st_crs(eez_proj)) +
    theme_bw() +
    theme(legend.position = c(0.85, 0.8)) +
    theme(legend.background = element_rect(fill = NA)) +
    labs(title = col, face = "bold", wjust = 0.5)
  #g
  # ggsave(paste0("threat_", col, ".pdf"), width = 12, height = 10)
  all_plots[[col]] <- g
}

# Combination plots ordered to match previous plots

patchwork::wrap_plots(all_plots[c(2, 1, 4, 14, 9, 16, 11, 12, 15)],
  nrow = 3, ncol = 3
)

ggsave("ind_threat_maps_v4-1.png", width = 16, height = 16)
ggsave("ind_threat_maps_v4-1.pdf", width = 16, height = 16)

patchwork::wrap_plots(all_plots[c(6, 7, 19, 20, 5, 21, 22, 26)],
  nrow = 3, ncol = 3
)

ggsave("ind_threat_maps_v4-2.png", width = 16, height = 16)
ggsave("ind_threat_maps_v4-2.pdf", width = 16, height = 16)

patchwork::wrap_plots(all_plots[c(3, 8, 10, 13, 17, 18, 24, 25, 23)],
  nrow = 3, ncol = 3
)

ggsave("ind_threat_maps_v4-3.png", width = 16, height = 16)
ggsave("ind_threat_maps_v4-3.pdf", width = 16, height = 16)
