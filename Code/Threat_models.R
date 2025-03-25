# # Set up
library(dplyr)
library(data.table)
library(ggplot2)
library(forcats)
library(lme4)
library(sjPlot)
library(ggplot2)
library(tidyr)
library(MuMIn)

###############################################################################
##############Select data for models##############################
###############################################################################
###read in final dataset
lpi_cad_data_threats_habitats_withlambda <- read.csv("FINAL_WWF_Canada_dataset_140824_updated.csv")

# Get rid of replicates
#lpi_cad_data_threats_habitats_withlambda_noreps = subset(lpi_cad_data_threats_habitats_withlambda, Replicate == 0)
# these are now excluded from the dataset

# Add in updated number of threats
nrow(lpi_cad_data_threats_habitats_withlambda)
lpi_cad_data_threats_habitats_withlambda <- lpi_cad_data_threats_habitats_withlambda %>% 
  select(-c(index,sum_high))

WWF_Canada_dataset_20241201 <- read.csv("WWF_Canada_threat_df_20250314_withThreat_100km2.csv")

lpi_cad_data_threats_habitats_withlambda <- merge(lpi_cad_data_threats_habitats_withlambda, WWF_Canada_dataset_20241201[c("ID", "index", "sum_hgh", "sm_qntl", "hmn_pp_v","hmn_pp_q")], by = "ID")


# Select columns for models
model_data = select(lpi_cad_data_threats_habitats_withlambda, ID, Binomial, Taxonomic_group,Protected_status,Latitude, Longitude,Specific_location, System, Managed,Marine_neritic:Introduced_vegetation, X1970:X2022, av_lambda, n_habitats, Utilised, Managed, sum_lambda, sum_hgh, sm_qntl, hmn_pp_v,hmn_pp_q, Body_size) 

# # Convert habitat columns into long form (1 per row)
# id_habitat = tidyr::pivot_longer(select(lpi_cad_data_threats_habitats_withlambda_noreps, ID, Marine_neritic:Introduced_vegetation),  Marine_neritic:Introduced_vegetation)
# 
# # Get rid of NAs
# #id_habitat = subset(id_habitat, value != "NULL")
# id_habitat = subset(id_habitat, !is.na(value))
# 
# ### CHECK AS SOME SPECIES APPEARING TWICE ****
# # Stercorarius_longicaudus()
# # Stercorarius_longicaudus
# # Stercorarius_pomarinus
# # Stercorarius_pomarinus
# #*****
# 
# # Count the number of habtiats per pop ID
# n_habitats = id_habitat %>% 
#   group_by(ID) %>% 
#   summarise(n_habitats = n())
# 
# # Get a "TRUE' for any Grassland species/pops
# is_grassland = id_habitat %>% 
#   group_by(ID) %>% filter(name == "Grassland") %>% 
#   summarise(is_grassland = (value == 1)) # if there's a '1' in the value column for the grassland species, then it has that habitat (but may have others)
# 
# # Get a "TRUE' for any Forest species/pops 
# is_forest = id_habitat %>% 
#   group_by(ID) %>% 
#   filter(name == "Forest") %>% 
#   summarise(is_forest = (value == 1)) # if there's a '1' in the value column for the forest species, then it has that habitat (but it may have others)
# 
# # Get a "TRUE' for any FW species/pops 
# is_wetland = id_habitat %>% 
#   group_by(ID) %>% 
#   filter(name == "Wetlands_inland") %>% 
#   summarise(is_wetland = (value == 1)) # if there's a '1' in the value column for the wetland species, then it has that habitat (but it may have others)
# 
# # Join back together to add a n_habitats, is_grassland and is_forest columns
# model_data = left_join(model_data, n_habitats, by = join_by(ID))
# model_data = left_join(model_data, is_grassland, by = join_by(ID))
# model_data = left_join(model_data, is_forest, by = join_by(ID))
# model_data = left_join(model_data, is_wetland, by = join_by(ID))



#read in model data with bodymass, threat_richness, threat_intensity and human pop data
#model_data = read.csv("model_data_revised_20250312.csv")
# model_data = read.csv("model_data.csv")

#model_data <- read.csv("FINAL_WWF_Canada_dataset_090724.csv")

# Make a location ID
model_data$loc_id = as.factor(paste0(model_data$Longitude, "_", model_data$Latitude))
model_data = model_data %>% rename(
  threat_intensity = sm_qntl,
  threat_richness = sum_hgh
)

# Merge some factors in protected areas
model_data$Protected_status = forcats::fct_collapse(model_data$Protected_status, 
                                                    Yes = c("Yes", "Both"), 
                                                    No = c("No", "No (area surrounding PA)", "Unknown", "No (large survey area)"))
# model_data$Protected_status = forcats::fct_collapse(model_data$Protected_status, 
#                                                     Yes = c("Yes", "Both"), 
#                                                     No = c("No", "No (area surrounding PA)"), 
#                                                     Unknown = c("Unknown", "No (large survey area)"))

# Log body mass for scaling
model_data$Body_size_log <- log10(model_data$Body_size)
# Log human pop data
model_data$hmn_pp_v_log = log10(model_data$hmn_pp_v + 1)
# Scale threat intensity
model_data$threat_intensity_log = scale(model_data$threat_intensity)

# Calculate TS length
model_data$first <- apply((model_data[,27:79]), 1, function(x) Position(function(x)!is.na(x), x)) 
#lastValue <- Position(function(x) last(!is.na(x))
lastValue <- function(x) tail(which(x!="NA"),1)
model_data$last <- apply((model_data[,27:79]), 1, lastValue)
model_data$length <- as.numeric(model_data$last)-as.numeric(model_data$first)+1
head(model_data)
write.csv(model_data,"model_data.csv")

# Models - all taxa combined ####
### Average lambdas ####

# Select model data and remove missing values
model_data_no_na <- select(model_data, av_lambda, sum_lambda, Taxonomic_group, threat_intensity_log, threat_richness, hmn_pp_v_log, n_habitats, Protected_status, Utilised, Managed, Binomial, loc_id, Body_size_log, length) %>%
  drop_na()

nrow(model_data_no_na)
nrow(model_data)

#summary(model_data)

# Save model data with missing values removed
# model_data_no_na <- as.data.frame(scale(model_data_no_na))
write.csv(model_data_no_na, "model_data_no_na_20250314.csv")

#### Load Model data with missing values removed ####
model_data_no_na = read.csv("model_data_no_na_20250314.csv")

# mod_av_all = lmer(av_lambda ~ Taxonomic_group + sum_high + Protected_status + n_habitats + Body_size_log + Utilised + Managed +
#                     sum_high:Taxonomic_group + sum_high:n_habitats + n_habitats:Protected_status +
#                     (1|Binomial) + (1|loc_id), 
#                   data = model_data_no_na, na.action = "na.fail")

#model_data_non_na_nobirds = subset(model_data_no_na, Taxonomic_group != "Birds")


#########ALL SPECIES ##########################################################
#### Average Lambda models ####
mod_av_all_null = lmer(av_lambda ~ 1 +
                     (1|Binomial) + (1|loc_id), 
                   data = model_data_no_na, na.action = "na.fail", REML = FALSE)

mod_av_all_A = lmer(av_lambda ~ Taxonomic_group*threat_intensity_log*n_habitats +
                    Body_size_log + Utilised +  Managed + Protected_status +
                    (1|Binomial) + (1|loc_id), 
                  data = model_data_no_na, na.action = "na.fail", REML = FALSE)

mod_av_all_B = lmer(av_lambda ~ Taxonomic_group*threat_richness*n_habitats +
                    Body_size_log + Utilised +  Managed + Protected_status +
                    (1|Binomial) + (1|loc_id), 
                  data = model_data_no_na, na.action = "na.fail", REML = FALSE)

mod_av_all_C = lmer(av_lambda ~ Taxonomic_group*hmn_pp_v_log*n_habitats +
                      Body_size_log + Utilised +  Managed + Protected_status +
                      (1|Binomial) + (1|loc_id), 
                    data = model_data_no_na, na.action = "na.fail", REML = FALSE)

AICs <- AIC(mod_av_all_null, mod_av_all_A, mod_av_all_B, mod_av_all_C)
AICs

#define file name
sink("./Model results/AIC_output_av_all.txt")
print(AICs)
#close the external connection
sink()


tab_model(mod_av_all_null, mod_av_all_A, mod_av_all_B, mod_av_all_C,digits = 5)

# plot_model(mod_av_all_B, type = "pred", terms = c("threat_richness", "Taxonomic_group"), title = "Average change (all) threats")
# plot_model(mod_av_all_B, type = "pred", terms = c("n_habitats", "Taxonomic_group"), title = "Average change (all) threats")
# tab_model(mod_av_all_B, digits = 5)

#### Dredge - Av lambda  ####
d=dredge(mod_av_all_B)
d

#### top model - Av lambda  ####
top_model <- get.models(d, subset = 1)[[1]]
plot_model(top_model)
tab_model(top_model)
plot_model(top_model, type = "pred", terms = c("threat_richness", "Taxonomic_group"), title = "Average change (all) threats")
plot_model(top_model, type = "pred", terms = c("n_habitats", "Taxonomic_group"), title = "Average change (all) threats")

#### Model average - Av lambda  ####
av_model = model.avg(d, subset = delta <= 2, fit = TRUE)

summary(av_model)

#define file name
# sink("./Model results/Dredge_output_av_all_B.txt")
# print(d)
# #close the external connection
# sink()
write.csv(as.data.frame(d), "./Model results/Dredge_output_av_all_B.csv")
all_model <- get.models(d, subset=TRUE)
all_r2 = lapply(all_model, performance::r2)
all_r2_df = lapply(all_r2, as.data.frame)
#all_r2_combined = data.table::rbindlist(all_r2_df)
all_r2_combined <- dplyr::bind_rows(all_r2_df, .id = "model_number")
write.csv(all_r2_combined, "./Model results/Dredge_output_av_all_B_r2.csv")


#### Total Lambda models ####
mod_sum_all_null = lmer(sum_lambda ~ 1 + 
                     (1|Binomial) + (1|loc_id) + (1|length), 
                   data = model_data_no_na, na.action = "na.fail", 
                   REML = FALSE, control = lmerControl(optimizer = "bobyqa"))

mod_sum_all_A = lmer(sum_lambda ~ Taxonomic_group*threat_intensity_log*n_habitats +
                     Body_size_log + Utilised +  Managed + Protected_status + 
                     (1|Binomial) + (1|loc_id) + (1|length), 
                   data = model_data_no_na, na.action = "na.fail", 
                   REML = FALSE, control = lmerControl(optimizer = "bobyqa"))

mod_sum_all_B = lmer(sum_lambda ~ Taxonomic_group*threat_richness*n_habitats +
                     Body_size_log + Utilised +  Managed + Protected_status + 
                     (1|Binomial) + (1|loc_id) + (1|length), 
                   data = model_data_no_na, na.action = "na.fail", 
                   REML = FALSE, control = lmerControl(optimizer = "bobyqa"))

mod_sum_all_C = lmer(sum_lambda ~ Taxonomic_group*hmn_pp_v_log*n_habitats +
                      Body_size_log + Utilised +  Managed + Protected_status +
                      (1|Binomial) + (1|loc_id) + (1|length), 
                    data = model_data_no_na, na.action = "na.fail", 
                    REML = FALSE, control = lmerControl(optimizer = "bobyqa"))


AICs <- AIC(mod_sum_all_null, mod_sum_all_A, mod_sum_all_B, mod_sum_all_C)
AICs

#define file name
sink("./Model results/AIC_output_sum_all.txt")
print(AICs)
#close the external connection
sink()

tab_model(mod_sum_all_null, mod_sum_all_A, mod_sum_all_B, mod_sum_all_C,digits = 5)

# plot_model(mod_sum_all_A, type = "pred", terms = c("threat_intensity", "Taxonomic_group"), title = "Total change (all) threats")
# plot_model(mod_sum_all_A, type = "pred", terms = c("n_habitats", "Taxonomic_group"), title = "Total change (all) threats")
# tab_model(mod_sum_all_A, digits = 4)

# #### Dredge - Total lambda  ####
# d_sum=dredge(mod_sum_all_A)
# d_sum
# 
# #define file name
# sink("Dredge_output_sum_all_A.txt")
# print(d_sum)
# #close the external connection
# sink()
# 
# #### top model - Total lambda  ####
# top_model_sum <- get.models(d_sum, subset = 1)[[1]]
# plot_model(top_model_sum)
# plot_model(top_model_sum, type = "pred", terms = c("threat_intensity", "Taxonomic_group"), title = "Total change (all) threats")
# plot_model(top_model_sum, type = "pred", terms = c("n_habitats", "Taxonomic_group"), title = "Total change (all) threats")
# 
# #### Model average - Total lambda  ####
# av_sum_model = model.avg(d_sum, subset = delta <= 2, fit = TRUE)
# 
# summary(av_sum_model)


###########BIRDS###############################################################
### Birds
model_data_no_na_birds <- select(model_data, av_lambda, sum_lambda, Taxonomic_group, threat_intensity_log, threat_richness, hmn_pp_v_log, n_habitats, Protected_status, Utilised, Managed, Binomial, loc_id, Body_size_log, length) %>%
  subset(Taxonomic_group == "Birds") %>%
  drop_na()

nrow(model_data_no_na_birds)
write.csv(model_data_no_na_birds, "model_data_no_na_birds.csv")

#model_data_no_na_birds <- read.csv("model_data_no_na_birds.csv")

#### Average Lambda models ####
mod_av_birds_null = lmer(av_lambda ~ 1 +
                         (1|Binomial) + (1|loc_id), 
                       data = model_data_no_na_birds, na.action = "na.fail", REML = FALSE)
# doesn't run if I include protected status
mod_av_birds_A = lmer(av_lambda ~ threat_intensity_log*n_habitats +
                      Body_size_log + Utilised +  Managed +
                      (1|Binomial) + (1|loc_id), 
                    data = model_data_no_na_birds, na.action = "na.fail", REML = FALSE)
# doesn't run if I include protected status
mod_av_birds_B = lmer(av_lambda ~ threat_richness*n_habitats +
                      Body_size_log + Utilised +  Managed +
                      (1|Binomial) + (1|loc_id), 
                    data = model_data_no_na_birds, na.action = "na.fail", REML = FALSE)
# doesn't run if I include protected status
mod_av_birds_C = lmer(av_lambda ~ hmn_pp_v_log*n_habitats +
                      Body_size_log + Utilised +  Managed + 
                      (1|Binomial) + (1|loc_id), 
                    data = model_data_no_na_birds, na.action = "na.fail", REML = FALSE)

AICs <- AIC(mod_av_birds_null, mod_av_birds_A, mod_av_birds_B, mod_av_birds_C)
AICs

#define file name
sink("./Model results/AIC_output_av_birds.txt")
print(AICs)
#close the external connection
sink()

tab_model(mod_av_birds_null, mod_av_birds_A, mod_av_birds_B, mod_av_birds_C,digits = 5)

# plot_model(mod_av_birds_B, type = "pred", terms = c("threat_richness", "Taxonomic_group"), title = "Average change (birds) threats")
# plot_model(mod_av_birds_B, type = "pred", terms = c("n_habitats", "Taxonomic_group"), title = "Average change (birds) threats")
# tab_model(mod_av_birds_B, digits = 5)

#### Dredge - Av lambda  ####
d=dredge(mod_av_birds_B)
d

#### top model - Av lambda  ####
top_model <- get.models(d, subset = 1)[[1]]
plot_model(top_model)
tab_model(top_model)


#### Model average - Av lambda  ####
av_model = model.avg(d, subset = delta <= 2, fit = TRUE)

summary(av_model)

#define file name
sink("./Model results/Dredge_output_av_birds_B.txt")
print(d)
#close the external connection
sink()


#### Total Lambda models ####
mod_sum_birds_null = lmer(sum_lambda ~ 1 + 
                          (1|Binomial) + (1|loc_id) + (1|length), 
                        data = model_data_no_na_birds, na.action = "na.fail", 
                        REML = FALSE, control = lmerControl(optimizer = "bobyqa"))

mod_sum_birds_A = lmer(sum_lambda ~ threat_intensity_log*n_habitats +
                       Body_size_log + Utilised +  Managed + 
                       (1|Binomial) + (1|loc_id) + (1|length), 
                     data = model_data_no_na_birds, na.action = "na.fail", 
                     REML = FALSE, control = lmerControl(optimizer = "bobyqa"))

mod_sum_birds_B = lmer(sum_lambda ~ threat_richness*n_habitats +
                       Body_size_log + Utilised +  Managed + 
                       (1|Binomial) + (1|loc_id) + (1|length), 
                     data = model_data_no_na_birds, na.action = "na.fail", 
                     REML = FALSE, control = lmerControl(optimizer = "bobyqa"))

mod_sum_birds_C = lmer(sum_lambda ~ hmn_pp_v_log*n_habitats +
                       Body_size_log + Utilised +  Managed +
                       (1|Binomial) + (1|loc_id) + (1|length), 
                     data = model_data_no_na_birds, na.action = "na.fail", 
                     REML = FALSE, control = lmerControl(optimizer = "bobyqa"))


AICs <- AIC(mod_sum_birds_null, mod_sum_birds_A, mod_sum_birds_B, mod_sum_birds_C)
AICs

#define file name
sink("./Model results/AIC_output_sum_birds.txt")
print(AICs)
#close the external connection
sink()

tab_model(mod_sum_birds_null, mod_sum_birds_A, mod_sum_birds_B, mod_sum_birds_C,digits = 5)


#### Dredge - sum lambda  ####
d=dredge(mod_sum_birds_A)
d

#### top model - sum lambda  ####
top_model <- get.models(d, subset = 1)[[1]]
plot_model(top_model)
tab_model(top_model)


#### Model average - sum lambda  ####
av_model = model.avg(d, subset = delta <= 2, fit = TRUE)

summary(av_model)

#define file name
sink("./Model results/Dredge_output_sum_birds_A.txt")
print(d)
#close the external connection
sink()


###########mammals###############################################################
### mammals
model_data_no_na_mammals <- select(model_data, av_lambda, sum_lambda, Taxonomic_group, threat_intensity_log, threat_richness, hmn_pp_v_log, n_habitats, Protected_status, Utilised, Managed, Binomial, loc_id, Body_size_log, length) %>%
  subset(Taxonomic_group == "Mammals") %>%
  drop_na()

nrow(model_data_no_na_mammals)
write.csv(model_data_no_na_mammals, "model_data_no_na_mammals.csv")

#model_data_no_na_mammals <- read.csv("model_data_no_na_mammals.csv")

#### Average Lambda models ####
mod_av_mammals_null = lmer(av_lambda ~ 1 +
                           (1|Binomial) + (1|loc_id), 
                         data = model_data_no_na_mammals, na.action = "na.fail", REML = FALSE)

mod_av_mammals_A = lmer(av_lambda ~ threat_intensity_log*n_habitats +
                        Body_size_log + Utilised +  Managed + Protected_status +
                        (1|Binomial) + (1|loc_id), 
                      data = model_data_no_na_mammals, na.action = "na.fail", REML = FALSE)

mod_av_mammals_B = lmer(av_lambda ~ threat_richness*n_habitats +
                        Body_size_log + Utilised +  Managed + Protected_status +
                        (1|Binomial) + (1|loc_id), 
                      data = model_data_no_na_mammals, na.action = "na.fail", REML = FALSE)

mod_av_mammals_C = lmer(av_lambda ~ hmn_pp_v_log*n_habitats +
                        Body_size_log + Utilised +  Managed + Protected_status +
                        (1|Binomial) + (1|loc_id), 
                      data = model_data_no_na_mammals, na.action = "na.fail", REML = FALSE)

AICs <- AIC(mod_av_mammals_null, mod_av_mammals_A, mod_av_mammals_B, mod_av_mammals_C)
AICs

#define file name
sink("./Model results/AIC_output_av_mammals.txt")
print(AICs)
#close the external connection
sink()

tab_model(mod_av_mammals_null, mod_av_mammals_A, mod_av_mammals_B, mod_av_mammals_C,digits = 5)

# plot_model(mod_av_mammals_B, type = "pred", terms = c("threat_richness", "Taxonomic_group"), title = "Average change (mammals) threats")
# plot_model(mod_av_mammals_B, type = "pred", terms = c("n_habitats", "Taxonomic_group"), title = "Average change (mammals) threats")
# tab_model(mod_av_mammals_B, digits = 5)

# #### Dredge - Av lambda  ####
# d=dredge(mod_av_mammals_B)
# d
# 
# #### top model - Av lambda  ####
# top_model <- get.models(d, subset = 1)[[1]]
# plot_model(top_model)
# tab_model(top_model)
# 
# 
# #### Model average - Av lambda  ####
# av_model = model.avg(d, subset = delta <= 2, fit = TRUE)
# 
# summary(av_model)
# 
# #define file name
# sink("./Model results/Dredge_output_av_mammals_B.txt")
# print(d)
# #close the external connection
# sink()


#### Total Lambda models ####
mod_sum_mammals_null = lmer(sum_lambda ~ 1 + 
                            (1|Binomial) + (1|loc_id) + (1|length), 
                          data = model_data_no_na_mammals, na.action = "na.fail", 
                          REML = FALSE, control = lmerControl(optimizer = "bobyqa"))

mod_sum_mammals_A = lmer(sum_lambda ~ threat_intensity_log*n_habitats +
                         Body_size_log + Utilised +  Managed + Protected_status + 
                         (1|Binomial) + (1|loc_id) + (1|length), 
                       data = model_data_no_na_mammals, na.action = "na.fail", 
                       REML = FALSE, control = lmerControl(optimizer = "bobyqa"))

mod_sum_mammals_B = lmer(sum_lambda ~ threat_richness*n_habitats +
                         Body_size_log + Utilised +  Managed + Protected_status + 
                         (1|Binomial) + (1|loc_id) + (1|length), 
                       data = model_data_no_na_mammals, na.action = "na.fail", 
                       REML = FALSE, control = lmerControl(optimizer = "bobyqa"))

mod_sum_mammals_C = lmer(sum_lambda ~ hmn_pp_v_log*n_habitats +
                         Body_size_log + Utilised +  Managed + Protected_status +
                         (1|Binomial) + (1|loc_id) + (1|length), 
                       data = model_data_no_na_mammals, na.action = "na.fail", 
                       REML = FALSE, control = lmerControl(optimizer = "bobyqa"))


AICs <- AIC(mod_sum_mammals_null, mod_sum_mammals_A, mod_sum_mammals_B, mod_sum_mammals_C)
AICs

#define file name
sink("./Model results/AIC_output_sum_mammals.txt")
print(AICs)
#close the external connection
sink()

tab_model(mod_sum_mammals_null, mod_sum_mammals_A, mod_sum_mammals_B, mod_sum_mammals_C,digits = 5)


###########fish###############################################################
### fish
model_data_no_na_fish <- select(model_data, av_lambda, sum_lambda, Taxonomic_group, threat_intensity_log, threat_richness, hmn_pp_v_log, n_habitats, Protected_status, Utilised, Managed, Binomial, loc_id, Body_size_log, length) %>%
  subset(Taxonomic_group == "Fish") %>%
  drop_na()

nrow(model_data_no_na_fish)
write.csv(model_data_no_na_fish, "model_data_no_na_fish.csv")

#model_data_no_na_fish <- read.csv("model_data_no_na_fish.csv")

#### Average Lambda models ####
mod_av_fish_null = lmer(av_lambda ~ 1 +
                             (1|Binomial) + (1|loc_id), 
                           data = model_data_no_na_fish, na.action = "na.fail", REML = FALSE)

mod_av_fish_A = lmer(av_lambda ~ threat_intensity_log*n_habitats +
                          Body_size_log + Utilised +  Managed + Protected_status +
                          (1|Binomial) + (1|loc_id), 
                        data = model_data_no_na_fish, na.action = "na.fail", REML = FALSE)

mod_av_fish_B = lmer(av_lambda ~ threat_richness*n_habitats +
                          Body_size_log + Utilised +  Managed + Protected_status +
                          (1|Binomial) + (1|loc_id), 
                        data = model_data_no_na_fish, na.action = "na.fail", REML = FALSE)

mod_av_fish_C = lmer(av_lambda ~ hmn_pp_v_log*n_habitats +
                          Body_size_log + Utilised +  Managed + Protected_status +
                          (1|Binomial) + (1|loc_id), 
                        data = model_data_no_na_fish, na.action = "na.fail", REML = FALSE)

AICs <- AIC(mod_av_fish_null, mod_av_fish_A, mod_av_fish_B, mod_av_fish_C)
AICs

#define file name
sink("./Model results/AIC_output_av_fish.txt")
print(AICs)
#close the external connection
sink()

tab_model(mod_av_fish_null, mod_av_fish_A, mod_av_fish_B, mod_av_fish_C,digits = 5)

# plot_model(mod_av_fish_B, type = "pred", terms = c("threat_richness", "Taxonomic_group"), title = "Average change (fish) threats")
# plot_model(mod_av_fish_B, type = "pred", terms = c("n_habitats", "Taxonomic_group"), title = "Average change (fish) threats")
# tab_model(mod_av_fish_B, digits = 5)

# #### Dredge - Av lambda  ####
# d=dredge(mod_av_fish_B)
# d
# 
# #### top model - Av lambda  ####
# top_model <- get.models(d, subset = 1)[[1]]
# plot_model(top_model)
# tab_model(top_model)
# 
# 
# #### Model average - Av lambda  ####
# av_model = model.avg(d, subset = delta <= 2, fit = TRUE)
# 
# summary(av_model)
# 
# #define file name
# sink("./Model results/Dredge_output_av_fish_B.txt")
# print(d)
# #close the external connection
# sink()


#### Total Lambda models ####
mod_sum_fish_null = lmer(sum_lambda ~ 1 + 
                              (1|Binomial) + (1|loc_id) + (1|length), 
                            data = model_data_no_na_fish, na.action = "na.fail", 
                            REML = FALSE, control = lmerControl(optimizer = "bobyqa"))

mod_sum_fish_A = lmer(sum_lambda ~ threat_intensity_log*n_habitats +
                           Body_size_log + Utilised +  Managed + Protected_status + 
                           (1|Binomial) + (1|loc_id) + (1|length), 
                         data = model_data_no_na_fish, na.action = "na.fail", 
                         REML = FALSE, control = lmerControl(optimizer = "bobyqa"))

mod_sum_fish_B = lmer(sum_lambda ~ threat_richness*n_habitats +
                           Body_size_log + Utilised +  Managed + Protected_status + 
                           (1|Binomial) + (1|loc_id) + (1|length), 
                         data = model_data_no_na_fish, na.action = "na.fail", 
                         REML = FALSE, control = lmerControl(optimizer = "bobyqa"))

mod_sum_fish_C = lmer(sum_lambda ~ hmn_pp_v_log*n_habitats +
                           Body_size_log + Utilised +  Managed + Protected_status +
                           (1|Binomial) + (1|loc_id) + (1|length), 
                         data = model_data_no_na_fish, na.action = "na.fail", 
                         REML = FALSE, control = lmerControl(optimizer = "bobyqa"))


AICs <- AIC(mod_sum_fish_null, mod_sum_fish_A, mod_sum_fish_B, mod_sum_fish_C)
AICs

#define file name
sink("./Model results/AIC_output_sum_fish.txt")
print(AICs)
#close the external connection
sink()

tab_model(mod_sum_fish_null, mod_sum_fish_A, mod_sum_fish_B, mod_sum_fish_C,digits = 5)


###########herps###############################################################
### herps
model_data_no_na_herps <- select(model_data, av_lambda, sum_lambda, Taxonomic_group, threat_intensity_log, threat_richness, hmn_pp_v_log, n_habitats, Protected_status, Utilised, Managed, Binomial, loc_id, Body_size_log, length) %>%
  subset(Taxonomic_group == "Herps") %>%
  drop_na()

nrow(model_data_no_na_herps)
write.csv(model_data_no_na_herps, "model_data_no_na_herps.csv")

model_data_no_na_herps <- read.csv("model_data_no_na_herps.csv")

#### Average Lambda models ####
mod_av_herps_null = lmer(av_lambda ~ 1 +
                          (1|Binomial) + (1|loc_id), 
                        data = model_data_no_na_herps, na.action = "na.fail", REML = FALSE)

mod_av_herps_A = lmer(av_lambda ~ threat_intensity_log*n_habitats +
                       Body_size_log + Utilised +  Managed + Protected_status +
                       (1|Binomial) + (1|loc_id), 
                     data = model_data_no_na_herps, na.action = "na.fail", REML = FALSE)

mod_av_herps_B = lmer(av_lambda ~ threat_richness*n_habitats +
                       Body_size_log + Utilised +  Managed + Protected_status +
                       (1|Binomial) + (1|loc_id), 
                     data = model_data_no_na_herps, na.action = "na.fail", REML = FALSE)

mod_av_herps_C = lmer(av_lambda ~ hmn_pp_v_log*n_habitats +
                       Body_size_log + Utilised +  Managed + Protected_status +
                       (1|Binomial) + (1|loc_id), 
                     data = model_data_no_na_herps, na.action = "na.fail", REML = FALSE)

AICs <- AIC(mod_av_herps_null, mod_av_herps_A, mod_av_herps_B, mod_av_herps_C)
AICs

#define file name
sink("./Model results/AIC_output_av_herps.txt")
print(AICs)
#close the external connection
sink()

tab_model(mod_av_herps_null, mod_av_herps_A, mod_av_herps_B, mod_av_herps_C,digits = 5)

# plot_model(mod_av_herps_B, type = "pred", terms = c("threat_richness", "Taxonomic_group"), title = "Average change (herps) threats")
# plot_model(mod_av_herps_B, type = "pred", terms = c("n_habitats", "Taxonomic_group"), title = "Average change (herps) threats")
# tab_model(mod_av_herps_B, digits = 5)

# #### Dredge - Av lambda  ####
# d=dredge(mod_av_herps_B)
# d
# 
# #### top model - Av lambda  ####
# top_model <- get.models(d, subset = 1)[[1]]
# plot_model(top_model)
# tab_model(top_model)
# 
# 
# #### Model average - Av lambda  ####
# av_model = model.avg(d, subset = delta <= 2, fit = TRUE)
# 
# summary(av_model)
# 
# #define file name
# sink("./Model results/Dredge_output_av_herps_B.txt")
# print(d)
# #close the external connection
# sink()


#### Total Lambda models ####
mod_sum_herps_null = lmer(sum_lambda ~ 1 + 
                           (1|Binomial) + (1|loc_id) + (1|length), 
                         data =  model_data_no_na_herps, na.action = "na.fail", 
                         REML = FALSE, control = lmerControl(optimizer = "bobyqa"))

mod_sum_herps_A = lmer(sum_lambda ~ threat_intensity_log*n_habitats +
                        Body_size_log + Utilised +  Managed + Protected_status + 
                        (1|Binomial) + (1|loc_id) + (1|length), 
                      data =  model_data_no_na_herps, na.action = "na.fail", 
                      REML = FALSE, control = lmerControl(optimizer = "bobyqa"))

mod_sum_herps_B = lmer(sum_lambda ~ threat_richness*n_habitats +
                        Body_size_log + Utilised +  Managed + Protected_status + 
                        (1|Binomial) + (1|loc_id) + (1|length), 
                      data =  model_data_no_na_herps, na.action = "na.fail", 
                      REML = FALSE, control = lmerControl(optimizer = "bobyqa"))

mod_sum_herps_C = lmer(sum_lambda ~ hmn_pp_v_log*n_habitats +
                        Body_size_log + Utilised +  Managed + Protected_status +
                        (1|Binomial) + (1|loc_id) + (1|length), 
                      data =  model_data_no_na_herps, na.action = "na.fail", 
                      REML = FALSE, control = lmerControl(optimizer = "bobyqa"))


AICs <- AIC(mod_sum_herps_null, mod_sum_herps_A, mod_sum_herps_B, mod_sum_herps_C)
AICs

#define file name
sink("./Model results/AIC_output_sum_herps.txt")
print(AICs)
#close the external connection
sink()

tab_model(mod_sum_herps_null, mod_sum_herps_A, mod_sum_herps_B, mod_sum_herps_C,digits = 5)

