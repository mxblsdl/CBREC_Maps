
# script to pull out list of THP+ results that are greater than 100 Tons 
library(sf) # spatial data manipulation
library(dplyr) # used for joins
library(data.table) # basic data structures
library(ggplot2)

# additional plotting libraries
library(ggridges)
library(tidyr) # format data for ggridge (currently not working)
library(scales)

# set options
options(scipen = 999)

# load scenarios to remove list
# This is based on a size constraint we set on the THPs to remove really small THPs
# This workflow is in the results_mapping.RMD and involves analyis of the THP shapefiles
remove <- readRDS('scenarios_to_remove.rds')

# load spatial data
shapes <- read_sf("thps_2018.shp")

# load scenario matrix
scen_mat <- read.csv("scenarios.csv")

# filter scenario matrix to specified scenario
use <- 
  subset(scen_mat, Fraction_Piled == 50 & 
           Biomass_Collection == "All Tech Recoverable" & 
           Pulp_Market == "No" &
           Burn_Type == "None")

# get ids
use_ids <- use$ID

ref <- 
  subset(scen_mat, Fraction_Piled == 50 & 
           Biomass_Collection == "No" & 
           Pulp_Market == "No" &
           Burn_Type == "None")

# get ids
ref_ids <- ref$ID

# find all results files
files <- dir("climate_metrics_2019-11-13", full.names = T)

# find files which are larger than a certain size and have data

## create empty list
good_files <- list()

for (i in 1:length(files)) {
 
  # save file paths based on criteria 
  if(1000 < file.size(files[i])) {
  good_files[[i]] <- files[i]
  }
}

# filter out null values
good_files <- Filter(length, good_files)

# clean up
rm(files)

# extract polygon names
polys <- tools::file_path_sans_ext(gsub(".*/", "", unlist(good_files)))

# read files (wrap into filtering)
# Filter based on reference ids first
data <- lapply(good_files, function(file) {
  dat <-
    readRDS(file)
  da <- dat[grepl(paste0("\\b", ref_ids, "\\b", collapse = "|"), names(dat))]
  return(da)
})

# simplify list structure
data <- unlist(data, recursive = F) # retain list structure

# filter based on selected use case
data <-
  lapply(data, function(file) {
  file[grepl(paste0("\\b", use_ids, "\\b", collapse = "|"), names(file))]
})

# simplify list structure
data <- unlist(data, recursive = F)

# filter based on cbrec specific variables
data <- data[grepl("Dry-Chip", names(data))]

# extract important information from data
out <- list()
for (i in 1:length(data)) {
  out[[i]] <- data.table(
    ref = regmatches(names(data), gregexpr("[[:digit:]]+", names(data)))[[i]][1],# get reference id
    use = regmatches(names(data), gregexpr("[[:digit:]]+", names(data)))[[i]][3],# get use id
    AGTP_co2e = data[[i]]$ref_use_diff.ghg$MT_CO2e.AGTP.100yr,
    MT_mobilized = data[[i]]$ref_use_diff.ghg$MT_Residue_Mobilized,
    kwh_Generated = data[[i]]$ref_use_diff.ghg$kWh_Generated # is zero due to mistake in processing
    )
}

# bind together
data <- rbindlist(out)

# add polygon information to data
# TODO this would be better to preserve in the data somehow rather than binding together
data <- cbind(polys, data)

# convert data to correct units
data <-
  data %>%
  mutate(AGTP_co2e = AGTP_co2e * 1000000)

# density plot
# data %>%
#   ggplot() +
#   theme_minimal() +
#   geom_density(aes(AGTP_co2e), adjust = 1/5, fill = "orange", alpha = .5) +
#   labs(title = "Difference between use and reference case",
#        subtitle = "50% material piled with no prescribed burn",
#        x = expression("grams CO"[2]~"e per kWh"),
#        y = "Count") +
#   theme(text = element_text(size = 16))
# 
# # ggridges solution
# data %>%
#   pivot_longer(cols = AGTP_co2e) %>%
#   ggplot(aes(x = value, y = name, fill = ..x..)) +
#   theme_minimal() +
#   geom_density_ridges_gradient(show.legend = F, color = "white", scale = 5500) + # pick V large number
#   scale_fill_gradient(low = "#63B8FF", high = "#CD950C") +
#   labs(title = "Difference between use and reference case",
#        subtitle = "50% material piled with no prescribed burn",
#        x = expression("grams CO"[2]~"e per kWh"),
#        y = "Count") +
#   theme(text = element_text(size = 16))
  
# histogram version
g <- 
  ggplot(data, aes(x = AGTP_co2e, fill = ..x..)) +
  theme_minimal() +
  geom_histogram(binwidth = 10, show.legend = F) +
  scale_fill_gradient(low = "#63B8FF", high = "#CD950C") +
  labs(title = "Difference between use and reference case",
       subtitle = "50% material piled with no prescribed burn",
       x = expression("grams CO"[2]~"e per kWh"),
       y = "Count") +
  theme(text = element_text(size = 16))

plot_name = "hist_grad"

ggsave(g, filename = paste0("G:/My Drive/CBI_Unshared_Work/LCA Results Report/Graphics/figs_mb/scen_1/",
                            plot_name
                            ,".png"),
       dpi = 400,
       width = 7,
       height = 7)

# scatter plot of agtp to mobilized residue tons
g <- 
  data %>%
  ggplot(aes(x = AGTP_co2e, y = MT_mobilized)) +
  theme_minimal() +
  geom_point() +
  geom_smooth(method = "gam") +
  scale_y_log10(label = comma) +
  labs(title = "Difference between use and reference case",
       subtitle = "50% material piled with no prescribed burn",
       x = expression("grams CO"[2]~"e per kWh"),
       y = "Mobilized Residue (MT)") +
  theme(text = element_text(size = 16))
  
plot_name = "correlation_no_log"

ggsave(g, filename = paste0("G:/My Drive/CBI_Unshared_Work/LCA Results Report/Graphics/figs_mb/scen_1/",
                            plot_name
                            ,".png"),
       dpi = 400)
g
######################### Spatial Data Initital #################################

# bind spatial data together and write to disk
# rename column before join
shapes <-
  shapes %>%
  rename(polys = OBJECTID) %>%
  mutate(polys = as.character(polys))

# join data
data <-
  left_join(data, shapes)

# convert to spatial and write to disk
# data  %>%
#   st_as_sf() %>%
#   write_sf("output/spatial/all_thp_no_burn.shp", delete_layer = T)
###################################### Initial Plotting ###########################

## Second plot with different use and reference cases
# filter scenario matrix to specified scenario
use <- 
  subset(scen_mat, Fraction_Piled == 50 & 
           Biomass_Collection == "All Tech Recoverable" & 
           Pulp_Market == "No" &
           Burn_Type == "Broadcast")

# get ids
use_ids <- use$ID

ref <- 
  subset(scen_mat, Fraction_Piled == 50 & 
           Biomass_Collection == "No" & 
           Pulp_Market == "No" &
           Burn_Type == "Pile and Broadcast")

# get ids
ref_ids <- ref$ID

# find all results files
files <- dir("climate_metrics_2019-11-13", full.names = T)

# find files which are larger than a certain size and have data

## create empty list
good_files <- list()

for (i in 1:length(files)) {
  
  # save file paths based on criteria 
  if(1000 < file.size(files[i])) {
    good_files[[i]] <- files[i]
  }
}

# filter out null values
good_files <- Filter(length, good_files)

# clean up
rm(files)

# extract polygon names
polys <- tools::file_path_sans_ext(gsub(".*/", "", unlist(good_files)))

# read files (wrap into filtering)
# Filter based on reference ids first
data <- lapply(good_files, function(file) {
  dat <-
    readRDS(file)
  da <- dat[grepl(paste0("\\b", ref_ids, "\\b", collapse = "|"), names(dat))]
  return(da)
})

# simplify list structure
data <- unlist(data, recursive = F) # retain list structure

# filter based on selected use case
data <-
  lapply(data, function(file) {
    file[grepl(paste0("\\b", use_ids, "\\b", collapse = "|"), names(file))]
  })

# simplify list structure
data <- unlist(data, recursive = F)

# filter based on cbrec specific variables
data <- data[grepl("Dry-Chip", names(data))]

# extract important information from data
out <- list()
for (i in 1:length(data)) {
  out[[i]] <- data.table(
    ref = regmatches(names(data), gregexpr("[[:digit:]]+", names(data)))[[i]][1],# get reference id
    use = regmatches(names(data), gregexpr("[[:digit:]]+", names(data)))[[i]][3],# get use id
    AGTP_co2e = data[[i]]$ref_use_diff.ghg$MT_CO2e.AGTP.100yr,
    MT_mobilized = data[[i]]$ref_use_diff.ghg$MT_Residue_Mobilized,
    kwh_Generated = data[[i]]$ref_use_diff.ghg$kWh_Generated # is zero due to mistake in processing
  )
}

# bind together
data <- rbindlist(out)

# add polygon information to data
# TODO this would be better to preserve in the data somehow rather than binding together
data <- cbind(polys, data)

# convert data to correct units
data <-
  data %>%
  mutate(AGTP_co2e = AGTP_co2e * 1000000)

########################## Plots ##########################################
## Plot and save each 


# density plot
# g <-
#   data %>%
#   ggplot() +
#   theme_minimal() +
#   geom_density(aes(AGTP_co2e), adjust = 1/5, fill = "orange", alpha = .5) +
#   labs(title = "Difference between use and reference case",
#        subtitle = "Pile and Broadcast in reference case and Broadcast in use case",
#        x = expression("grams CO"[2]~"e per kWh"),
#        y = "Count") +
#   theme(text = element_text(size = 16))
# 
# plot_name = "str_density"
# 
# ggsave(g, filename = paste0("G:/My Drive/CBI_Unshared_Work/LCA Results Report/Graphics/figs_mb/scen_2/",
#                             plot_name
#                             ,".png"),
#        dpi = 400)
# 
# # ggridges solution
# data %>%
#   pivot_longer(cols = AGTP_co2e) %>%
#   ggplot(aes(x = value, y = name, fill = ..x..)) +
#   theme_minimal() +
#   geom_density_ridges_gradient(show.legend = F, color = "white") +
#   scale_fill_gradient(low = "#63B8FF", high = "#CD950C", space = "Lab", scale = 5500) +
#   labs(title = "Difference between use and reference case",
#        subtitle = "Pile and Broadcast in reference case and Broadcast in use case",
#        x = expression("grams CO"[2]~"e per kWh"),
#        y = "Count") +
#   theme(text = element_text(size = 16))
# 
# plot_name = "smooth_grad"
# 
# ggsave(g, filename = paste0("G:/My Drive/CBI_Unshared_Work/LCA Results Report/Graphics/figs_mb/scen_2/",
#                             plot_name
#                             ,".png"),
#        dpi = 400)

# histogram version
g <-
  ggplot(data, aes(x = AGTP_co2e, fill = ..x..)) +
  theme_minimal() +
  geom_histogram(binwidth = 10, show.legend = F) +
  scale_fill_gradient(low = "#63B8FF", high = "#CD950C") +
  labs(title = "Difference between use and reference case",
       subtitle = "Pile and Broadcast in reference case and Broadcast in use case",
       x = expression("grams CO"[2]~"e per kWh"),
       y = "Count") +
  theme(text = element_text(size = 16))

plot_name = "hist_grad"

ggsave(g, filename = paste0("G:/My Drive/CBI_Unshared_Work/LCA Results Report/Graphics/figs_mb/scen_2/",
                            plot_name
                            ,".png"),
       dpi = 400,
       width = 7,
       height = 7)

# scatter plot of agtp to mobilized residue tons
g <-
  data %>%
  ggplot(aes(x = AGTP_co2e, y = MT_mobilized)) +
  theme_minimal() +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_y_log10(label = comma) +
  labs(title = "Difference between use and reference case",
       subtitle = "Pile and Broadcast in reference case and Broadcast in use case",
       x = expression("grams CO"[2]~"e per kWh"),
       y = "Mobilized Residue (MT)") +
  theme(text = element_text(size = 16))

plot_name = "correlation"

ggsave(g, filename = paste0("G:/My Drive/CBI_Unshared_Work/LCA Results Report/Graphics/figs_mb/scen_2/",
                            plot_name
                            ,".png"),
       dpi = 400)

######################## Spatial data ##################################

# bind spatial data together and write to disk
# rename column before join
shapes <-
  shapes %>%
  rename(polys = OBJECTID) %>%
  mutate(polys = as.character(polys))

# join data
data <-
  left_join(data, shapes)

# write to disk
data %>%
  st_as_sf() %>%
write_sf("output/spatial/all_thps_with_burn.shp")

