# load packages ----
library(sf)
library(DT)
library(bslib)
library(shiny)
library(fresh)
library(plotly)
library(raster)
library(slickR)
library(bsicons)
library(leaflet)
library(markdown)
library(tidyverse)
library(shinyWidgets)
library(shinydashboard)
library(shinycssloaders)

# read data ----

# 100 km coastline segments shapefile
ca_segments <- st_read("data/spatial_data/segments_shapefile/CA_segments.shp")

# Dangermond Preserve shapefile
dangermond <- read_sf("data/spatial_data/dangermond_shapefile/jldp_boundary.shp") %>%
  st_transform(crs = 4326)

# species info
species_names <- read_csv("data/species_info/species_names.csv") %>%
  dplyr::select(species_lump, common_name, image_url)

# species range edges data
species_extent <- read_csv("data/analyses_results/species_extent.csv") %>%
  inner_join(species_names, by = "species_lump")

# contemporary range shift data
target_boundaries <- read_rds("data/analyses_results/target_boundaries.rds") %>%
  rename(species_lump = species) %>%
  inner_join(species_names, by = "species_lump") %>%
  mutate(full_name = paste(species_lump, paste0("(", common_name, ")")))

# projected shifts picker widget (scientific/common name)
common_filtered <- species_names %>%
  filter(species_lump %in% target_boundaries$species_lump) %>%
  filter(!species_lump %in% c("Acanthinucella paucilirata", "Aglaophenia spp", "Calliostoma annulatum",
                              "Coryphella trilineata", "Opalia wroblewskyi", "Pseudomelatoma penicillata"))

named_choices <- list.files("data/species_model_rasters/change_species_rasters",
                            pattern = "^ESDM_.*_change\\.tif$",
                            full.names = TRUE) %>%
  basename() %>%
  str_remove("^ESDM_") %>%
  str_remove("_change\\.tif$") %>%
  sort() %>%
  setNames(paste(str_replace_all(., "_", " "), paste0("(", common_filtered$common_name, ")")))

# Read priority and suitability results

priority_scores <- read_rds("data/analyses_results/priority_species_scores.rds")%>% 
  select(-common_name)

suitability_changes <- read_csv("data/analyses_results/species_suitability_change.csv") 

priority_species_joined <- priority_scores %>%
  left_join(suitability_changes, by = c("species_lump" = "species_name")) %>%
  left_join(species_names, by = "species_lump")