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
ca_segments <- st_read("data/processed/spatial_data/segments_shapefile/CA_segments.shp")

dangermond <- read_sf("data/raw/spatial_data/dangermond_shapefile/jldp_boundary.shp") %>%
  st_transform(crs = 4326)

species_names <- read_csv("data/processed/species_names.csv") %>%
  select(species_lump, common_name, image_url)


species_extent <- read_csv("data/processed/species_extent.csv") %>%
  inner_join(species_names, by = "species_lump")


change_raster_files <- list.files("data/processed/species_model_rasters/change_species_rasters",
                                  pattern = "^ESDM_.*_change\\.tif$",
                                  full.names = TRUE)

change_species_choices <- basename(change_raster_files) %>%
  str_remove("^ESDM_") %>%
  str_remove("_change\\.tif$") %>%
  sort()

# Create a display-friendly version with spaces
nice_names <- str_replace_all(change_species_choices, "_", " ")

# Set names to display, values to keep original
named_choices <- setNames(change_species_choices, nice_names)

cumulative_change <- raster("data/processed/species_model_rasters/cumulative_species_rasters/cumulative_change.tif")

target_boundaries <- read_rds("data/processed/target_boundaries.rds")

# Add in assessment information

# Read priority and suitability results
priority_scores <- read_csv("data/processed/analyses_results/priority_species_scores.csv")
suitability_changes <- read_csv("data/processed/analyses_results/species_suitability_change.csv")

priority_species_joined <- priority_scores %>%
  left_join(suitability_changes, by = c("species_lump" = "species_name")) %>%
  left_join(species_names, by = "species_lump")
