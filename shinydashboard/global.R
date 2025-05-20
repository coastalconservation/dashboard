
# load packages ----
library(sf)
library(DT)
library(shiny)
library(fresh)
library(slickR)
library(leaflet)
library(tidyverse)
library(shinyWidgets)
library(shinydashboard)
library(shinycssloaders)
library(raster)
library(here)

# Read Data
# Changing it so it calls in from the duck server -A
dangermond <- read_sf("/capstone/coastalconservation/data/raw/spatial_data/dangermond_shapefile/jldp_boundary.shp") %>%
  st_transform(crs = 4326)
species_extent <- read_csv("/capstone/coastalconservation/data/processed/species_extent.csv")
ca_segments <- st_read("/capstone/coastalconservation/data/processed/segments_shapefile/CA_segments.shp")
# Cumulative change raster
cumulative_change <- raster("/capstone/coastalconservation/data/processed/species_model_rasters/cumulative_species_rasters/cumulative_change.tif")


# Rasters for expect species shifts

# List all change raster files
change_raster_files <- list.files(
  "/capstone/coastalconservation/data/processed/species_model_rasters/change_species_rasters",
  pattern = "^ESDM_.*_change\\.tif$",
  full.names = TRUE
)

# Extract change species names from filenames
change_species_choices <- basename(change_raster_files) %>%
  str_remove("^ESDM_") %>%
  str_remove("_change\\.tif$") %>%
  sort()

# Colors for raster change species

breaks <- c(-1, -0.6, -0.3, -0.1, 0.1, 0.3, 0.6, 1)

breaks_total <- c(-10, -7, -3, 0, 3, 7, 10)

# Color palettes
change_habitat <- colorBin(
  palette = c("#00205B", # strong loss
                       "#FF0049", # moderate loss 
                       "#FFC700", # weak lost
                       "#E4E2F5", # no change 
                       "#00C2CB",  # weak gain 
                       "#038C45",  # moderate gain
                       "#49A842"),
  domain = c(-14, 14),
  bins = breaks_total,
  na.color = "transparent",
  right = FALSE)

change_habitat_pal <- colorBin(
  palette = c("#00205B", # strong loss
              "#FF0049", # moderate loss 
              "#FFC700", # weak lost
              "#E4E2F5", # no change 
              "#00C2CB",  # weak gain 
              "#038C45",  # moderate gain
              "#49A842"), 
  domain = c(-1, 1),
  bins = breaks,
  na.color = "transparent",
  right = FALSE
)

# List all current habitat raster files
current_raster_files <- list.files(
  "/capstone/coastalconservation/data/processed/species_model_rasters/current_species_rasters",
  pattern = "^current_.*\\.tif$",
  full.names = TRUE
)

# Extract species names from file names
current_species_choices <- basename(current_raster_files) %>%
  str_remove("^current_") %>%
  str_remove("\\.tif$") %>%
  sort()

stable_habitat_pal <- colorBin(
  palette = c("#E4E2F5", 
                       "#FFC700",
                       "#49A842",
                       "#00205B"), 
                       domain = c(0, 1),
  na.color = "transparent",
  right = FALSE
)

# Projected species in 2050

# List available rasters and extract species names
projected_raster_files <- list.files(
  "/capstone/coastalconservation/data/processed/species_model_rasters/projected_species_rasters",
  pattern = "^projected_.*\\.tif$",
  full.names = TRUE
)

projected_species_choices <- basename(projected_raster_files) %>%
  str_remove("^projected_") %>%
  str_remove("\\.tif$") %>%
  sort()

# Cumulative raster

