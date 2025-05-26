# load packages ----
library(sf)
library(DT)
library(here)
library(shiny)
library(fresh)
library(raster)
library(slickR)
library(leaflet)
library(markdown)
library(tidyverse)
library(shinyWidgets)
library(shinydashboard)
library(shinycssloaders)

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

#current_raster_files <- list.files("data/processed/species_model_rasters/current_species_rasters",
                                   #pattern = "^current_.*\\.tif$",
                                   #full.names = TRUE)

#projected_raster_files <- list.files("data/processed/species_model_rasters/projected_species_rasters",
                                     #pattern = "^projected_.*\\.tif$",
                                     #full.names = TRUE)

cumulative_change <- raster("data/processed/species_model_rasters/cumulative_species_rasters/cumulative_change.tif")
