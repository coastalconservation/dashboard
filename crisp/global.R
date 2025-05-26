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
