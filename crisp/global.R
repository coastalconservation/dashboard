# load packages ----
library(sf)
library(DT)
library(bslib)
library(shiny)
library(fresh)
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

cumulative_change <- raster("data/processed/species_model_rasters/cumulative_species_rasters/cumulative_change.tif")


change_raster_files <- list.files("data/processed/species_model_rasters/change_species_rasters",
                                  pattern = "^ESDM_.*_change\\.tif$",
                                  full.names = TRUE)

change_species_choices <- basename(change_raster_files) %>%
  str_remove("^ESDM_") %>%
  str_remove("_change\\.tif$") %>%
  sort()