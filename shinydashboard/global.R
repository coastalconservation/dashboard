
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
# Notes: The range_list csv was not on cyberduck, it has now been moved to cyberduck
# Several files that were called in from the "processed" cyberduck  folder were actually in the "raw" folder
dangermond <- read_sf("/capstone/coastalconservation/data/raw/spatial_data/dangermond_shapefile/jldp_boundary.shp") %>%
  st_transform(crs = 4326)
range_list <- read_csv("/capstone/coastalconservation/data/processed/range_list.csv")
species_extent <- read_csv("/capstone/coastalconservation/data/processed/species_extent.csv") #%>%
  #select(! ...1) Could not find `select` function, when loading dyplr it threw a new error, is this supposed to be selectInput()?
ca_segments <- st_read(here("california_coast_segments_polygons.shp"))


# Rasters for expect species shifts

# List all change raster files
raster_files <- list.files(
  "/capstone/coastalconservation/data/processed/species_model_rasters/change_species_rasters",
  pattern = "^ESDM_.*_change\\.tif$",
  full.names = TRUE
)

# Extract species names from filenames
species_choices <- basename(raster_files) %>%
  str_remove("^ESDM_") %>%
  str_remove("_change\\.tif$") %>%
  sort()


# Species change map models
Fucus_spp_change <- raster("/capstone/coastalconservation/data/processed/species_model_rasters/change_species_rasters/ESDM_Fucus_spp_change.tif")

# Get raster vals 
Fucus_spp_change_vals <- getValues(Fucus_spp_change)

# Color palettes
change_habitat <- c( "#381300","#49a842")

change_habitat_pal <- colorNumeric(palette = change_habitat,
             domain = values(Fucus_spp_change),
             na = "transparent")

