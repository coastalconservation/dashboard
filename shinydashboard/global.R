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

# Read Data
dangermond <- read_sf("data/processed/spatial_data/dangermond_shapefile/jldp_boundary.shp") %>%
  st_transform(crs = 4326)
range_list <- read_csv("data/processed/range_list.csv")
species_extent <- read_csv("data/processed/species_extent.csv") %>%
  select(! ...1)
ca_segments <- st_read("data/processed/spatial_data/segments_shapefile/CA_segments.shp")
