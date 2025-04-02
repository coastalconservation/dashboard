# load packages ----
library(sf)
library(shiny)
library(slickR)
library(leaflet)
library(tidyverse)
library(shinyWidgets)
library(shinydashboard)
library(shinycssloaders)

# Read Data
longterm_data <- read_csv("data/longterm_data_processed.csv")
biodiversity_data <- read_csv("data/biodiversity_data_processed.csv")
dangermond <- read_sf("data/dangermond_shp/dangermond.shp")
