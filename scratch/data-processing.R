# Load packages
library(sf)
library(here)
library(readxl)
library(lubridate)
library(tidyverse)

# Read biodiversity data (point contact, quadrat, swath)
point_contact_data <- read_excel(here("raw-data", "raw", "MARINe_data", "biodiversity", "cbs_data_2025.xlsx"), sheet = 2) %>%
  select(! c("number_of_transect_locations", "percent_cover")) %>%
  rename(total_count = number_of_hits) %>%
  filter(island == "Mainland" & state_province == "California")

quadrat_data <- read_excel(here("raw-data", "raw", "MARINe_data", "biodiversity", "cbs_data_2025.xlsx"), sheet = 3) %>%
  select(! c("number_of_quadrats_sampled", "total_area_sampled_m2", "density_per_m2")) %>%
  filter(island == "Mainland" & state_province == "California")

swath_data <- read_excel(here("raw-data", "raw", "MARINe_data", "biodiversity", "cbs_data_2025.xlsx"), sheet = 4) %>%
  select(! c("number_of_transects_sampled", "est_swath_area_searched_m2", "density_per_m2")) %>%
  filter(island == "Mainland" & state_province == "California")

# Combine biodiversity data (2000-2024)
biodiversity_data <- bind_rows(point_contact_data, quadrat_data, swath_data) %>%
  mutate(presence = ifelse(total_count > 0, 1, 0)) %>%
  mutate(presence = factor(presence, levels = c(0, 1))) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

# Read in dangermond polygon
dangermond <- read_sf(here("raw-data", "dangermond_shp", "jldp_boundary.shp")) %>%
  st_transform(crs = 4326)

# Save processed data to data directory ----
write_csv(x = longterm_data, file = here::here("shinydashboard", "data", "longterm_data_processed.csv"))
write_csv(x = biodiversity_data, file = here::here("shinydashboard", "data", "processed", "biodiversity_data_processed.csv"))
st_write(dangermond, here::here("shinydashboard", "data", "dangermond_shp", "dangermond.shp"), delete_dsn = TRUE)
