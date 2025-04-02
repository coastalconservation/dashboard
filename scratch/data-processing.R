# Load packages
library(sf)
library(here)
library(readxl)
library(lubridate)
library(tidyverse)

# Species list
species_list <- c("CHOCAN", "CLACOL", "EGRMEN", 
                  "EISARB", "ENDMUR", "MAZAFF", 
                  "MYTCAL", "PISOCH", "POLPOL", 
                  "SARMUT", "SILCOM", "TETRUB", 
                  "NEOLAR", "SEMCAR", "HEDSES")

# Species names
species_names <- c(CHOCAN = "Chondracanthus canaliculatus", 
                   CLACOL = "Cladophora columbiana",
                   EGRMEN = "Egregia menziesii", 
                   EISARB = "Eisenia arborea", 
                   ENDMUR = "Endocladia muricata",
                   MAZAFF = "Mazzaella affinis", 
                   MYTCAL = "Mytilus californianus", 
                   PISOCH = "Pisaster ochraceus", 
                   POLPOL = "Pollicipes polymerus", 
                   SARMUT = "Sargassum muticum", 
                   SILCOM = "Silvetia compressa", 
                   TETRUB = "Tetraclita rubescens",
                   NEOLAR = "Neorhodomela larix", 
                   SEMCAR = "Semibalanus cariosus", 
                   HEDSES = "Hedophyllum sessile")

# Read long-term data (2002-2024)
longterm_data <- read_csv(here("raw-data", "phototranraw_download.csv")) %>%
  filter(state_province == "California" & island == "Mainland") %>%
  filter(!lumping_code %in% c("ROCK", "SAND", "TAR", "NONCRU")) %>%
  filter(lumping_code %in% species_list) %>%
  mutate(species_name = recode(lumping_code, !!!species_names)) %>%
  mutate(year = year(survey_date))

# Read biodiversity data (point contact, quadrat, swath)
point_contact_data <- read_excel(here("raw-data", "cbs_data_2025.xlsx"), sheet = 2) %>%
  select(! c("number_of_transect_locations", "percent_cover")) %>%
  rename(total_count = number_of_hits) %>%
  filter(island == "Mainland" & state_province == "California")

quadrat_data <- read_excel(here("raw-data", "cbs_data_2025.xlsx"), sheet = 3) %>%
  select(! c("number_of_quadrats_sampled", "total_area_sampled_m2", "density_per_m2")) %>%
  filter(island == "Mainland" & state_province == "California")

swath_data <- read_excel(here("raw-data", "cbs_data_2025.xlsx"), sheet = 4) %>%
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
write_csv(x = biodiversity_data, file = here::here("shinydashboard", "data", "biodiversity_data_processed.csv"))
st_write(dangermond, here::here("shinydashboard", "data", "dangermond_shp", "dangermond.shp"), delete_dsn = TRUE)
