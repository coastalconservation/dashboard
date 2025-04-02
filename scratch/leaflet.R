# Load packages
library(tidyverse)
library(leaflet)
library(readxl)
library(here)
library(sf)
library(tmap)

# Read and clean datasets
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

# Combine datasets (2000-2024)
biodiversity_data <- bind_rows(point_contact_data, quadrat_data, swath_data)

# Process data
biodiversity_data <- biodiversity_data %>%
  # Create binary presence column
  mutate(presence = ifelse(total_count > 0, 1, 0)) %>% # 1 = present, 0 = absent
  # Convert presence to a factor
  mutate(presence = factor(presence, levels = c(0, 1))) %>% 
  # Convert lat/long to an sf object
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

longterm_data <- longterm_data %>%
  # Create binary presence column
  mutate(presence = ifelse(percent_cover > 0, 1, 0)) %>% # 1 = present, 0 = absent
  # Convert presence to a factor
  mutate(presence = factor(presence, levels = c(0, 1))) %>% 
  # Convert lat/long to an sf object
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

# Read in CA polygon
california <- spData::us_states %>% 
  filter(NAME == "California") %>%
  st_transform(crs = 4326)

# Read in dangermond polygon
dangermond <- read_sf(here("shinydashboard", "data", "dangermond_shp", "jldp_boundary.shp")) %>%
  st_transform(crs = 4326)

# Create species distibution map (biodiversity)
tm_shape(california) +
  tm_polygons(col = "seagreen") +
tm_shape(subset(biodiversity_data, species_lump == "Pisaster giganteus" & year >= 2002 & year <= 2024)) +
  tm_symbols(col = "presence",
             shape = "presence",
             palette = c("darkgrey", "darkorange"),
             shapes = c(16, 8),
             size = 0.1,
             alpha = 1,
             legend.col.show = FALSE,
             legend.shape.show = FALSE) +
  tm_compass(position = c("right", "top"),
             size = 1) +
  tm_scale_bar(position = c("left", "bottom"),
               size = 1) +
  tm_add_legend(type = "symbol", 
                labels = c("Absent", "Present"),  
                col = c("darkgrey", "darkorange"),  
                shape = c(8, 16),
                title = "Presence") +
  tm_layout(main.title = "Species Distribution Along CA Coastline",
            main.title.size = 1,
            main.title.fontface = "bold",
            legend.title.size = 1,
            legend.text.size = 0.7,
            legend.outside = TRUE,
            bg.color = "ivory")

# leaflet map (biodiversity)
biodiversity_data %>%
  filter(species_lump == "Pisaster giganteus") %>%
  filter(year >= 2002 & year <= 2010) %>%
  leaflet() %>%
  addProviderTiles(provider = "Esri.WorldStreetMap") %>%
  setView(lng = -119.0,
          lat = 36.0,
          zoom = 6) %>%
  addMiniMap(toggleDisplay = TRUE, minimized = FALSE) %>%
  addPolygons(data = dangermond,
              color = "blue",
              fillColor = "blue",
              fillOpacity = 1,
              weight = 2) %>%
  addMarkers(data = dangermond,
             lng = -120.45,
             lat = 34.5,
             popup = "Jack and Lara Dangermond Preserve") %>%
  addCircleMarkers(color = ~ifelse(presence == 1, "red", "black"),
                   radius = ~ifelse(presence == 1, 2, 1.5),
                   opacity = ~ifelse(presence == 1, 1, 0.01)) %>%
  addLegend(position = "topright",
            colors = c("red", "black"),
            labels = c("Presence", "Absence"),
            title = "Species Presence")



# Create species distribution map (long-term)
tm_shape(california) +
  tm_polygons(col = "#A9DA3F") +
  tm_shape(subset(longterm_data, lumping_code == "MYTCAL" & year >= 2002 & year <= 2024)) +
  tm_symbols(col = "presence",
             shape = "presence",
             palette = c("black", "red"),
             shapes = c(16, 8),
             size = 0.1,
             alpha = 0.5,
             legend.col.show = FALSE,
             legend.shape.show = FALSE) +
  tm_compass(position = c("right", "top"),
             size = 1) +
  tm_scale_bar(position = c("left", "bottom"),
               size = 1) +
  tm_add_legend(type = "symbol", 
                labels = c("Absent", "Present"),  
                col = c("black", "red"),  
                shape = c(16, 8),
                title = "Presence") +
  tm_layout(main.title = "Species Distribution Along CA Coastline",
            main.title.size = 1,
            main.title.fontface = "bold",
            legend.title.size = 1,
            legend.text.size = 0.7,
            legend.outside = TRUE,
            bg.color = "#5ECFFA")


