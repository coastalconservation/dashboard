# load packages
library(tidyverse)
library(tmap)
library(sf)
library(leaflet)

##############
species_extent <- read_csv("shinydashboard/data/processed/species_extent.csv") %>%
  select(! ...1)
ca_segments <- st_read("raw-data/raw/spatial_data/segments/CA_segments.shp")
dangermond <- st_read("raw-data/raw/spatial_data/dangermond_shapefile/jldp_boundary.shp") %>%
  st_transform(crs = 4326)

# total count per segment
count <- species_extent %>%
  filter(!southern_extent_id %in% c(NA, 1, 18)) %>%
  group_by(southern_extent_id, southern_extent_name) %>%
  summarize(num_species = length(species_lump))
  
# create row for segment 16
segment_16 <- data.frame(southern_extent_id = 16,
                         southern_extent_name = "Eureka",
                         num_species = 0)

# add row to df
count_updated <- rbind(count, segment_16) %>%
  arrange(desc(southern_extent_id))

# join dfs
segments <- cbind(ca_segments, count_updated)
  
# Define bins
bins <- c(0, 6, 11, 16, 21, 100)

# Create the palette with 5 color levels
pal <- colorBin(palette = "GnBu", 
                domain = segments$num_species, 
                bins = bins, 
                right = FALSE)  # Makes ranges left-inclusive

# Leaflet map
leaflet() %>%
  addProviderTiles(provider = "Esri.WorldStreetMap") %>%
  setView(lng = -120, 
          lat = 36.7, 
          zoom = 5) %>%
  addMiniMap(toggleDisplay = TRUE, minimized = FALSE) %>%
  addPolygons(data = segments,
              label = segments$southern_extent_name,
              fillColor = ~pal(num_species),
              color = "black",
              weight = 1,
              fillOpacity = 0.7) %>%
  addPolygons(data = dangermond,
              color = "black",
              fillColor = "black",
              fillOpacity = 1,
              weight = 2) %>%
  addMarkers(data = dangermond,
             lng = -120.45,
             lat = 34.5,
             popup = "Jack and Lara Dangermond Preserve") %>%
  addLegend(pal = pal, 
            values = segments$num_species, 
            title = "Number of Species",
            labFormat = labelFormat(digits = 0),
            opacity = 1)



