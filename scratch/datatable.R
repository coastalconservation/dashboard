# load packages
library(DT)
library(tidyverse)
library(readxl)

# read data
species_extent <- read_csv("crisp/data/processed/species_extent.csv")
species_names <- read_csv("crisp/data/processed/species_names.csv") %>%
  select(! c(image, ...5))

# DT
species_extent <- species_extent %>%
  inner_join(species_names, by = "species_lump")
  

species_extent %>%
  filter(!northern_extent_id %in% c(1, 18)) %>%
  select(! c(southern_extent_lat, southern_extent_id, southern_extent_m, southern_extent_name)) %>%
  select(common_name, species_lump, northern_extent_lat, northern_extent_name, image_url) %>%
  mutate(northern_extent_lat = formatC(northern_extent_lat, format = "f", digits = 2),
         image_url = paste0('<img src="', image_url, '" height="60"/>')) %>%
  datatable(colnames = c("Common Name", "Scientific Name", "Northern Extent Latitude", "Segment Name", "Image"),
            class = "hover",
            escape = FALSE,
            options = list(dom = "ft", scrollY = 400, paging = FALSE))

species_extent %>%
  filter(!southern_extent_id %in% c(1, 18)) %>%
  select(! c(northern_extent_lat, northern_extent_id, northern_extent_m, northern_extent_name)) %>%
  select(species_lump, southern_extent_lat) %>%
  mutate(southern_extent_lat = formatC(southern_extent_lat, format = "f", digits = 2)) %>%
  datatable(colnames = c("Scientific Name", "Southern Extent Latitude"),
            class = "hover",
            options = list(dom = "ft", scrollY = 400, paging = FALSE))

range_list %>%
  filter(range_edge_category %in% c("Northern Range Edge")) %>%
  select(species_lump, range_lat) %>%
  datatable(colnames = c("Scientific Name", "Latitude Range"),
            class = "hover",
            options = list(dom = "ft", scrollY = 400, paging = FALSE))

range_list %>%
  filter(range_edge_category %in% c("Southern Range Edge")) %>%
  select(species_lump, range_lat) %>%
  datatable(colnames = c("Scientific Name", "Latitude Range"), 
            class = "hover",
            options = list(dom = "ft", scrollY = 400, paging = FALSE))
