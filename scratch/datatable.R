# load packages
library(DT)
library(tidyverse)

# read data
range_list <- read_csv("shinydashboard/data/range_list.csv")

# DT
dt <- species_extent %>%
  filter(!northern_extent_id %in% c(1, 18, NA),
         !southern_extent_id %in% c(1, 18, NA)) %>%
  select(species_lump, southern_extent_lat, northern_extent_lat)



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
