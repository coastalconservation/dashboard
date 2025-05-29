# load packages
library(readr)
library(tidyverse)

species_names <- read_csv("crisp/data/processed/species_names.csv") %>%
  select(! c(image, ...5))

# read data
target_boundaries <- read_rds("crisp/data/processed/target_boundaries.rds") %>%
  mutate(north_boundary = north_boundary/1000,
         south_boundary = south_boundary/1000) %>%
  inner_join(species_names, join_by = (species_lump == species), multiple = "all")

# create plot
target_boundaries %>%
  filter(species == "Roperia poulsoni") %>%
  ggplot() +
  geom_segment(aes(x = year_bin, xend = year_bin, 
                   y = south_boundary, yend = north_boundary), na.rm = TRUE) +
  geom_point(aes(x = year_bin, y = north_boundary), color = "#49A842", na.rm = TRUE) +
  geom_point(aes(x = year_bin, y = south_boundary), color = "#01c1e3", na.rm = TRUE) +
  geom_hline(yintercept = 520.8593, linetype = "dashed", color = "#ff004d") +
  coord_cartesian(clip = "off") +
  annotate("text", x = -Inf, y = 520.8593, 
           label = "Point Conception", 
           color = "#ff004d", size = 3, hjust = 0.4, vjust = -1) +
  annotate("text", x = -Inf, y = 0, 
           label = "CA/Mexico Border", 
           color = "#ff004d", size = 3, hjust = 0.4, vjust = -1) +
  labs(y = "Distance Along the CA Coast (km)") +
  scale_y_continuous(expand = c(0.02, 0), limits = c(0, NA)) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12, vjust = 5),
        axis.text = element_text(size = 10, color = "black"),
        axis.text.x = element_text(vjust = 0),
        axis.ticks = element_blank(),
        panel.grid = element_line(color = "#eae8f5", linetype = "dashed"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, "cm"))
