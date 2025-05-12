# load packages
library(sf)
library(tmap)
library(tidyverse)

# read CA coastline shapefile (ESPG 4326)
coastline <- st_read("raw-data/raw/spatial_data/coastline/CA_coastline.shp")

# create a buffer around coastline
coastline_buffer <- st_buffer(x = coastline, dist = 10000) %>%
  st_simplify(dTolerance = 9000)

# read segment dataframe
segments <- read_csv("shinydashboard/data/processed/ca_segments.csv") %>%
  filter(!segment_id %in% c(1, 18, 19)) %>%
  select(!...1)

# create a function to create a shapefile of segment polygons
segment_polygons <- function(segments, coastline_buffer) {
  lapply(1:nrow(segments), function(i) {
    seg <- segments[i, ]
    
    # create polygons using max and min latitudes
    lat_band <- st_polygon(list(rbind(
      c(-125, seg$min_latitude),
      c(-117, seg$min_latitude),
      c(-117, seg$max_latitude),
      c(-125, seg$max_latitude),
      c(-125, seg$min_latitude)
    ))) %>%
      st_sfc(crs = 4326)
    
    # intersect coastline buffer to shape polygons
    segment_geom <- st_intersection(coastline_buffer, lat_band)
    
    # add metadata
    if (nrow(segment_geom) > 0) {
      segment_geom$segment_id <- seg$segment_id
      segment_geom$segment_name <- seg$segment_name
    }
    
    return(segment_geom)
  }) %>% 
    # bind geometries to segment df
    do.call(rbind, .)
}

# use function
coastline_segments <- segment_polygons(segments, coastline_buffer)

# save shapefile
st_write(coastline_segments, "shinydashboard/data/processed/spatial_data/segments_shapefile/CA_segments.shp", delete_dsn = TRUE)
