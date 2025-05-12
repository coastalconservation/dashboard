library(sf)
library(tmap)

# CA coastline
coastline <- st_read("raw-data/raw/spatial_data/coastline/CA_coastline.shp")

# create a buffer around coastline
coastline_buffer <- st_buffer(coastline, dist = 10000) %>%
  st_simplify(coastline_buffer, dTolerance = 6000)

# coastal segments
segments <- read_csv("shinydashboard/data/processed/ca_segments.csv") %>%
  filter(!segment_id %in% c(1, 18, 19)) %>%
  select(!...1)

segment_polygons <- lapply(1:nrow(segments), function(i) {
  seg <- segments[i, ]
  
  # Create lat band as polygon box
  lat_band <- st_polygon(list(rbind(
    c(-125, seg$min_latitude),
    c(-114, seg$min_latitude),
    c(-114, seg$max_latitude),
    c(-125, seg$max_latitude),
    c(-125, seg$min_latitude)
  ))) %>%
    st_sfc(crs = 4326)
  
  # intersect with coastline buffer
  coastline_segments <- st_intersection(coastline_buffer, lat_band)
  
  # add metadata
  if (nrow(coastline_segments) > 0) {
    coastline_segments$segment_id <- seg$segment_id
    coastline_segments$segment_name <- seg$segment_name
  }
  
  return(coastline_segments)
})

coastline_segments <- do.call(rbind, segment_polygons)
st_write(coastline_segments, "raw-data/raw/spatial_data/segments/CA_segments.shp", delete_dsn = TRUE)