server <- function(input, output) {
  
  # carousel images file path ----
  file_path <- reactive({
    
    list.files("www/carousel_images", full.names = TRUE, pattern = "jpg")
    
  })
  
  # image captions ----
  captions <- reactive({
    
    c("Jordan", "Amanda", "Ian", "Matteo", "Bruce", "Erica", "Max")
    
  })
  
  # slickR carousel ----
  output$carousel_images_output <- renderSlickR({
    
    # slickR carousel
    slickR(file_path(),
           slideType = "img", 
           slideId = "Carousel",
           height = 300,
           width = "100%") %synch%
      (slickR(captions(), 
              slideType = "p",
              height = 50,
              width = "100%") + 
         settings(arrows = FALSE)) +
      settings(slidesToShow = 1,
               slidesToScroll = 1,
               arrows = TRUE,
               autoplay = TRUE,
               autoplaySpeed = 3000)
    
  })
  
  # filter long-term data ----
  longterm_data_df <- reactive({
    
    longterm_data %>%
      filter(species_name == input$species_input[1]) %>%
      filter(year %in% input$year_input[1]:input$year_input[2])
    
  })
  
  # Build plot ----
  output$trend_plot_output <- renderPlot({
    
    # Average Percent Cover Overtime (including 0s) ----
    longterm_data_df() %>%
      group_by(year) %>%
      summarize(avg_percent_cover = mean(percent_cover, na.rm = TRUE)) %>%
      ggplot(aes(x = year, y = avg_percent_cover, group = 1)) +
      geom_point(col = "#0066cc", size = 2) +
      geom_line(col = "#0066cc", linewidth = 1) +
      scale_x_continuous(breaks = seq(2002, 2024, by = 5), expand = c(0, 0.1)) +
      labs(title =  "Changes in Percent Cover of Intertidal Species Along the CA Coast",
           y = "Average Percent Cover (%)") +
      theme_bw() +
      theme(axis.title.x = element_blank(),
            plot.title = element_text(size = 12,
                                      face = "bold")) 
    
  })
  
  # filter biodiversity data ----
  biodiversity_data_df <- reactive({
    
    biodiversity_data %>%
      filter(species_lump == input$species_input[1]) %>%
      filter(year %in% input$year_input[1]:input$year_input[2])
    
  })
  
  # Build leaflet map ----
  output$presence_map_output <- renderLeaflet({
    
    # leaflet map (biodiversity)
    biodiversity_data_df() %>%
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
                 popup = "The Jack and Laura Dangermond Preserve") %>%
      addCircleMarkers(color = ~ifelse(presence == 1, "red", "black"),
                       radius = 1,
                       opacity = 0.5) %>%
      addLegend(position = "topright",
                colors = c("red", "black"),
                labels = c("Present", "Absent"),
                title = "Species Presence")
    
  })
  
  # filter range list ----
  range_list_df <- reactive({
    
    range_list %>%
      filter(range_edge_category %in% c("Northern Range Edge", "Southern Range Edge")) %>%
      filter(segment_name == input$segment_input)
      
  })
  
  # DT ----
  output$DT_output <- renderDT({
    
    if (input$DT_input == "Northern") {
      
      range_list_df() %>%
        filter(range_edge_category %in% c("Northern Range Edge")) %>%
        select(species_lump, range_lat) %>%
        datatable(colnames = c("Scientific Name", "Latitude Range"),
                  class = "hover",
                  options = list(dom = "ft", scrollY = 400, paging = FALSE))
      
    } else if (input$DT_input == "Southern") {
      
      range_list_df() %>%
        filter(range_edge_category %in% c("Southern Range Edge")) %>%
        select(species_lump, range_lat) %>%
        datatable(colnames = c("Scientific Name", "Latitude Range"),
                  class = "hover",
                  options = list(dom = "ft", scrollY = 400, paging = FALSE))
      
    }
    
  })
  
  # filter range list ----
  value_box_df <- reactive({
    
    range_list %>%
      filter(segment_name %in% c("Northern Dangermond", "Southern Dangermond"))
    
  })
  
  # value boxes ----
  output$northern_output <- renderValueBox({
    
    valueBox(value_box_df() %>%
               filter(range_edge_category %in% c("Northern Range Edge")) %>%
               summarize(total = n()),
             subtitle = "Northern Range Edge",
             color = "black",
             icon = icon("arrows-up-to-line", style = "color: #49A842;"))
    
  })
  
  output$southern_output <- renderValueBox({
    
    valueBox(value_box_df() %>%
               filter(range_edge_category %in% c("Southern Range Edge")) %>%
               summarize(total = n()),
             subtitle = "Southern Range Edge",
             color = "black",
             icon = icon("arrows-down-to-line", style = "color: #49A842;"))
    
  })
  
  output$image_a <- renderUI({
    
    req(input$artist_input)
    tags$img(src = file.path("a", input$artist_input), height = 400, width = "100%")
    
  })
  
  output$image_b <- renderUI({
    
    req(input$artist_input)
    tags$img(src = file.path("b", input$artist_input), height = 400, width = "100%")
    
  })
  
  # filter northern extent data ----
  northern_range_edges <- reactive({
    
    # refresh button
    input$refresh_northern_map
    
    isolate({
      
      species_extent %>%
        filter(!northern_extent_id %in% c(NA, 1, 18)) %>%
        group_by(northern_extent_id, northern_extent_name) %>%
        summarize(num_species = length(species_lump)) %>%
        arrange(desc(northern_extent_id)) %>%
        cbind(ca_segments, .)
      
    })
    
  })
  
  # northern range edge map ----
  output$northern_range_output <- renderLeaflet({
    
    # define bins
    bins <- c(0, 5, 10, 15)
    
    # color palette
    pal <- colorBin(palette = "GnBu", 
                    domain = northern_range_edges()$num_species, 
                    bins = bins, 
                    right = FALSE)
    
    # leaflet map
    leaflet() %>%
      addProviderTiles(provider = "Esri.WorldStreetMap") %>%
      setView(lng = -120, lat = 36.7, zoom = 5) %>%
      addMiniMap(toggleDisplay = TRUE, minimized = FALSE) %>%
      addPolygons(data = northern_range_edges(),
                  label = northern_range_edges()$northern_extent_name,
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
                values = northern_range_edges()$num_species, 
                title = "Number of Species",
                labFormat = labelFormat(digits = 0),
                opacity = 1)
    
  })
  
  # filter southern extent data ----
  southern_range_edges <- reactive({
    
    # refresh button
    input$refresh_southern_map
    
    isolate({
      
      species_extent %>%
        filter(!southern_extent_id %in% c(NA, 1, 18)) %>%
        group_by(southern_extent_id, southern_extent_name) %>%
        summarize(num_species = length(species_lump)) %>%
        rbind(data.frame(southern_extent_id = 16,
                         southern_extent_name = "Eureka",
                         num_species = 0)) %>%
        arrange(desc(southern_extent_id)) %>%
        cbind(ca_segments, .)
      
    })
    
  })
  
  # southern range edge map ----
  output$southern_range_output <- renderLeaflet({
    
    # define bins
    bins <- c(0, 6, 11, 16, 21, 100)
    
    # color palette
    pal <- colorBin(palette = "GnBu", 
                    domain = southern_range_edges()$num_species, 
                    bins = bins, 
                    right = FALSE)
    
    # leaflet map
    leaflet() %>%
      addProviderTiles(provider = "Esri.WorldStreetMap") %>%
      setView(lng = -120, lat = 36.7, zoom = 5) %>%
      addMiniMap(toggleDisplay = TRUE, minimized = FALSE) %>%
      addPolygons(data = southern_range_edges(),
                  label = southern_range_edges()$southern_extent_name,
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
                values = southern_range_edges()$num_species, 
                title = "Number of Species",
                labFormat = labelFormat(digits = 0),
                opacity = 1)
    
  })
  
  # Selecting species for change map ----
  change_selected_raster <- reactive({
    req(input$change_selected_species)
    
    # Build file path to species-specific raster
    change_file_path <- file.path(
      "/capstone/coastalconservation/data/processed/species_model_rasters/change_species_rasters",
      paste0("ESDM_", gsub(" ", "_", input$change_selected_species), "_change.tif")
    )
    
    # Load raster
    raster(change_file_path)
  })
  
  # Render change raster leaflet map ----
  output$change_raster_output <- renderLeaflet({
    
    # Get selected species' change raster
    change_rast <- change_selected_raster()
    
    # Build leaflet map
    leaflet() |>
      addProviderTiles(provider = "Esri.WorldStreetMap") |>
      addRasterImage(change_rast, colors = change_habitat_pal) |>
      addLegend(
        pal = change_habitat_pal,
        values = c(-1, 1),  # full range for legend
        title = paste0("Change in Habitat Suitability for ", input$change_selected_species),
        position = "bottomright"
      ) |>
      setView(lng = -120, lat = 36.7, zoom = 5) |>
      addMiniMap(toggleDisplay = TRUE, minimized = FALSE)
  }) # End of select change in species
  
  # Present species habitat
  
  current_selected_raster <- reactive({
    req(input$current_selected_species)
    
    # Build file path to species-specific current habitat raster
    current_file_path <- file.path(
      "/capstone/coastalconservation/data/processed/species_model_rasters/current_species_rasters",
      paste0("current_", gsub(" ", "_", input$current_selected_species), ".tif")
    )
    
    # Load raster
    raster(current_file_path)
  })
  
  # Render current habitat raster leaflet map ----
  output$current_raster_output <- renderLeaflet({
    
    # Get selected species' current habitat raster
    current_rast <- current_selected_raster()
    
    # Build leaflet map
    leaflet() |>
      addProviderTiles(provider = "Esri.WorldStreetMap") |>
      addRasterImage(current_rast, colors = stable_habitat_pal) |>
      addLegend(
        pal = stable_habitat_pal,
        values = c(-1, 1),  # full domain (adjust if needed for current rasters)
        title = paste0("Current Habitat Suitability for ", input$current_selected_species),
        position = "bottomright"
      ) |>
      setView(lng = -120, lat = 36.7, zoom = 5) |>
      addMiniMap(toggleDisplay = TRUE, minimized = FALSE)
  }) # End of select current habitat in species
  
  
  # Selecting species for projected habitat map ----
  projected_selected_raster <- reactive({
    req(input$projected_selected_species)
    
    # Build file path
    projected_file_path <- file.path(
      "/capstone/coastalconservation/data/processed/species_model_rasters/projected_species_rasters",
      paste0("projected_", gsub(" ", "_", input$projected_selected_species), ".tif")
    )
    
    # Load raster
    raster(projected_file_path)
  })
  
  # Render projected habitat raster leaflet map ----
  output$projected_raster_output <- renderLeaflet({
    projected_rast <- projected_selected_raster()
    
    leaflet() |>
      addProviderTiles(provider = "Esri.WorldStreetMap") |>
      addRasterImage(projected_rast, colors = stable_habitat_pal, opacity = 0.85) |>
      addLegend(
        pal = stable_habitat_pal,
        values = values(projected_rast),
        title = paste0("Projected Habitat for ", input$projected_selected_species),
        position = "bottomright"
      ) |>
      setView(lng = -120, lat = 36.7, zoom = 5) |>
      addMiniMap(toggleDisplay = TRUE, minimized = FALSE)
  })
}