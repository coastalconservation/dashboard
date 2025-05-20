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
           height = 400,
           width = "100%") %synch%
      (slickR(captions(), 
              slideType = "p",
              height = 25,
              width = "100%") + 
         settings(arrows = FALSE)) +
      settings(slidesToShow = 1,
               slidesToScroll = 1,
               arrows = TRUE,
               autoplay = TRUE,
               autoplaySpeed = 3000)
    
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
        summarize(num_species = length(species_lump), .groups = "keep") %>%
        arrange(desc(northern_extent_id)) %>%
        cbind(ca_segments, .)
      
    })
    
  })
  
  # northern range edge map ----
  output$northern_range_output <- renderLeaflet({
    
    # define bins
    bins <- c(0, 5, 10, 15)
    
    # color palette
    pal <- colorBin(palette = c("#49A842", "#256931", "#00291F"), 
                    domain = northern_range_edges()$num_species, 
                    bins = bins, 
                    right = FALSE)
    
    # leaflet map
    leaflet() %>%
      addProviderTiles(provider = "Esri.WorldStreetMap") %>%
      setView(lng = -120, lat = 36.7, zoom = 5) %>%
      addMiniMap(toggleDisplay = TRUE, minimized = FALSE) %>%
      addPolygons(data = northern_range_edges(),
                  layerId = ~northern_extent_name,
                  label = ~paste0(northern_extent_name, ": ", num_species, " sp."),
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
      addPolygons(data = northern_range_edges() %>%
                    filter(northern_extent_name == default_seg_north),
                  layerId  = "highlight_north",
                  fill = FALSE, color = "#ffc700", weight = 4, dashArray = "3") %>%
      addLegend(pal = pal, 
                values = northern_range_edges()$num_species, 
                title = "Number of Species",
                labFormat = labelFormat(digits = 0),
                opacity = 1)
    
  })
  
  # default segment
  default_seg_north <- sort(unique(species_extent$northern_extent_name))[2]
  clicked_seg_north <- reactiveVal(default_seg_north)
  
  # highlight default segment
  observe({
    
    leafletProxy("northern_range_output") %>%
      addPolygons(data = northern_range_edges() %>%
                    filter(northern_extent_name == clicked_seg_north()),
                  layerId = "highlight_north",
                  fill = FALSE, 
                  color = "#ffc700",
                  weight = 4, 
                  dashArray = "3")
    
  })
  
  # update polygon (click + highlight)
  observeEvent(input$northern_range_output_shape_click, {
    
    seg_north <- input$northern_range_output_shape_click$id
    
    clicked_seg_north(seg_north)
    
    leafletProxy("northern_range_output") %>%
      removeShape(layerId = "highlight_north") %>%
      addPolygons(data = northern_range_edges() %>%
                    filter(northern_extent_name == seg_north),
                  layerId = "highlight_north",
                  fill = FALSE, 
                  color = "#ffc700", 
                  weight = 4, 
                  dashArray = "3")
  })
    
  # filter data for DT ----
  north_dt <- reactive({
    
    species_extent %>%
      filter(northern_extent_name == clicked_seg_north()) %>%
      dplyr::select("Scientific Name" = species_lump, "Latitude" = northern_extent_lat) %>%
      arrange(desc(Latitude))
    
  })
  
  # title 
  output$table_header_north <- renderText({
    
    paste("Species with Northern Range Edges in", clicked_seg_north())
    
  })
  
  # DT ----
  output$northern_edge_table <- renderDT({
    
    datatable(north_dt(),
              rownames = TRUE,
              class = "hover", options = list(dom = "t", scrollY = 400, paging = FALSE))
    
  })
  
  # filter southern extent data ----
  southern_range_edges <- reactive({
    
    # refresh button
    input$refresh_southern_map
    
    isolate({
      
      species_extent %>%
        filter(!southern_extent_id %in% c(NA, 1, 18)) %>%
        group_by(southern_extent_id, southern_extent_name) %>%
        summarize(num_species = length(species_lump), .groups = "keep") %>%
        rbind(data.frame(southern_extent_id = 16,
                         southern_extent_name = "Eureka",
                         num_species = 0)) %>%
        arrange(desc(southern_extent_id)) %>%
        cbind(ca_segments, .)
      
    })
    
  })
  
  # default segment
  default_seg_south <- sort(unique(species_extent$southern_extent_name))[2]
  clicked_seg_south <- reactiveVal(default_seg_south)
  
  # southern range edge map ----
  output$southern_range_output <- renderLeaflet({
    
    # define bins
    bins <- c(0, 6, 11, 16, 21, 100)
    
    # color palette
    pal <- colorBin(palette = c("#01C1E3", "#0292BA", "#046490", "#053567", "#06063D"), 
                    domain = southern_range_edges()$num_species, 
                    bins = bins, 
                    right = FALSE)
    
    # leaflet map
    leaflet() %>%
      addProviderTiles(provider = "Esri.WorldStreetMap") %>%
      setView(lng = -120, lat = 36.7, zoom = 5) %>%
      addMiniMap(toggleDisplay = TRUE, minimized = FALSE) %>%
      addPolygons(data = southern_range_edges(),
                  layerId = ~southern_extent_name,
                  label = ~paste0(southern_extent_name, ": ", num_species, " sp."),
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
      addPolygons(data = southern_range_edges() %>%
                    filter(southern_extent_name == default_seg_south),
                  layerId = "highlight_south",
                  fill = FALSE, color = "#ffc700", weight = 4, dashArray = "3") %>%
      addLegend(pal = pal, 
                values = southern_range_edges()$num_species, 
                title = "Number of Species",
                labFormat = labelFormat(digits = 0),
                opacity = 1)
    
  })
  
  observeEvent(input$southern_range_output_shape_click, {
    
    seg_south <- input$southern_range_output_shape_click$id
    
    clicked_seg_south(seg_south)
    
    leafletProxy("southern_range_output") %>%
      removeShape(layerId = "highlight_south") %>%
      addPolygons(data = southern_range_edges() %>%
                    filter(southern_extent_name == seg_south),
                  layerId = "highlight_south",
                  fill = FALSE, color = "#ffc700", weight = 4, dashArray = "3")
  })
  
  south_dt <- reactive({
    
    species_extent %>%
      filter(southern_extent_name == clicked_seg_south()) %>%
      dplyr::select("Scientific Name" = species_lump, "Latitude" = southern_extent_lat) %>%
      arrange(Latitude)
    
  })
  
  output$table_header_south <- renderText({
    
    paste("Species with Southern Range Edges in", clicked_seg_south())
    
  })
  
  output$southern_edge_table <- renderDT({
    
    datatable(south_dt(),
              rownames = TRUE,
              class = "hover",
              options = list(dom = "t", scrollY = 400, paging = FALSE))
  })
  
  output$range_shift <- renderImage({ 
    
      list(src = "www/range-shift.jpg", width = "100%", height = "95%") 
    
    }, 
    
    deleteFile = FALSE 
    
  ) 
  
  output$cal_currents <- renderImage({ 
    
    list(src = "www/cal-currents.jpg", width = "100%", height = "95%") 
    
  }, 
  
  deleteFile = FALSE
  
  )
  
  output$cal_ranges <- renderImage({ 
    
    list(src = "www/cal-ranges.jpg", width = "100%", height = "95%") 
    
  }, 
  
  deleteFile = FALSE 
  
  )
  
  output$bob <- renderImage({ 
    
    list(src = "www/bob.jpeg", width = "100%", height = "95%") 
    
  }, 
  
  deleteFile = FALSE 
  
  )
  
    # Selecting species for change map ----
  change_selected_raster <- reactive({
    req(input$change_selected_species)
    
    change_file_path <- file.path(
      "/capstone/coastalconservation/data/processed/species_model_rasters/change_species_rasters",
      paste0("ESDM_", gsub(" ", "_", input$change_selected_species), "_change.tif")
    )
    raster(change_file_path)
  })
  
  output$change_raster_output <- renderLeaflet({
    change_rast <- change_selected_raster()
    
    leaflet() |>
      addProviderTiles(provider = "Esri.WorldStreetMap") |>
      addRasterImage(change_rast, colors = change_habitat_pal) |>
      addLegend(
        pal = change_habitat_pal,
        values = c(-1, 1),
        title = paste0("Change in Habitat Suitability <br> for ", input$change_selected_species),
        position = "bottomright"
      ) |>
      setView(lng = -120, lat = 36.7, zoom = 5) |>
      addMiniMap(toggleDisplay = TRUE, minimized = FALSE)
  })
  
  # Current habitat map ----
  current_selected_raster <- reactive({
    req(input$change_selected_species)
    
    current_file_path <- file.path(
      "/capstone/coastalconservation/data/processed/species_model_rasters/current_species_rasters",
      paste0("current_", gsub(" ", "_", input$change_selected_species), ".tif")
    )
    raster(current_file_path)
  })
  
  output$current_raster_output <- renderLeaflet({
    current_rast <- current_selected_raster()
    
    leaflet() |>
      addProviderTiles(provider = "Esri.WorldStreetMap") |>
      addRasterImage(current_rast, colors = stable_habitat_pal) |>
      addLegend(
        pal = stable_habitat_pal,
        values = c(-1, 1),
        title = paste0("Current Habitat Suitability for ", input$change_selected_species),
        position = "bottomright"
      ) |>
      setView(lng = -120, lat = 36.7, zoom = 5) |>
      addMiniMap(toggleDisplay = TRUE, minimized = FALSE)
  })
  
  # Projected habitat map ----
  projected_selected_raster <- reactive({
    req(input$change_selected_species)
    
    projected_file_path <- file.path(
      "/capstone/coastalconservation/data/processed/species_model_rasters/projected_species_rasters",
      paste0("projected_", gsub(" ", "_", input$change_selected_species), ".tif")
    )
    raster(projected_file_path)
  })
  
  output$projected_raster_output <- renderLeaflet({
    projected_rast <- projected_selected_raster()
    
    leaflet() |>
      addProviderTiles(provider = "Esri.WorldStreetMap") |>
      addRasterImage(projected_rast, colors = stable_habitat_pal, opacity = 0.85) |>
      addLegend(
        pal = stable_habitat_pal,
        values = values(projected_rast),
        title = paste0("Projected Habitat for ", input$change_selected_species),
        position = "bottomright"
      ) |>
      setView(lng = -120, lat = 36.7, zoom = 5) |>
      addMiniMap(toggleDisplay = TRUE, minimized = FALSE)
  })
  
  # Cumulative change map ----
  output$cumulative_change_output <- renderLeaflet({
    leaflet() |>
      addProviderTiles(provider = "Esri.WorldStreetMap") |>
      addRasterImage(cumulative_change, colors = change_habitat) |>
      addLegend(
        pal = change_habitat,
        values = values(cumulative_change),
        title = "Cumulative Habitat Change Across All Species",
        position = "bottomright"
      ) |>
      setView(lng = -120, lat = 36.7, zoom = 5) |>
      addMiniMap(toggleDisplay = TRUE, minimized = FALSE)
  }) # End cumulative change map
}