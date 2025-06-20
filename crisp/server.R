server <- function(input, output) {
  
  # home tab ----
  
  # slickR file path
  file_path <- reactive({
    
    list.files("www/carousel_images", full.names = TRUE, pattern = "jpg")
    
  })
  
  # slickR output
  output$carousel_images_output <- renderSlickR({
    
    slickR(file_path(),
           slideType = "img", 
           slideId = "Carousel",
           height = 400,
           width = "100%") +
      settings(slidesToShow = 1,
               slidesToScroll = 1,
               arrows = TRUE,
               autoplay = TRUE,
               autoplaySpeed = 5000)
    
  })
  
  # dangermond image
  output$dangermond <- renderImage({ 
    
    list(src = "www/diagrams/tnc-interactive-dangermond.jpg", contentType = "image/.jpg", width = "100%", height = "95%") 
    
  }, 
  
  deleteFile = FALSE
  
  )
  
  output$zoom_dangermond <- renderUI({
    
    req(input$dangermond_click)
    
    showModal(modalDialog(tags$img(src = "diagrams/tnc-interactive-dangermond.jpg", style = "width: 100%"),
                          easyClose = TRUE,
                          size = "m"))
    
  })
  
  # background tab ----
  
  # currents image
  output$cal_currents <- renderImage({ 
    
    list(src = "www/diagrams/cal-currents.jpg", contentType = "image/.jpg", width = "100%", height = "100%") 
    
  }, 
  
  deleteFile = FALSE
  
  )
  
  output$zoom_currents <- renderUI({
    
    req(input$currents_click)
    
    showModal(modalDialog(tags$img(src = "diagrams/cal-currents.jpg", style = "width: 100%"),
                          easyClose = TRUE,
                          size = "m"))
    
  })
  
  # range shift image
  output$range_shift <- renderImage({ 
    
    list(src = "www/diagrams/RANGESHIFT.png", contentType = "image/.jpg", width = "100%", height = "100%") 
    
  }, 
  
  deleteFile = FALSE 
  
  )
  
  output$zoom_shift <- renderUI({
    
    req(input$shift_click)
    
    showModal(modalDialog(tags$img(src = "diagrams/RANGESHIFT.png", style = "width: 100%"),
                          easyClose = TRUE,
                          size = "m"))
    
  })
  
  # kellet's whelk image
  output$kellet <- renderImage({ 
    
    list(src = "www/kellet.jpeg", contentType = "image/.jpeg", width = "100%", height = "100%") 
    
  }, 
  
  deleteFile = FALSE 
  
  )
  
  output$zoom_kellet <- renderUI({
    
    req(input$kellet_click)
    
    showModal(modalDialog(tags$img(src = "kellet.jpeg", style = "width: 100%"),
                          easyClose = TRUE,
                          size = "m"))
    
  })
  
  # range edges tab ----
  
  # filter northern species extent data
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
    
    bins <- c(0, 5, 10, 15)
    
    pal <- colorBin(palette = c("#e1e7f5", "#6993c6", "#027782"), 
                    domain = northern_range_edges()$num_species, 
                    bins = bins, 
                    right = FALSE)
    
    leaflet() %>%
      addProviderTiles(provider = "Esri.NatGeoWorldMap") %>%
      setView(lng = -120, lat = 36.7, zoom = 6) %>%
      addMiniMap(toggleDisplay = TRUE, minimized = FALSE) %>%
      addPolygons(data = northern_range_edges(),
                  layerId = ~northern_extent_name,
                  label = ~paste0(northern_extent_name, ": ", num_species, " species"),
                  fillColor = ~pal(num_species),
                  color = "black",
                  weight = 1.5,
                  fillOpacity = 1) %>%
      addPolygons(data = dangermond,
                  label = "The Jack and Laura Dangermond Preserve",
                  color = "#90214A",
                  fillColor = "#90214A",
                  fillOpacity = 1,
                  weight = 2) %>%
      addPolygons(data = northern_range_edges() %>%
                    filter(northern_extent_name == default_seg_north),
                  layerId  = "highlight_north",
                  fill = FALSE, color = "#90214A", weight = 4, dashArray = "3") %>%
      addLegend(pal = pal, 
                values = northern_range_edges()$num_species, 
                title = "Number of Species",
                labFormat = labelFormat(digits = 0),
                opacity = 1)
    
  })
  
  # default segment
  default_seg_north <- sort(unique(species_extent$northern_extent_name))[13]
  clicked_seg_north <- reactiveVal(default_seg_north)
  
  # highlight default segment
  observe({
    
    leafletProxy("northern_range_output") %>%
      addPolygons(data = northern_range_edges() %>%
                    filter(northern_extent_name == clicked_seg_north()),
                  layerId = "highlight_north",
                  fill = FALSE, 
                  color = "#90214A",
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
                  color = "#90214A", 
                  weight = 4, 
                  dashArray = "3")
  })
  
  # northern range edge DT ----
  north_dt <- reactive({
    
    species_extent %>%
      filter(northern_extent_name == clicked_seg_north()) %>%
      mutate(Latitude = signif(northern_extent_lat, digits = 4),
             image_url = paste0('<div style="text-align: center;">',
                                '<img src="', image_url, '" height="80" width="80" ',
                                'style="object-fit: cover; display: block; margin: auto;" />',
                                '</div>')) %>%
      dplyr::select("Common Name" = common_name, "Scientific Name" = species_lump, Latitude, "Image" = image_url) %>%
      arrange(desc(Latitude))
    
  })
  
  # dynamic title
  output$table_header_north <- renderText({
    
    paste(clicked_seg_north())
    
  })
  
  # DT output
  output$northern_edge_table <- renderDT({
    
    datatable(north_dt(),
              rownames = FALSE,
              escape = FALSE,
              class = "row-border",
              options = list(dom = "t", 
                             scrollY = 375, 
                             paging = FALSE,
                             columnDefs = list(list(className = "dt-center", targets = "_all"))), 
              selection = "none")
    
  })
  
  # filter southern species extent data
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
  
  # southern range edge map ----
  output$southern_range_output <- renderLeaflet({
    
    bins <- c(0, 5, 10, 15, 20, 75)
    
    pal <- colorBin(palette = c("#e1e7f5", "#a6bae0", "#6993c6", "#2aa5b0", "#027782"), 
                    domain = southern_range_edges()$num_species, 
                    bins = bins, 
                    right = FALSE)
    
    leaflet() %>%
      addProviderTiles(provider = "Esri.NatGeoWorldMap") %>%
      setView(lng = -120, lat = 36.7, zoom = 6) %>%
      addMiniMap(toggleDisplay = TRUE, minimized = FALSE) %>%
      addPolygons(data = southern_range_edges(),
                  layerId = ~southern_extent_name,
                  label = ~paste0(southern_extent_name, ": ", num_species, " species"),
                  fillColor = ~pal(num_species),
                  color = "black",
                  weight = 1.5,
                  fillOpacity = 1) %>%
      addPolygons(data = dangermond,
                  label = "The Jack and Laura Dangermond Preserve",
                  color = "#90214A",
                  fillColor = "#90214A",
                  fillOpacity = 1,
                  weight = 2) %>%
      addPolygons(data = southern_range_edges() %>%
                    filter(southern_extent_name == default_seg_south),
                  layerId = "highlight_south",
                  fill = FALSE, color = "#90214A", weight = 4, dashArray = "3") %>%
      addLegend(pal = pal, 
                values = southern_range_edges()$num_species, 
                title = "Number of Species",
                labFormat = labelFormat(digits = 0),
                opacity = 1)
    
  })
  
  # default segment
  default_seg_south <- sort(unique(species_extent$southern_extent_name))[12]
  clicked_seg_south <- reactiveVal(default_seg_south)
  
  # highlight default segment
  observe({
    
    leafletProxy("southern_range_output") %>%
      addPolygons(data = southern_range_edges() %>%
                    filter(southern_extent_name == clicked_seg_south()),
                  layerId = "highlight_south",
                  fill = FALSE, 
                  color = "#90214A",
                  weight = 4, 
                  dashArray = "3")
    
  })
  
  # update polygon (click + highlight)
  observeEvent(input$southern_range_output_shape_click, {
    
    seg_south <- input$southern_range_output_shape_click$id
    
    clicked_seg_south(seg_south)
    
    leafletProxy("southern_range_output") %>%
      removeShape(layerId = "highlight_south") %>%
      addPolygons(data = southern_range_edges() %>%
                    filter(southern_extent_name == seg_south),
                  layerId = "highlight_south",
                  fill = FALSE, color = "#90214A", weight = 4, dashArray = "3")
    
  })
  
  # southern range edge DT ----
  south_dt <- reactive({
    
    species_extent %>%
      filter(southern_extent_name == clicked_seg_south()) %>%
      mutate(Latitude = signif(southern_extent_lat, digits = 4),
             image_url = paste0('<div style="text-align: center;">',
                                '<img src="', image_url, '" height="80" width="80" ',
                                'style="object-fit: cover; display: block; margin: auto;" />',
                                '</div>')) %>%
      dplyr::select("Common Name" = common_name, "Scientific Name" = species_lump, Latitude, "Image" = image_url) %>%
      arrange(Latitude)
    
  })
  
  # dynamic title
  output$table_header_south <- renderText({
    
    paste(clicked_seg_south())
    
  })
  
  # DT output
  output$southern_edge_table <- renderDT({
    
    datatable(south_dt(),
              rownames = FALSE,
              escape = FALSE,
              class = "row-border",
              options = list(dom = "t", 
                             scrollY = 375, 
                             paging = FALSE, 
                             columnDefs = list(list(className = "dt-center", targets = "_all"))),
              selection = "none")
  })
  
  # dangermond range edges image
  output$range_edges <- renderImage({ 
    
    list(src = "www/diagrams/pointconception-segments.png", contentType = "image/.png", width = "100%", height = "100%") 
    
  }, 
  
  deleteFile = FALSE 
  
  )
  
  output$zoom_edges <- renderUI({
    
    req(input$edges_click)
    
    showModal(modalDialog(tags$img(src = "diagrams/pointconception-segments.png", style = "width: 100%"),
                          easyClose = TRUE,
                          size = "m"))
    
  })
  
  # contemporary range shift tab ----
  
  # contemporary range shift diagram
  output$contemp_shift <- renderImage({ 
    
    list(src = "www/diagrams/contemp-rangeshift.png", contentType = "image/.png", width = "100%", height = "100%") 
    
  }, 
  
  deleteFile = FALSE 
  
  )
  
  output$zoom_contemp <- renderUI({
    
    req(input$image_click)
    
    showModal(modalDialog(tags$img(src = "diagrams/contemp-rangeshift.png", style = "width: 100%"),
                          easyClose = TRUE,
                          size = "m"))
    
  })
  
  # filter target boundaries
  range_shift <- reactive({
    
    target_boundaries %>%
      mutate(north_boundary = north_boundary/1000,
             south_boundary = south_boundary/1000) %>%
      filter(full_name == input$species)
    
  })
  
  # species images
  output$species_image <- renderUI({
    
    tags$img(src = range_shift()$image_url[1], alt = input$species, 
             style = "width: 375px; height: 375px; border-radius: 8px;")
    
  })
  
  # dynamic title
  output$plotly_header <- renderText({
    
    paste(input$species)
    
  })
  
  # plots
  output$species_plot <- renderPlotly({
    
    ggplotly(width = 450, height = 525,
             ggplot(range_shift()) +
               geom_segment(aes(x = year_bin, xend = year_bin, 
                                y = south_boundary, yend = north_boundary), linewidth = 1.5, na.rm = TRUE) +
               geom_point(aes(x = year_bin, y = north_boundary), color = "#49A842", size = 3, na.rm = TRUE) +
               geom_point(aes(x = year_bin, y = south_boundary), color = "#01c1e3", size = 3, na.rm = TRUE) +
               geom_hline(yintercept = 520.8593, linetype = "dashed", color = "#ff004d") +
               geom_point(aes(x = 0, y = 520.8593, text = "Point Conception"), color = "#ff004d", alpha = 0) +
               geom_point(aes(x = 0, y = 0, text = "CA/MX Border"), color = "transparent") +
               geom_point(aes(x = 0, y = 200, text = "Orange County"), color = "transparent") +
               geom_point(aes(x = 0, y = 400, text = "Ventura"), color = "transparent") +
               geom_point(aes(x = 0, y = 600, text = "Pismo Beach"), color = "transparent") +
               geom_point(aes(x = 0, y = 800, text = "Big Sur"), color = "transparent") +
               geom_point(aes(x = 0, y = 1000, text = "Half Moon Bay"), color = "transparent") +
               geom_point(aes(x = 0, y = 1200, text = "Point Reyes"), color = "transparent") +
               geom_point(aes(x = 0, y = 1400, text = "Mendocino"), color = "transparent") +
               geom_point(aes(x = 0, y = 1600, text = "Eureka"), color = "transparent") +
               geom_point(aes(x = 0, y = 1800, text = "US/CA Border"), color = "transparent") +
               scale_y_continuous(expand = c(0.02, 0), limits = c(0, 1800), breaks = seq(0, 1800, by = 200)) +
               labs(y = "Distance Along the CA Coastline (km)") +
               theme_bw() +
               theme(axis.title.x = element_blank(),
                     axis.ticks = element_blank(),
                     panel.grid = element_line(color = "#eae8f5"),
                     panel.grid.major.x = element_blank(),
                     panel.grid.minor.y = element_blank(),
                     panel.border = element_blank()),
             tooltip = "text") %>%
      config(displayModeBar = FALSE)
    
  })
  
  # coastline distance image
  output$coastline_distance <- renderImage({ 
    
    list(src = "www/diagrams/coastline-key.png", contentType = "image/.png", width = 450, height = 525) 
    
  }, 
  
  deleteFile = FALSE 
  
  )
  
  output$zoom_coastline <- renderUI({
    
    req(input$distance_click)
    
    showModal(modalDialog(tags$img(src = "diagrams/coastline-key.png", style = "width: 100%"),
                          easyClose = TRUE,
                          size = "m"))
    
  })
  
  # dangermond range edges image
  output$range_edges2 <- renderImage({ 
    
    list(src = "www/diagrams/pointconception-segments.png", contentType = "image/.png", width = "100%", height = "100%") 
    
  }, 
  
  deleteFile = FALSE 
  
  )
  
  output$zoom_edges2 <- renderUI({
    
    req(input$edges2_click)
    
    showModal(modalDialog(tags$img(src = "diagrams/pointconception-segments.png", style = "width: 100%"),
                          easyClose = TRUE,
                          size = "m"))
    
  })
  
  # projected shifts tab ----
  
  # filter raster df
  habitat_suitability <- reactive({
    
    raster_df %>%
      filter(full_name == input$change_selected_species)
    
  })
  
  # species images
  output$species_info_box <- renderUI({
    
    tags$img(src = habitat_suitability()$image_url[1], alt = input$change_selected_species,
             style = "width: 375px; height: 375px; border-radius: 8px;")
    
  })
  
  # load change detection rasters
  change_selected_raster <- reactive({
    
    req(input$change_selected_species)
    
    # refresh button
    input$refresh_change_detection
    
    isolate({
      
      change_file_path <- file.path("data/species_model_rasters/change_species_rasters",
                                    paste0("ESDM_", habitat_suitability()$raster_name, "_change.tif"))
      
      raster(change_file_path)
      
    })
    
  })
  
  # change detection map ----
  output$change_raster_output <- renderLeaflet({
    
    change_rast <- change_selected_raster()
    
    breaks <- c(-1, -0.6, -0.3, -.1, .1, 0.3, 0.6, 1)
    
    change_habitat_pal <- colorBin(palette = c("#00205B", "#FF0049", "#FFC700", "#E4E2F5","#00C2CB","#49A842","#038C45"),
                                   domain = c(-1, 1),
                                   bins = breaks,
                                   na.color = "transparent",
                                   right = FALSE)
    
    leaflet() |>
      addProviderTiles(provider = "Esri.NatGeoWorldMap") |>
      addRasterImage(change_rast, colors = change_habitat_pal) |>
      addLegend(pal = change_habitat_pal,
                values = c(-1, 1),
                title = paste0("Change in Habitat Suitability"),
                position = "topright") |>
      setView(lng = -118, lat = 37, zoom = 5) |>
      addMiniMap(toggleDisplay = TRUE, minimized = FALSE)
    
  })
  
  # load current suitability rasters
  current_selected_raster <- reactive({
    
    req(input$change_selected_species)
    
    # refresh button
    input$refresh_current_suitability
    
    isolate({
      
      current_file_path <- file.path("data/species_model_rasters/current_species_rasters",
                                     paste0("current_", habitat_suitability()$raster_name, ".tif"))
      raster(current_file_path)
      
    })
    
  })
  
  # current suitability map ----
  output$current_raster_output <- renderLeaflet({
    
    current_rast <- current_selected_raster()
    
    stable_habitat_pal <- colorBin(palette = c("#E4E2F5", "#FFC700","#49A842", "#038C45", "#00205B"),
                                   bins = seq(0, 1, length.out = 6),
                                   na.color = "transparent",
                                   right = FALSE)
    
    leaflet() |>
      addProviderTiles(provider = "Esri.NatGeoWorldMap") |>
      addRasterImage(current_rast, colors = stable_habitat_pal) |>
      addLegend(pal = stable_habitat_pal,
                values = c(-1, 1),
                title = paste0("2025 Habitat Suitability"),
                position = "topright") |>
      setView(lng = -118, lat = 37, zoom = 5) |>
      addMiniMap(toggleDisplay = TRUE, minimized = FALSE)
    
  })
  
  # load projected suitability rasters
  projected_selected_raster <- reactive({
    
    req(input$change_selected_species)
    
    # refresh button
    input$refresh_projected_suitability
    
    isolate({
      
      projected_file_path <- file.path("data/species_model_rasters/projected_species_rasters",
                                       paste0("projected_", habitat_suitability()$raster_name, ".tif"))
      
      raster(projected_file_path)
      
    })
    
  })
  
  # projected suitability map ----
  output$projected_raster_output <- renderLeaflet({
    
    projected_rast <- projected_selected_raster()
    
    stable_habitat_pal <- colorBin(palette = c("#E4E2F5", "#FFC700","#49A842", "#038C45", "#00205B"),
                                   bins = seq(0, 1, length.out = 6),
                                   na.color = "transparent",
                                   right = FALSE)
    
    leaflet() |>
      addProviderTiles(provider = "Esri.NatGeoWorldMap") |>
      addRasterImage(projected_rast, colors = stable_habitat_pal, opacity = 0.85) |>
      addLegend(pal = stable_habitat_pal,
                values = values(projected_rast),
                title = paste0("Projected Habitat Suitability"),
                position = "topright") |>
      setView(lng = -118, lat = 37, zoom = 5) |>
      addMiniMap(toggleDisplay = TRUE, minimized = FALSE)
    
  })
  
  # dangermond range edges image
  output$range_edges3 <- renderImage({ 
    
    list(src = "www/diagrams/pointconception-segments.png", contentType = "image/.png", width = "100%", height = "100%") 
    
  }, 
  
  deleteFile = FALSE 
  
  )
  
  output$zoom_edges3 <- renderUI({
    
    req(input$edges3_click)
    
    showModal(modalDialog(tags$img(src = "diagrams/pointconception-segments.png", style = "width: 100%"),
                          easyClose = TRUE,
                          size = "m"))
    
  })
  
  # priority monitoring assessment tab ----
  
  # contraction table output
  output$species_contraction_output <- renderDT({
    
    req(priority_species_joined)
    
    contraction <- priority_species_joined %>%
      filter(eco_process == "contraction") %>% 
      filter(
        #if (input$range_edge_filter_con) southern_range_edge == 1 else TRUE,
        if (input$north_trend_filter_con) northward_trend == 1 else TRUE,
        if (input$percent_change_filter_con) suitability_decrease == 1 else TRUE,
        species_contraction_score %in% input$contraction  
      ) %>%
      mutate(
        image_html = paste0(
          '<div style="text-align: center;">',
          '<img src="', image_url, '" height="80" width="80" ',
          'style="object-fit: cover; display: block; margin: auto;" />',
          '</div>'
        ),
        percent_change_dangermond = paste0(round(percent_change_dangermond, 2), "%")
      ) %>%
      dplyr::select(
        "Common Name" = common_name,
        "Scientific Name" = species_lump,
        "Southern Range Edge in Dangermond" = southern_range_edge,
        "Moving North" = northward_trend,
        "Habitat Loss In Dangermond" = suitability_decrease,
        "Priority" = priority,
        "Image" = image_html
      ) %>%
      arrange(desc(`Priority`))
    
    datatable(
      data = contraction,
      escape = FALSE,
      rownames = FALSE,
      options = list(
        dom = 'tp', 
        pageLength = 10,
        scrollY = 350, 
        paging = FALSE,
        columnDefs = list(
          list(
            targets = c(2, 3, 4),
            render = JS(
              "function(data, type, row, meta) {",
              "return data == 1 ? '✔' : '';",
              "}"
            )
          ),
          list(className = "dt-center", targets = "_all")
        )
      ), selection = "none"
    )
  })
  
  
  # expansion table output
  output$species_expansion_output <- renderDT({
    
    req(priority_species_joined)
    
    expansion <- priority_species_joined %>%
      filter(eco_process == "expansion") %>% 
      filter(
        #if (input$range_edge_filter_exp) northern_range_edge == 1 else TRUE,
        if (input$north_trend_filter_exp) northward_trend == 1 else TRUE,
        if (input$percent_change_filter_exp) suitability_increase == 1 else TRUE,
        species_expansion_score %in% input$expansion
      ) %>%
      mutate(
        image_html = paste0(
          '<div style="text-align: center;">',
          '<img src="', image_url, '" height="80" width="80" ',
          'style="object-fit: cover; display: block; margin: auto;" />',
          '</div>'
        )
      ) %>%
      dplyr::select(
        "Common Name" = common_name,
        "Scientific Name" = species_lump,
        "Northern Range Edge In Dangermond" = northern_range_edge,
        "Moving North" = northward_trend,
        "Habitat Gain In Dangermond" = suitability_increase,
        "Priority" = priority,
        "Image" = image_html
      ) %>%
      arrange(desc(`Priority`))
    
    datatable(
      data = expansion,
      escape = FALSE,
      rownames = FALSE,
      options = list(
        dom = 'tp', 
        pageLength = 10,
        scrollY = 350, 
        paging = FALSE,
        columnDefs = list(
          list(
            targets = c(2, 3, 4),
            render = JS(
              "function(data, type, row, meta) {",
              "return data == 1 ? '✔' : '';",
              "}"
            )
          ),
          list(className = "dt-center", targets = "_all")
        )
      ), selection = "none"
    )
  })
  
}  