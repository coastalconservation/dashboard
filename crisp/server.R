server <- function(input, output) {
  
  # home tab ----
  
  # slickR file path
  file_path <- reactive({
    
    list.files("www/carousel_images", full.names = TRUE, pattern = "jpg")
    
  })
  
  # slickR captions
  captions <- reactive({
    
    c("Jordan", "Amanda", "Ian", "Matteo", "Bruce", "Erica", "Max")
    
  })
  
  # slickR output
  output$carousel_images_output <- renderSlickR({
    
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
               autoplaySpeed = 5000)
    
  })
  
  # dangermond image
  output$dangermond <- renderImage({ 
    
    list(src = "www/diagrams/dangermond.jpg", contentType = "image/.jpg", width = "100%", height = "95%") 
    
  }, 
  
  deleteFile = FALSE
  
  )
  
  output$zoom_dangermond <- renderUI({
    
    req(input$dangermond_click)
    
    showModal(modalDialog(tags$img(src = "diagrams/dangermond.jpg", style = "width: 100%"),
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
                             scrollY = 350, 
                             paging = FALSE,
                             columnDefs = list(list(className = "dt-center", targets = "_all"))))
    
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
                             scrollY = 350, 
                             paging = FALSE, 
                             columnDefs = list(list(className = "dt-center", targets = "_all"))))
  })
  
  # dangermond range edges image
  output$cal_ranges <- renderImage({ 
    
    list(src = "www/diagrams/cal-ranges.jpg", contentType = "image/.jpg", width = "100%", height = "100%") 
    
  }, 
  
  deleteFile = FALSE 
  
  )
  
  output$zoom_modal <- renderUI({
    
    req(input$image_click)
    
    showModal(modalDialog(tags$img(src = "diagrams/cal-ranges.jpg", style = "width: 100%"),
                          easyClose = TRUE,
                          size = "m"))
    
  })
  
  
  # contemporary range shift tab ----
  
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
             style = "width: 350px; height: 350px; border-radius: 8px;")
    
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
    
    list(src = "www/diagrams/unnamed.png", contentType = "image/.jpg", width = 450, height = 525) 
    
  }, 
  
  deleteFile = FALSE 
  
  )
  
  output$zoom_coastline <- renderUI({
    
    req(input$distance_click)
    
    showModal(modalDialog(tags$img(src = "diagrams/unnamed.png", style = "width: 100%"),
                          easyClose = TRUE,
                          size = "m"))
    
  })
  
  # projected shifts tab ----
  
  # species info
  
  output$species_info_box <- renderUI({
    
    req(input$change_selected_species)
    
    # Convert dash-format back to species name format
    species_lump_input <- input$change_selected_species %>% 
      gsub("_", " ", .)
    
    info <- species_names %>%
      filter(species_lump == species_lump_input)
    
    image_url <- info$image_url
      
      tags$img(src = info$image_url,
               style = "width: 350px; height: 350px; border-radius: 8px;")
    
  })

  # current suitability map ----

  output$change_raster_output <- renderLeaflet({
    
    change_rast <- change_selected_raster()
    
    leaflet() |>
      addProviderTiles(provider = "Esri.WorldStreetMap") |>
      addRasterImage(change_rast, colors = change_habitat_pal) |>
      addLegend(
        pal = change_habitat_pal,
        values = c(-1, 1),
        title = paste0("Change in Habitat Suitability"),
        position = "bottomright"
      ) |>
      setView(lng = -120, lat = 36.7, zoom = 5) |>
      addMiniMap(toggleDisplay = TRUE, minimized = FALSE)
  })
  
  # current habitat map ----
  
  
  # current suitability habitat map ----

  current_selected_raster <- reactive({
    
    req(input$change_selected_species)
    
    current_file_path <- file.path("data/species_model_rasters/current_species_rasters",
                                   paste0("current_", gsub(" ", "_", input$change_selected_species), ".tif"))
    raster(current_file_path)
    
  })
  
  output$current_raster_output <- renderLeaflet({
    
    current_rast <- current_selected_raster()
    
    stable_habitat_pal <- colorBin(palette = c("#E4E2F5", "#FFC700","#49A842", "#038C45", "#00205B"),
                                   bins = seq(0, 1, length.out = 6),
                                   na.color = "transparent",
                                   right = FALSE)
    
    leaflet() |>
      addProviderTiles(provider = "Esri.WorldStreetMap") |>
      addRasterImage(current_rast, colors = stable_habitat_pal) |>
      addLegend(pal = stable_habitat_pal,
                values = c(-1, 1),
                title = paste0("2025 Habitat <br>Suitability"),
                position = "bottomright") |>
      setView(lng = -120, lat = 36.7, zoom = 5) |>
      addMiniMap(toggleDisplay = TRUE, minimized = FALSE)
    
  })
  
  # projected suitability map ----
  projected_selected_raster <- reactive({
    
    req(input$change_selected_species)
    
    projected_file_path <- file.path("data/species_model_rasters/projected_species_rasters",
                                     paste0("projected_", gsub(" ", "_", input$change_selected_species), ".tif"))
    
    raster(projected_file_path)
    
  })
  
  output$projected_raster_output <- renderLeaflet({
    
    projected_rast <- projected_selected_raster()
    
    stable_habitat_pal <- colorBin(palette = c("#E4E2F5", "#FFC700","#49A842", "#038C45", "#00205B"),
                                   bins = seq(0, 1, length.out = 6),
                                   na.color = "transparent",
                                   right = FALSE)
    
    leaflet() |>
      addProviderTiles(provider = "Esri.WorldStreetMap") |>
      addRasterImage(projected_rast, colors = stable_habitat_pal, opacity = 0.85) |>
      addLegend(pal = stable_habitat_pal,
                values = values(projected_rast),
                title = paste0("Projected Habitat <br>Suitability"),
                position = "bottomright") |>
      setView(lng = -120, lat = 36.7, zoom = 5) |>
      addMiniMap(toggleDisplay = TRUE, minimized = FALSE)
    
  })
  
  # change suitability map ----
  change_selected_raster <- reactive({
    
    req(input$change_selected_species)
    
    change_file_path <- file.path("data/species_model_rasters/change_species_rasters",
                                  paste0("ESDM_", gsub(" ", "_", input$change_selected_species), "_change.tif"))
    
    raster(change_file_path)
    
  })
  
  output$change_raster_output <- renderLeaflet({
    
    change_rast <- change_selected_raster()
    
    # Editing this to check something
    
    breaks <- c(-1, -0.6, -0.3, -.1, .1, 0.3, 0.6, 1)
    
    change_habitat_pal <- colorBin(palette = c("#00205B", "#FF0049", "#FFC700", "#E4E2F5","#00C2CB","#49A842","#038C45"),
                                   domain = c(-1, 1),
                                   bins = breaks,
                                   na.color = "transparent",
                                   right = FALSE)
    
    leaflet() |>
      addProviderTiles(provider = "Esri.WorldStreetMap") |>
      addRasterImage(change_rast, colors = change_habitat_pal) |>
      addLegend(pal = change_habitat_pal,
                values = c(-1, 1),
                title = paste0("Change in Habitat Suitability"),
                position = "bottomright") |>
      setView(lng = -120, lat = 36.7, zoom = 5) |>
      addMiniMap(toggleDisplay = TRUE, minimized = FALSE)
    
  })
  
  # contraction table output
  output$species_contraction_output <- renderDT({
    
    req(priority_species_joined)
    
    contraction <- priority_species_joined %>%
      filter(
        if (input$range_edge_filter) southern_range_edge == 1 else TRUE,
        if (input$north_trend_filter) northward_trend == 1 else TRUE,
        if (input$percent_change_filter) suitability_decrease_dangermond == 1 else TRUE,
        species_contraction_score %in% input$contraction  # <-- make sure you have input$contraction in your UI
      ) %>%
      mutate(
        image_html = paste0(
          '<div style="text-align: center;">',
          '<img src="', image_url, '" height="80" width="80" ',
          'style="object-fit: cover; display: block; margin: auto;" />',
          '</div>'
        )
      ) %>%
      
      mutate(
        percent_change_dangermond = paste0(round(percent_change_dangermond, 2), "%")
      ) %>%
      
      dplyr::select(
        "Common Name" = common_name,
        "Scientific Name" = species_lump,
        "Moving North" = northward_trend,
        "Lower suitable habitat in Dangermond" = suitability_decrease_dangermond,
        "Percentage change" = percent_change_dangermond,
        "Southern Range Edge in Point Conception" = southern_range_edge,
        "Total Score" = species_contraction_score,
        "Image" = image_html
      ) %>%
      arrange(desc(`Total Score`))
    
    datatable(
      data = contraction,
      escape = FALSE,
      rownames = FALSE,
      options = list(dom = 'tp', pageLength = 10)
    )
  })
  
  
  # expansion table output
  output$species_expansion_output <- renderDT({
    
    req(priority_species_joined)
    
    expansion <- priority_species_joined %>%
      filter(
        if (input$range_edge_filter) northern_range_edge == 1 else TRUE,
        if (input$north_trend_filter) northward_trend == 1 else TRUE,
        if (input$percent_change_filter) suitability_increase_dangermond == 1 else TRUE,
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
        "Moving North" = northward_trend,
        "Higher suitable habitat in Dangermond" = suitability_increase_dangermond,
        "Northern Range Edge in Point Conception" = northern_range_edge,
        "Total Score" = species_expansion_score,
        "Image" = image_html
      ) %>%
      arrange(desc(`Total Score`))
    
    datatable(
      data = expansion,
      escape = FALSE,
      rownames = FALSE,
      options = list(dom = 'tp', pageLength = 10)
    )
  })
  
  
  
  # acknowledgements tab ----
}