server <- function(input, output) {
  
  # image path ----
  image_list <- reactive({
    
    list.files("www/images", full.names = TRUE, pattern = "jpg")
    
  })
  
  # build slickR carousel ----
  output$carousel_images_output <- renderSlickR({
    
    # slickR carousel ----
    slickR(
      image_list(),  
      height = 300,  
      width = "100%",  
      slideId = "Carousel"
    ) + 
      settings(
        slidesToShow = 1,
        slidesToScroll = 1,
        arrows = FALSE,
        dots = TRUE,
        autoplay = TRUE,
        autoplaySpeed = 3000
      )
    
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
  
}