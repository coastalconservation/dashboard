# dashboardHeader ----
header <- dashboardHeader(
  
  # title ----
  title = span(img(src = "TNCLogoPrimary_RGB_PNG.png", height = 70, style = "padding-top: 20px;")),
  titleWidth = 300,
  
  # navbar adjustments ----
  tags$li(class = "dropdown",
          tags$style(".main-header .logo {height: 100px;}"),
          tags$style(".sidebar-toggle {color: #05641C; font-size: 30px; padding-top: 10px !important;}"),
          tags$style(".skin-blue .main-header .navbar .sidebar-toggle {color: #05641C !important;}"),
          tags$div("California Ranges of Intertidal Species Portal (CRISP)",
                   style = "padding-top: 20px; padding-right: 300px; font-weight: normal; color: black; font-size: 30px;"))
  
) # END dashboardHeader

# dashboardSidebar ----
sidebar <- dashboardSidebar(
  
  # dashboard theme ----
  use_theme("dashboard-fresh-theme.css"),
  
  # sidebar adjustments ----
  tags$style(".left-side, .main-sidebar {padding-top: 100px; font-size: 14px;}"),
  width = 300,
  
  # sidebarMenu ----
  sidebarMenu(
    
    # menuItems
    menuItem(text = "Home", tabName = "home", icon = icon("house")),
    menuItem(text = "Background", tabName = "background", icon = icon("circle-info")),
    menuItem(text = "Range Edges", tabName = "ranges", icon = icon("location-dot")),
    menuItem(text = "Contemporary Range Shifts", tabName = "trends", icon = icon("arrow-trend-up")),
    menuItem(text = "Projected Shifts", tabName = "model", icon = icon("project-diagram")),
    menuItem(text = "Priority Monitoring Assessment", tabName = "assessment", icon = icon("list-check")),
    menuItem(text = "Data and Limitations", tabName = "data", icon = icon("database")),
    menuItem(text = "Acknowledgements", tabName = "acknowledgments", icon = icon("star"))
    
  ) # END sidebarMenu
  
) # END dashboardSidebar

# dashboardBody ----
body <- dashboardBody(
  
  # body adjustments ----
  tags$style(".content-wrapper, .right-side {padding-top: 25px;}"),
  
  # tabItems
  tabItems(
    
    # home tabItem ----
    tabItem(tabName = "home",
            
            # first fluidRow
            fluidRow(
              
              # left buffer column
              column(width = 1),
              
              # welcome box
              box(width = 5,
                  
                  div(style = "font-size: 16px;", includeMarkdown("text/welcome.md"))
                  
              ), # END welcomed box
              
              # slickR box
              box(width = 5,
                  
                  # slickR output
                  slickROutput(outputId = "carousel_images_output",
                               width = "100%",
                               height = "450px") %>%
                    withSpinner(color = "#05641C", type = 1, size = 1)
                  
              ), # END slickR box
              
              # right buffer column
              column(width = 1)
              
            ), # END first fluidRow
            
            # second fluidRow
            fluidRow(
              
              # left buffer column
              column(width = 1),
              
              # dangermond box
              box(width = 5), # END dangermond box
              
              # image box
              box(width = 5), # END image box
              
              # right buffer column
              column(width = 1)
              
            ), # END second fluidRow
            
    ), # END home tabItem
    
    # background tabItem ----
    tabItem(tabName = "background",
            
            # first fluidRow
            fluidRow(
              
              # left buffer column
              column(width = 1),
              
              # biogeographic box
              box(width = 5,
                  
                  div(style = "font-size: 16px;", includeMarkdown("text/biogeographic.md"))
                  
              ), # END biogeographic box
              
              # currents image box
              box(width = 5,
                  
                  imageOutput("cal_currents", click = "currents_click") %>%
                    withSpinner(color = "#05641C", type = 1, size = 1),
                  
                  uiOutput("zoom_currents"),
                  
                  "Figure 1. Insert Caption"
                  
              ), # END currents image box
              
              # right buffer column
              column(width = 1)
              
            ), # END first fluidRow
            
            # second fluidRow
            fluidRow(
              
              # left buffer column
              column(width = 1),
              
              # range shift image box
              box(width = 5,
                  
                  imageOutput("range_shift", click = "shift_click") %>%
                    withSpinner(color = "#05641C", type = 1, size = 1),
                  
                  uiOutput("zoom_shift"),
                  
                  "Figure 2. Insert Caption"
                  
              ), # END range shift image box
              
              # range shift box
              box(width = 5,
                  
                  div(style = "font-size: 16px;", includeMarkdown("text/range-shift.md"))
                  
              ), # END range shift box
              
              # right buffer column
              column(width = 1)
              
            ) # END second fluidRow
            
    ), # END background tabItem
    
    # range edges tabItem ----
    tabItem(tabName = "ranges",
            
            # first fluidRow
            fluidRow(
              
              # left buffer column
              column(width = 1),
              
              # info box
              box(width = 10,
                  
                  includeMarkdown("text/range-edges.md")
                  
              ), # END info box
              
              # right buffer column
              column(width = 1)
              
            ), # END first fluidRow
            
            # second fluidRow
            fluidRow(
              
              # left buffer column
              column(width = 1),
              
              # northern range edge map box
              box(width = 5,
                  
                  tags$h4("Northern Range Edges of Rocky Intertidal Species in CA",
                          style = "text-align: center; font-weight: bold; padding-bottom: 15px;"),
                  
                  actionButton(inputId = "refresh_northern_map", 
                               label = "Refresh Map",
                               icon = icon("rotate")),
                  
                  leafletOutput(outputId = "northern_range_output",
                                width = "100%") %>%
                    withSpinner(color = "#05641C", type = 1, size = 1)
                  
              ), # END northern range edge map box
              
              # northern range edge DT box
              box(width = 5,
                  
                  tags$h4(textOutput("table_header_north"),
                          style = "text-align: center; font-weight: bold; padding-bottom: 15px;"),
                  
                  DTOutput("northern_edge_table") %>%
                    withSpinner(color = "#05641C", type = 1, size = 1)
                  
              ), # END northern range edge DT box
              
              # right buffer column
              column(width = 1)
              
            ), # END second fluidRow
            
            # third fluidRow
            fluidRow(
              
              # left buffer column
              column(width = 1),
              
              # southern range edge map box
              box(width = 5,
                  
                  tags$h4("Southern Range Edges of Rocky Intertidal Species in CA",
                          style = "text-align: center; font-weight: bold; padding-bottom: 15px;"),
                  
                  actionButton(inputId = "refresh_southern_map", 
                               label = "Refresh Map",
                               icon = icon("rotate")),
                  
                  leafletOutput(outputId = "southern_range_output",
                                width = "100%") %>%
                    withSpinner(color = "#05641C", type = 1, size = 1)
                  
              ), # END southern range edge map box
              
              # southern range edge DT box
              box(width = 5,
                  
                  tags$h4(textOutput("table_header_south"),
                          style = "text-align: center; font-weight: bold; padding-bottom: 15px;"),
                  
                  DTOutput("southern_edge_table") %>%
                    withSpinner(color = "#05641C", type = 1, size = 1)
                  
              ), # END southern range edge DT box
              
              # right buffer column
              column(width = 1)
              
            ), # END third fluidRow
            
            # fourth fluidRow
            fluidRow(
              
              # left buffer column
              column(width = 1),
              
              # dangermond box
              box(width = 10,
                  
                  # fluidRow
                  fluidRow(
                    
                    tags$h3("Rocky Intertidal Species with Range Edges Near Point Conception",
                            style = "text-align: center; font-weight: bold; padding-bottom: 25px;"),
                    
                    # left-hand column
                    column(width = 6,
                           
                           # first fluidRow
                           fluidRow(
                             
                             column(width = 1),
                             
                             column(width = 10,
                                    
                                    value_box(title = span("Northern Range Edges",
                                                           style = "font-size: 22px; font-weight: bold; color: #eae8f5;"),
                                              value = span("23 species",
                                                           style = "font-size: 20px;"),
                                              showcase = span(bs_icon("arrow-up"),
                                                              style = "color: black;"),
                                              p("11 in Northern Point Conception", style = "font-size: 15px;"),
                                              p("12 in Southern Point Conception", style = "font-size: 15px;"),
                                              
                                              style = "background-color: #05641c; border-radius: 15px;",
                                              
                                              style = "background-color: #01c1e3; border-radius: 15px;",
                                              
                                              height = "125px")
                                    
                             ),
                             
                             column(width = 1)
                             
                           ), # END first fluidRow
                           
                           # second fluidRow
                           fluidRow(
                             
                             column(width = 1),
                             
                             column(width = 10,
                                    
                                    value_box(title = span("Southern Range Edges",
                                                           style = "font-size: 22px; font-weight: bold;"),
                                              value = span("37 species",
                                                           style = "font-size: 20px;"),
                                              showcase = span(bs_icon("arrow-down"),
                                                              style = "color: black;"),
                                              p("17 in Northern Point Conception", style = "font-size: 15px;"),
                                              p("20 in Southern Point Conception", style = "font-size: 15px;"),
                                              
                                              style = "background-color: #eae8f5; border-radius: 15px;",
                                              style = "background-color: #49a842; border-radius: 15px;",
                                              
                                              height = "125px")  
                                    
                             ),
                             
                             column(width = 1)
                             
                           ), # END second fluidRow
                           
                           # third fluidRow
                           fluidRow(
                             
                             column(width = 1),
                             
                             column(width = 10,
                                    
                                    value_box(title = span("MARINe Survey Sites",
                                                           style = "font-size: 22px; font-weight: bold; color: #eae8f5"),
                                              value = span("16",
                                                           style = "font-size: 20px;"),
                                              showcase = span(bs_icon("geo-alt"),
                                                              style = "color: black;"),
                                              style = "background-color: #06063d; border-radius: 15px;",
                                              height = "125px") 
                                    
                             ),
                             
                             column(width = 1)
                             
                           ), # END third fluidRow
                           
                    ), # END left-hand column
                    
                    # right-hand column
                    column(width = 6,
                           
                           imageOutput("cal_ranges", click = "image_click") %>%
                             withSpinner(color = "#05641C", type = 1, size = 1),
                           
                           uiOutput("zoom_modal")
                           
                    ) # END right-hand column
                    
                  ), # END fluidRow
                  
              ), # END dangermond box
              
              # right buffer column
              column(width = 1)
              
            ) # END fourth fluidRow
            
    ), # END range edges tabItem
    
    # contemporary range shifts tabItem ----
    tabItem(tabName = "trends",
            
            # first fluidRow
            fluidRow(
              
              # left buffer column
              column(width = 1),
              
              # info box
              box(width = 10,
                  
                  includeMarkdown("text/contemp-range.md")
                  
              ), # END info box
              
              # right buffer column
              column(width = 1)
              
            ), # END first fluidRow
            
            # second fluidRow
            fluidRow(
              
              # left buffer column
              column(width = 1),
              
              # picker column
              column(width = 3,
                     
                     pickerInput(inputId = "species", 
                                 label = "Choose an intertidal species:",
                                 choices = unique(target_boundaries$species),
                                 selected = unique(target_boundaries$species)[1],
                                 multiple = FALSE,
                                 options = pickerOptions(dropupAuto = FALSE,
                                                         size = 3))
                     
              ), # END picker column
              
              # right buffer column
              column(width = 8),
              
            ), # END second fluidRow
            
            # third fluidRow
            fluidRow(
              
              # left buffer column
              column(width = 1),
              
              # plot box
              box(width = 5,
                  
                  tags$h4(textOutput("plotly_header"),
                          style = "text-align: center; font-weight: bold; padding-bottom: 15px;"),
                  
                  plotlyOutput("species_plot") %>%
                    withSpinner(color = "#05641C", type = 1, size = 1)
                  
              ), # END plot box
              
              # plot box
              box(width = 5,
                  
                  imageOutput("coastline_distance", click = "distance_click") %>%
                    withSpinner(color = "#05641C", type = 1, size = 1),
                  
                  uiOutput("zoom_coastline")
                  
              ), # END plot box
              
              # right buffer column
              column(width = 1)
              
            ), # END third fluidRow
            
    ), # END historic range shifts tabItem
    
    # projected shifts tabItem ----
    tabItem(tabName = "model",
            
            # First fluid row
            fluidRow(
              
              # Buffer column one
              column(width = 1), # end column one
              
              # Info box
              box(width = 10,
                  includeMarkdown("text/model-info.md")), # End species rasters info box
              
              # Buffer column two
              column(width = 1) # End buffer column two
              
            ), # End first fluid row
            
            # Second fluid row 
            fluidRow(
              
              column(width = 1),
              
              # Column 1
              column(width = 3,
                     
                     # Species change map selector
                     pickerInput(inputId = "change_selected_species", 
                                 label = "Choose a Species:",
                                 choices = named_choices,
                                 multiple = FALSE,
                                 options = pickerOptions(dropupAuto = FALSE,
                                                         size = 7)
                     ),
                     
                     # Species image and common name
                     uiOutput("species_info_box")
                     
              ), # end of column 1
              
              # Leaflet box for change raster
              box(width = 7,
                  
                  # Title for change map
                  tags$h4("Map of Habitat Change From 2025 to 2050",
                          style = "text-align: center; font-weight: bold"),
                  
                  # Leaflet output
                  leafletOutput(outputId = "change_raster_output") %>% 
                    withSpinner(color = "#05641C", type = 1, size = 1)
              ), # End leaflet box for change raster
              
              column(width = 1)
              
            ), # End second fluid row
            
            # Third fluid row
            fluidRow(
              
              column(width = 1),
              
              # Column 1 with current raster map
              box(width = 5,
                  
                  # Title for current species map
                  tags$h4("Map of Species Habitat In 2025",
                          style = "text-align: center; font-weight: bold"),
                  # Leaflet output
                  leafletOutput(outputId = "current_raster_output") %>% 
                    withSpinner(color = "#05641C", type = 1, size = 1)
              ), # End of column 1 with current raster habitat
              
              
              # Column 2: projected habitat map
              box(width = 5,
                  
                  # Title for Project Species map
                  tags$h4("Map of Forecasted Species Habitat For 2050",
                          style = "text-align: center; font-weight: bold"),
                  
                  leafletOutput(outputId = "projected_raster_output") %>% 
                    withSpinner(color = "#05641C", type = 1, size = 1)
                  
              ), # End of column 2: projected habitat raster
              
              column(width = 1)
              
            ), # End third fluid row
            
            # fourth fluid row
            fluidRow(
              
              column(width = 1),
              
              # Column 1
              box(width = 10,
                  includeMarkdown("text/cumulative_difference_intro.md")), # end column 1
              
              column(width = 1)
              
            ), # End fourth fluid row
            
            # Fifth fluid row
            fluidRow(
              
              column(width = 1),
              # Column 1: buffer column
              column(width = 2,
                     
                     fluidRow(   # end column 1
                       
                       value_box(title = span("53 species",
                                              style = "font-size: 22px; font-weight: bold;"),
                                 value = span("identified as having",
                                              style = "font-size: 20px;"),
                                 p("range edges within California", style = "font-size: 15px;"),
                                 style = "background-color: #01c1e3; border-radius: 10px; width: 300px; padding: 20px;",
                                 height = "140px")
                     ), # end first fluid row
                     fluidRow(
                       value_box(title = span("19 species", style = "font-size: 22px; font-weight: bold;"),
                                 value = span("projected to gain habitat",
                                              style = "font-size: 20px;"),
                                 p("within California", style = "font-size: 15px;"),
                                 style = "background-color: #eae8f5; border-radius: 10px; width: 300px; padding: 20px;",
                                 height = "140px")
                       
                     ), # end second fluid row
                     
                     fluidRow(
                       value_box(title = span("34 species", style = "font-size: 24px; font-weight: bold; color: #eae8f5;"),
                                 value = span("projected to lose habitat", 
                                              style = "font-size: 20px; color: #eae8f5;"),
                                 p("within California", style = "font-size: 15px; color: #eae8f5;"),
                                 style = "background-color: #49a842; border-radius: 10px; width: 300px; padding: 20px;",
                                 height = "140px"),
                       column(width = 1)) # end third fluid row
                     
              ),
              
              # Start of column
              column(width = 1), # End of column
              
              # Column 2: Cumulative map
              box(width = 7,
                  leafletOutput(outputId = "cumulative_change_output") %>% 
                    withSpinner(color = "#05641C", type = 1, size = 1)),
              
              column(width = 1)
              
            ) # End fifth fluid row
            
    ), # END model tabItem
    
    # priority monitoring assessment tabItem ----
    tabItem(tabName = "assessment",
            
            fluidRow(
              column(width = 1),
              box(width = 10,
                  div(style = "font-size: 16px;", includeMarkdown("text/assessment.md"))
              ),
              column(width = 1)
            ),
            
            # second fluidRow: dropdown
            fluidRow(
              column(width = 1),  # left buffer
              
              column(width = 10,
                     pickerInput(
                       inputId = "species_priority_input",
                       label = "Select a Priority Level:",
                       choices = c("high", "moderate", "minimal", "low"))
              ),
              
              column(width = 1)  # right buffer
            ),
            
            # third fluidRow: species table
            fluidRow(
              column(width = 1),  # left buffer
              
              column(width = 10,
                     DTOutput("species_priority_output") %>%
                       withSpinner(color = "#05641C", type = 1, size = 1)
              ),
              
              column(width = 1)  # right buffer
            )
            
           
    
  ), # END priority monitoring assessment tabItem
  
  # data and limitations tabItem ----
  tabItem(tabName = "data",
          
          # first fluidRow
          fluidRow(
            
            # left buffer column
            column(width = 1),
            
            # info box
            box(width = 10,
                
                div(style = "font-size: 16px;", includeMarkdown("text/data-limitations.md"))
                
            ), # END info box
            
            # right buffer column
            column(width = 1)
            
          ) # END first fluidRow
          
  ), # END data and limitations tabItem
  
  # acknowledgements tabItem ----
  tabItem(tabName = "acknowledgments",
          
          # left buffer column
          column(width = 1),
          
          # acknowledgements
          column(width = 10,
                 
                 includeMarkdown("text/acknowledgements.md")
                 
          ), # END acknowledgements
          
          # right buffer column
          column(width = 1)
          
  ) # END acknowledgements tabItem
  
) # END tabItems

) # END dashboardBody

# combine all into dashboardPage ----
dashboardPage(header, sidebar, body)