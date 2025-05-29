# dashboardHeader ----
header <- dashboardHeader(
  
  # title ----
  title = span(img(src = "TNCLogoPrimary_RGB_PNG.png", height = 80, style = "padding-top: 20px;")),
  titleWidth = 300,
  
  # navbar adjustments ----
  tags$li(class = "dropdown",
          tags$style(".main-header .logo {height: 100px;}"),
          tags$style(".sidebar-toggle {color: #05641C; font-size: 30px; padding-top: 10px !important;}"),
          tags$style(".skin-blue .main-header .navbar .sidebar-toggle {color: #05641C !important;}"),
          tags$div("California Ranges of Intertidal Species Portal (CRISP)",
                   style = "padding-top: 20px; padding-right: 225px; font-weight: bold; color: black; font-size: 32px; font-family: Merriweather;"))
  
) # END dashboardHeader

# dashboardSidebar ----
sidebar <- dashboardSidebar(
  
  # dashboard theme ----
  use_theme("dashboard-fresh-theme.css"),
  
  # sidebar adjustments ----
  tags$style(".left-side, .main-sidebar {padding-top: 100px; font-size: 14px; font-family: Barlow;}"),
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
  tags$head(tags$link(rel = "stylesheet", 
                      href = "https://fonts.googleapis.com/css2?family=Barlow:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap"),
            tags$link(rel = "stylesheet", 
                      href="https://fonts.googleapis.com/css2?family=Merriweather:ital,opsz,wght@0,18..144,300..900;1,18..144,300..900&display=swap")),
  
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
                  
                  tags$h1("Welcome",
                          style = "font-family: Barlow; font-weight: bold; color: #05641c; padding-bottom: 10px;"),
                  
                  div(style = "font-size: 18px; font-family: Merriweather", includeMarkdown("text/welcome.md"))
                  
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
              
              # image box
              box(width = 5,
                  
                  imageOutput("dangermond", click = "dangermond_click") %>%
                    withSpinner(color = "#05641C", type = 1, size = 1),
                  
                  uiOutput("zoom_dangermond"),
                  
                  div(style = "font-size: 18px; font-family: Merriweather;", "Figure 1. Insert Caption")
                  
                  ), # END kelley's box
              
              # image box
              box(width = 5,
                  
                  tags$h1("The Last Wild Coast",
                          style = "font-family: Barlow; font-weight: bold; color: #05641c; padding-bottom: 10px;"),
                  
                  div(style = "font-size: 18px; font-family: Merriweather", includeMarkdown("text/dangermond.md"))
                  
                  ), # END dangermond box
              
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
              
              # box
              box(width = 10,
                  
                  tags$h1("Kelly the Kellet's Whelk",
                          style = "font-family: Barlow; font-weight: bold; color: #05641c; padding-bottom: 10px;"),
                  
                  # fluidRow
                  fluidRow(
                    
                    # left-hand column
                    column(width = 8,
                           
                           div(style = "font-size: 18px; font-family: Merriweather", includeMarkdown("text/kelley.md"))
                           
                           ), # END left-hand column
                    
                    # right-hand column
                    column(width = 4,
                           
                           div(style = "text-align: center;",
                               img(src = "Whelk.webp", width = 300, height = 300))
                           
                           ) # END right-hand column
                    
                  ) # END fluidRow
                  
                  ), # END box
              
              # right buffer column
              column(width = 1)
              
            ), # END first fluidRow
            
            # second fluidRow
            fluidRow(
              
              # left buffer column
              column(width = 1),
              
              # biogeographic box
              box(width = 5,
                  
                  imageOutput("cal_currents", click = "currents_click") %>%
                    withSpinner(color = "#05641C", type = 1, size = 1),
                  
                  uiOutput("zoom_currents"),
                  
                  div(style = "font-size: 18px; font-family: Merriweather;", "Figure 2. Insert Caption")
                  
              ), # END biogeographic box
              
              # currents image box
              box(width = 5,
                  
                  tags$h1("Biogeographic Barrier",
                          style = "font-family: Barlow; font-weight: bold; color: #05641c; padding-bottom: 10px;"),
                  
                  div(style = "font-size: 18px; font-family: Merriweather;", includeMarkdown("text/biogeographic.md"))
                  
              ), # END currents image box
              
              # right buffer column
              column(width = 1)
              
            ), # END second fluidRow
            
            # third fluidRow
            fluidRow(
              
              # left buffer column
              column(width = 1),
              
              # range shift image box
              box(width = 5,
                  
                  tags$h1("Range Shift",
                          style = "font-family: Barlow; font-weight: bold; color: #05641c; padding-bottom: 10px;"),
                  
                  div(style = "font-size: 18px; font-family: Merriweather;", includeMarkdown("text/range-shift.md"))
                  
              ), # END range shift image box
              
              # range shift box
              box(width = 5,
                  
                  imageOutput("range_shift", click = "shift_click") %>%
                    withSpinner(color = "#05641C", type = 1, size = 1),
                  
                  uiOutput("zoom_shift"),
                  
                  div(style = "font-size: 18px; font-family: Merriweather;", "Figure 3. Insert Caption")
                  
              ), # END range shift box
              
              # right buffer column
              column(width = 1)
              
            ) # END third fluidRow
            
    ), # END background tabItem
    
    # range edges tabItem ----
    tabItem(tabName = "ranges",
            
            # first fluidRow
            fluidRow(
              
              # left buffer column
              column(width = 1),
              
              # info box
              box(width = 10,
                  
                  tags$h1("Range Edges",
                          style = "font-family: Barlow; font-weight: bold; color: #05641c; padding-bottom: 10px;"),
                  
                  div(style = "font-size: 18px; font-family: Merriweather;", includeMarkdown("text/range-edges.md"))
                  
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
                          style = "text-align: center; font-weight: bold; font-family: Barlow; padding-bottom: 10px;"),
                  
                  actionButton(inputId = "refresh_northern_map", 
                               label = "Refresh Map",
                               icon = icon("rotate")),
                  
                  leafletOutput(outputId = "northern_range_output",
                                width = "100%") %>%
                    withSpinner(color = "#05641C", type = 1, size = 1)
                  
              ), # END northern range edge map box
              
              # northern range edge DT box
              box(width = 5,
                  
                  tags$h4("Rocky Intertidal Species with Northern Range Edges at",
                          style = "text-align: center; font-weight: bold; font-family: Barlow;"),
                  
                  tags$h4(textOutput("table_header_north"),
                          style = "text-align: center; font-weight: bold; font-family: Barlow; color:#05641c;"),
                  
                  div(style = "font-family: Merriweather;", DTOutput("northern_edge_table") %>%
                    withSpinner(color = "#05641C", type = 1, size = 1))
                  
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
                          style = "text-align: center; font-weight: bold; font-family: Barlow; padding-bottom: 15px;"),
                  
                  actionButton(inputId = "refresh_southern_map", 
                               label = "Refresh Map",
                               icon = icon("rotate")),
                  
                  leafletOutput(outputId = "southern_range_output",
                                width = "100%") %>%
                    withSpinner(color = "#05641C", type = 1, size = 1)
                  
              ), # END southern range edge map box
              
              # southern range edge DT box
              box(width = 5,
                  
                  tags$h4("Rocky Intertidal Species with Southern Range Edges at",
                          style = "text-align: center; font-weight: bold; font-family: Barlow;"),
                  
                  tags$h4(textOutput("table_header_south"),
                          style = "text-align: center; font-weight: bold; color:#05641c; font-family: Barlow;"),
                  
                  div(style = "font-family: Merriweather;", DTOutput("southern_edge_table") %>%
                    withSpinner(color = "#05641C", type = 1, size = 1))
                  
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
              box(width = 9,
                  
                  # Title for change map
                  tags$h4("Map of Habitat Change From 2025 to 2050",
                          style = "text-align: center; font-weight: bold"),
                  
                  # Leaflet output
                  leafletOutput(outputId = "change_raster_output") %>% 
                    withSpinner(color = "#05641C", type = 1, size = 1)
              ) # End leaflet box for change raster
              
            ), # End second fluid row
            
            # Third fluid row
            fluidRow(
              
              # Column 1 with current raster map
              box(width = 6,
                  
                  # Title for current species map
                  tags$h4("Map of Species Habitat In 2025",
                          style = "text-align: center; font-weight: bold"),
                  # Leaflet output
                  leafletOutput(outputId = "current_raster_output") %>% 
                    withSpinner(color = "#05641C", type = 1, size = 1)
              ), # End of column 1 with current raster habitat
              
              
              # Column 2: projected habitat map
              box(width = 6,
                  
                  # Title for Project Species map
                  tags$h4("Map of Forecasted Species Habitat For 2050",
                          style = "text-align: center; font-weight: bold"),
                  
                  leafletOutput(outputId = "projected_raster_output") %>% 
                    withSpinner(color = "#05641C", type = 1, size = 1)
              ) # End of column 2: projected habitat raster
              
            ), # End third fluid row
            
            # fourth fluid row
            fluidRow(
              
              # Column 1
              box(width = 12,
                  includeMarkdown("text/cumulative_difference_intro.md")) # end column 1
            ), # End fourth fluid row
            
            # Fifth fluid row
            fluidRow(
              
              column(width = 1),
               # Column 1: buffer column
              column(width = 3,
                     
                     fluidRow(   # end column 1
              
              value_box(title = span("Northern Range Edges",
                                     style = "font-size: 22px; font-weight: bold;"),
                        value = span("23 species",
                                     style = "font-size: 20px;"),
                        showcase = span(bs_icon("arrow-up"),
                                        style = "color: black;"),
                        p("11 in Northern Point Conception", style = "font-size: 15px;"),
                        p("12 in Southern Point Conception", style = "font-size: 15px;"),
                        style = "background-color: #49a842; border-radius: 10px; width: 300px; padding: 20px;",
                        height = "140px")
              ), # end first fluid row
              fluidRow(
                value_box(title = span("Northern Range Edges", style = "font-size: 22px; font-weight: bold;"),
                                 value = span("23 species",
                                              style = "font-size: 20px;"),
                                 showcase = span(bs_icon("arrow-up"),
                                                 style = "color: black;"),
                                 p("11 in Northern Point Conception", style = "font-size: 15px;"),
                                 p("12 in Southern Point Conception", style = "font-size: 15px;"),
                                 style = "background-color: #49a842; border-radius: 10px; width: 300px; padding: 20px;",
                                 height = "140px")
                
              ), # end second fluid row
              
              fluidRow(
                value_box(title = span("Northern Range Edges", style = "font-size: 22px; font-weight: bold;"),
                                 value = span("23 species", 
                                              style = "font-size: 20px;"),
                                 showcase = span(bs_icon("arrow-up"),
                                                 style = "color: black;"),
                                 p("11 in Northern Point Conception", style = "font-size: 15px;"),
                                 p("12 in Southern Point Conception", style = "font-size: 15px;"),
                                 style = "background-color: #49a842; border-radius: 10px; width: 300px; padding: 20px;",
                                 height = "140px"),
                column(width = 1)) # end third fluid row
              
              ),
              
              # Start of column
              column(width = 1), # End of column
              
              # Column 2: Cumulative map
              box(width = 8,
                  leafletOutput(outputId = "cumulative_change_output") %>% 
                    withSpinner(color = "#05641C", type = 1, size = 1)),
    
            ) # End fifth fluid row
            
    ), # END model tabItem
    
    # priority monitoring assessment tabItem ----
    tabItem(tabName = "assessment",
            
            # first fluidRow
            fluidRow(
              
              # left buffer column
              column(width = 1),
              
              # info box
              box(width = 10,
                  
                  div(style = "font-size: 16px;", includeMarkdown("text/assessment.md"))
                  
              ), # END info box
              
              # right buffer column
              column(width = 1)
              
            ) # END first fluidRow
            
    ), # END priority monitoring assessment tabItem
    
    # data and limitations tabItem ----
    tabItem(tabName = "data",
            
            # first fluidRow
            fluidRow(
              
              # left buffer column
              column(width = 1),
              
               # title column
              column(width = 10,
                     
                     tags$h1("Data",
                             style = "font-family: Barlow; font-weight: bold; color: #05641c; padding-bottom: 10px;")
                     
                     ), # END title column
              
              # right buffer column
              column(width = 1)
              
            ), # END first fluidRow
            
            # second fluidRow
            fluidRow(
              
              # left buffer column
              column(width = 1),
              
              # MARINE info
              column(width = 10,
                     
                     # fluidRow
                     fluidRow(
                       
                       # left-hand column
                       column(width = 8,
                              
                              div(style = "font-family: Merriweather; font-size: 18px;  padding-bottom: 25px;", includeMarkdown("text/MARINe.md"))
                              
                       ), # END left-hand column
                       
                       # right-hand column
                       column(width = 4,
                              
                              div(style = "text-align: center; padding-bottom: 25px;",
                                  img(src = "logos/MARINe.png", width = 300, height = 325))
                              
                       ) # END right-hand column
                       
                     ) # END fluidRow
                     
              ), # END MARINe info
              
              # right buffer column
              column(width = 1)
              
            ), # END second fluidRow
            
            # third fluidRow
            fluidRow(
              
              # left buffer column
              column(width = 1),
              
              # line column
              column(width = 10,
                     
                     tags$hr(style = "border-top: 3px solid; color: #eae8f5; padding-bottom: 25px;")
                     
                     ), # END line column
              
              # right buffer column
              column(width = 1)
              
              ), # END third fluidRow
            
            # fourth fluidRow
            fluidRow(
              
              # left buffer column
              column(width = 1),
              
              # Bio-ORACLE info
              column(width = 10,
                     
                     # fluidRow
                     fluidRow(
                       
                       # left-hand column
                       column(width = 8,
                              
                              div(style = "font-family: Merriweather; font-size: 18px; padding-bottom: 25px;",  includeMarkdown("text/Bio-ORACLE.md"))
                              
                       ), # END left-hand column
                       
                       # right-hand column
                       column(width = 4,
                              
                              div(style = "text-align: center; padding-bottom: 25px;",
                                  img(src = "logos/Bio-ORACLE.png", width = 300, height = 325))
                              
                       ) # END right-hand column
                       
                     ) # END fluidRow
                     
              ), # END Bio-ORACLE info
              
              # right buffer column
              column(width = 1)
              
            ), # END fourth fluidRow
            
            # fifth fluidRow
            fluidRow(
              
              # left buffer column
              column(width = 1),
              
              # line column
              column(width = 10,
                     
                     tags$hr(style = "border-top: 3px solid; color: #eae8f5; padding-bottom: 25px;")
                     
              ), # END line column
              
              # right buffer column
              column(width = 1)
              
            ), # END fifth fluidRow
            
            # sixth fluidRow
            fluidRow(
              
              # left buffer column
              column(width = 1),
              
              # title column
              column(width = 10,
                     
                     tags$h1("Limitations",
                             style = "font-family: Barlow; font-weight: bold; color: #05641c; padding-bottom: 10px;")
                     
              ), # END title column
              
              # right buffer column
              column(width = 1)
              
            ), # END sixth fluidRow
            
            # seventh fluidRow
            fluidRow(
              
              # left buffer column
              column(width = 1),
              
              # limitations info
              column(width = 10,
                     
                     div(style = "font-family: Merriweather; font-size: 18px; padding-bottom: 25px;", includeMarkdown("text/limitations.md"))
                     
                     ), # END limitations info
              
              # right buffer column
              column(width = 1)
              
            ) # END seventh fluidRow
            
    ), # END data and limitations tabItem
    
    # acknowledgements tabItem ----
    tabItem(tabName = "acknowledgments",
            
            # first fluidRow
            fluidRow(
              
              # left buffer column
              column(width = 1),
              
              column(width = 10,
                     
                     tags$h1("Acknowledgements",
                             style = "font-family: Barlow; font-weight: bold; color: #05641c; padding-bottom: 10px;")
                     
                     ),
              
              # right buffer column
              column(width = 1)
              
            ), # END first fluidRow
            
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