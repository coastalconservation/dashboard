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

# dashboardSidebar ---
sidebar <- dashboardSidebar(
  
  # dashboard theme ----
  use_theme("dashboard-fresh-theme.css"),
  
  # sidebar adjustments ----
  tags$style(".left-side, .main-sidebar {padding-top: 100px; font-size: 16px;}"),
  width = 300,
  
  # sidebarMenu ----
  sidebarMenu(
    
    # menuItems ----
    menuItem(text = "Home", tabName = "home", icon = icon("house")),
    menuItem(text = "Background", tabName = "background", icon = icon("circle-info")),
    menuItem(text = "Range Edges", tabName = "ranges", icon = icon("location-dot")),
    menuItem(text = "Abundance Trends", tabName = "trends", icon = icon("arrow-trend-up")),
    menuItem(text = "Projected Shifts", tabName = "model", icon = icon("project-diagram")),
    menuItem(text = "Cumulative Assessment", tabName = "assessment", icon = icon("list-check")),
    menuItem(text = "Data", tabName = "data", icon = icon("database"))
    
  ) # END sidebarMenu
  
) # END dashboardSidebar

# dashboardBody ----
body <- dashboardBody(
  
  tags$style(".content-wrapper, .right-side {padding-top: 50px;}"),
  
  # tabItems ----
  tabItems(
    
    # home tabItem ----
    tabItem(tabName = "home",
            
            # first fluidRow ----
            fluidRow(
              
              # left buffer column ----
              column(width = 1),
              
              # welcome box ----
              box(width = 10,
                  
                  includeMarkdown("text/welcome.md")
                  
              ), # END welcome box
              
              # right buffer column ----
              column(width = 1)
              
            ), # END first fluidRow
            
            # second fluidRow ----
            fluidRow(
              
              # left buffer column ----
              column(width = 1),
              
              # dangermond box
              box(width = 6,
                  
                  includeMarkdown("text/wild-coast.md")
                  
              ), # END dangermond box
              
              # carousel images box ----
              box(width = 4,
                  
                  # slickR carousel ----
                  slickROutput(outputId = "carousel_images_output",
                               width = "100%",
                               height = "400px")
                  
              ), # END carousel images box
              
              # right buffer column ----
              column(width = 1)
              
            ), # END second fluidRow
            
    ), # END home tabItem
    
    # background tabItem ----
    tabItem(tabName = "background",
            
            # first fluidRow ----
            fluidRow(
              
              # left buffer column ----
              column(width = 1),
              
              # biogeographic box ----
              box(width = 6,
                  
                  includeMarkdown("text/biogeographic.md")
                  
              ), # END biogeographic box
              
              # image box ----
              box(width = 4,
                  
                  img(src = "cal-currents.jpg",
                      width = "375px",
                      height = "375px"),
                  
                  "Figure 1. The eddy..."
                  
              ), # END image box
              
              # right buffer column ----
              column(width = 1)
              
            ), # END first fluidRow
            
            # second fluidRow ----
            fluidRow(
              
              # left buffer column ----
              column(width = 1),
              
              # image box ----
              box(width = 4,
                  
                  img(src = "dangermond.png",
                      width = "100%",
                      height = "300px"),
                  
                  "Figure 2. Image that will be referred to for range shifts"
                  
              ), # END text box
              
              # text box ----
              box(width = 6,
                  
                  includeMarkdown("text/range-shift.md")
                  
              ), # END image box
              
              # right buffer column ----
              column(width = 1)
              
            ) # END second fluidRow
            
    ), # END background tabItem
    
    # ranges tabItem ----
    tabItem(tabName = "ranges",
            
            # first fluidRow ----
            fluidRow(
              
              # left buffer column ----
              column(width = 1),
              
              # info box ----
              box(width = 10,
                  
                  includeMarkdown("text/range-edges.md")
                  
              ), # END info box
              
              # right buffer column ----
              column(width = 1)
              
            ), # END first fluidRow
            
            # second fluidRow ----
            fluidRow(
              
              # left buffer column ----
              column(width = 1),
              
              # northern box ----
              box(width = 5,
                  
                  tags$h4("Northern Range Edges of Rocky Intertidal Species in CA",
                          style = "text-align: center; font-weight: bold"),
                  
                  actionButton(inputId = "refresh_northern_map", 
                               label = "Refresh Map",
                               icon = icon("rotate")),
                  
                  leafletOutput(outputId = "northern_range_output",
                                width = "100%") %>%
                    withSpinner(color = "#05641C", type = 1, size = 1)
                  
              ), # END northern box
              
              # southern box ----
              box(width = 5,
                  
                  tags$h4("Southern Range Edges of Rocky Intertidal Species in CA",
                          style = "text-align: center; font-weight: bold"),
                  
                  actionButton(inputId = "refresh_southern_map", 
                               label = "Refresh Map",
                               icon = icon("rotate")),
                  
                  leafletOutput(outputId = "southern_range_output",
                                width = "100%") %>%
                    withSpinner(color = "#05641C", type = 1, size = 1)
                  
              ), # END southern box
              
              # right buffer column ----
              column(width = 1)
              
            ), # END second fluidRow
            
            # third fluidRow ----
            fluidRow(
              
              # left buffer column ----
              column(width = 1),
              
              # DT box ----
              box(width = 10,
                  
                  # fluidRow ----
                  fluidRow(
                    
                    # left-hand column ----
                    column(width = 3,
                           
                           radioGroupButtons(inputId = "DT_input",
                                             label = "Choose a range edge option:",
                                             choices = c("Northern", "Southern"),
                                             selected = "Northern",
                                             size = "normal"),
                           
                           pickerInput(inputId = "segment_input",
                                       label = "Select a coastal segment:",
                                       choices = unique(range_list$segment_name),
                                       selected = unique(range_list$segment_name)[7],
                                       multiple = FALSE,
                                       options = pickerOptions(dropupAuto = FALSE,
                                                               size = 10))
                           
                    ), # END left-hand column
                    
                    # right-hand column ----
                    column(width = 9,
                           
                           DTOutput(outputId = "DT_output") %>%
                             withSpinner(color = "black", type = 1, size = 1)
                           
                    ) # END right-hand column
                    
                  ) # END fluidRow
                  
              ), # END DT box
              
              # right buffer column ----
              column(width = 1)
              
            ), # END third fluidRow
            
            # fourth fluidRow
            fluidRow(
              
              tags$h4("Rocky Intertidal Species with Range Edges Near Point Conception",
                      style = "text-align: center; font-weight: bold"),
              
              # left buffer column ----
              column(width = 1),
              
              # box ----
              box(width = 10,
                  
                  # fluidRow
                  fluidRow(
                    
                    # left-hand column ----
                    column(width = 6,
                           
                           # first fluidRow ----
                           fluidRow(
                             
                             column(width = 2),
                             
                             column(width = 8,
                                    
                                    valueBoxOutput("northern_output",
                                                   width = NULL)
                                    
                             ),
                             
                             column(width = 2)
                             
                           ), # END first fluidRow
                           
                           # second fluidRow ----
                           fluidRow(
                             
                             column(width = 2),
                             
                             column(width = 8,
                                    
                                    valueBoxOutput("southern_output",
                                                   width = NULL)
                                    
                             ),
                             
                             column(width = 2)
                             
                           ), # END second fluidRow
                           
                    ), # END left-hand column
                    
                    # right-hand column ---
                    column(width = 6,
                           
                           img(src = "bob.jpeg",
                               width = "100%",
                               height = "400px")
                           
                    ) # END right-hand column
                    
                  ), # END fluidRow
                  
              ), # END box
              
              # right buffer column ----
              column(width = 1)
              
            ) # END fourth fluidRow
            
    ), # END ranges tabItem
    
    # trends tabItem ----
    tabItem(tabName = "trends",
            
            # first fluidRow ----
            fluidRow(
              
              # left buffer column ----
              column(width = 1),
              
              # info box ----
              box(width = 10,
                  
                  includeMarkdown("text/range-edges.md")
                  
              ), # END info box
              
              # right buffer column ----
              column(width = 1)
              
            ), # END first fluidRow
            
            # second fluidRow ----
            fluidRow(
              
              # left buffer column ----
              column(width = 1),
              
              # picker column ----
              column(width = 3,
                     
                     pickerInput(inputId = "artist_input", 
                                 label = "Choose an artist image:",
                                 choices = c("addison.jpeg", "badbunny.jpeg", "billie.jpeg", "britney.jpeg", 
                                             "camila.jpeg", "caroline.jpeg", "charlixcx.jpeg", "clairo.jpeg", 
                                             "fka.jpeg", "haim.jpeg", "isoxo.jpeg", "lana.jpeg", 
                                             "le sserafim.jpeg", "lorde.jpeg", "rebecca.jpeg", 
                                             "sophie.jpeg", "troye.jpeg", "yeji.jpeg"),
                                 multiple = FALSE,
                                 options = pickerOptions(dropupAuto = FALSE,
                                                         size = 3))
                     
              ), # END picker column
              
              # right buffer column ----
              column(width = 8),
              
            ), # END second fluidRow
            
            # third fluidRow ----
            fluidRow(
              
              # left buffer column ----
              column(width = 1),
              
              # northern box ----
              box(width = 5,
                  
                  uiOutput("image_a") %>%
                    withSpinner(color = "black", type = 1, size = 1)
                  
              ), # END northern box
              
              # southern box ----
              box(width = 5,
                  
                  uiOutput("image_b") %>%
                    withSpinner(color = "black", type = 1, size = 1)
                  
              ), # END southern box
              
              # right buffer column ----
              column(width = 1)
              
            ), # END third fluidRow
            
    ), # END trends tabItem
    
    # model tabItem ----
    tabItem(tabName = "model",
            
            # First fluid row
            fluidRow(
              
              # Left buffer column
              
              # Info box
              box(width = 8,
                  includeMarkdown("text/range_change_maps.md")), # End species rasters info box
      
            
            ), # End first fluid row
            
            # Second fluid row 
            fluidRow(
              
              # First column
              # Column 1
              column(width = 2,
                     
                     pickerInput(inputId = "selected_species", 
                                 label = "Choose a Species:",
                                 choices = species_choices,
                                 multiple = FALSE,
                                 options = pickerOptions(dropupAuto = FALSE,
                                                         size = 3)
                     )
              ), # end of column 1
              
              # Leaflet box
              box( width = 8,
                   # Leaflet output
                   leafletOutput(outputId = "change_raster_output")
              ) # End leaflet box
              
              
              
            ) # End second fluid row
            
            
            ), # END model tabItem
    
    # assessment tabItem ----
    tabItem(tabName = "assessment"), # END assessment tabItem
    
    # data tabItem ----
    tabItem(tabName = "data") # END data tabItem
    
  ) # END tabItems
  
) # END dashboardBody

# combine all into dashboardPage ----
dashboardPage(header, sidebar, body)