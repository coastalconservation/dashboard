# dashboardHeader ----
header <- dashboardHeader(
  
  # title ----
  title = "Dangermond Rocky Shore Range Shifts Portal",
  titleWidth = 450
  
) # END dashboardHeader

# dashboardSidebar ---
sidebar <- dashboardSidebar(
  
  # sidebarMenu ----
  sidebarMenu(
    
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
  
  # tabItems ----
  tabItems(
    
    # home tabItem ----
    tabItem(tabName = "home",
            
            # left buffer column ----
            column(width = 1),
            
            # left-hand column ----
            column(width = 5,
                   
                   # first fluidRow ----
                   fluidRow(
                     
                     # welcome info box ----
                     box(width = NULL,
                         
                         includeMarkdown("text/welcome.md")
                         
                     ) # END welcome info box
                     
                   ), # END first fluidRow
                   
                   # second fluidRow ----
                   fluidRow(
                     
                     # images box ----
                     box(width = NULL,
                         
                         includeMarkdown("text/background.md")
                         
                     ) # END images box
                     
                   ) # END second fluidRow
                   
            ), # END left-hand column
            
            # center buffer column ----
            column(width = 1),
            
            # right-hand column ----
            column(width = 4,
                   
                   # first fluidRow ----
                   fluidRow(
                     
                     # carousel images box ----
                     box(width = NULL,
                         
                         # title ----
                         tags$h4("The Jack and Laura Dangermond Preserve",
                                 style = "text-align: center;"),
                         
                         # slickR carousel ----
                         slickROutput(outputId = "carousel_images_output",
                                      width = "100%",
                                      height = "250px"),
                         
                         "Idea: replace current images with pictures we take on our field trip!"
                         
                     ) # END carousel images box
                     
                   ), # END first fluidRow
                   
                   # second fluidRow ----
                   fluidRow(
                     
                     # team info box ----
                     box(width = NULL,
                         
                         includeMarkdown("text/team.md"),
                         div(tags$img(src = "hex.jpg", width = "150px"),
                             style = "text-align: center;")
                         
                     ) # END team info box
                     
                   ) # END second fluidRow
                   
            ), # END right-hand column
            
            # right buffer column ----
            column(width = 1)
            
    ), # END home tabItem
    
    # background tabItem ----
    tabItem(tabName = "background"), # END background tabItem
    
    # ranges tabItem ----
    tabItem(tabName = "ranges"), # END ranges tabItem
    
    # trends tabItem ----
    tabItem(tabName = "trends",
            
            # first fluidRow ----
            fluidRow(
              
              #left buffer column ----
              column(width = 1),
              
              # left-hand column ----
              column(width = 4,
                     
                     # slider box ----
                     box(width = NULL,
                         
                         # year sliderInput ----
                         sliderInput(inputId = "year_input",
                                     label = "Please Adjust the Time Scale:",
                                     min = min(longterm_data$year),
                                     max = max(longterm_data$year),
                                     value = c(min(longterm_data$year),
                                               max(longterm_data$year)),
                                     sep = "",
                                     step = 2)
                         
                     ), # END slider box
                     
                     # dropdown menu box ----
                     box(width = NULL,
                         
                         # species pickerInput ----
                         pickerInput(inputId = "species_input",
                                     label = "Please Select a Species:",
                                     choices = unique(longterm_data$species_name),
                                     selected = "Pisaster ochraceus",
                                     multiple = FALSE,
                                     options = pickerOptions(size = 7))
                         
                     ), # END dropdown menu box
                     
              ), # END left-hand column
              
              # right-hand column ----
              column(width = 6,
                     
                     # plot box ----
                     box(width = NULL,
                         
                         # trend plotOutput ----
                         plotOutput(outputId = "trend_plot_output") %>%
                           withSpinner(color = "black", type = 1, size = 1)
                         
                     ), # END plot box
                     
              ), # END left-hand column
              
              # right buffer column
              column(width = 1)
              
            ), # END first fluidRow
            
            # second fluidRow ----
            fluidRow(
              
              # left buffer column ----
              column(width = 1),
              
              # center column ----
              column(width = 10,
                     
                     # map box ----
                     box(width = NULL,
                         
                         leafletOutput(outputId = "presence_map_output") %>%
                           withSpinner(color = "black", type = 1, size = 1)
                         
                     ), # END map box
                     
              ), # END center column
              
              # right buffer column ----
              column(width = 1)
              
            ), # END second fluidRow
            
    ), # END trends tabItem
    
    # model tabItem ----
    tabItem(tabName = "model"), # END model tabItem
    
    # assessment tabItem ----
    tabItem(tabName = "assessment"), # END assessment tabItem
    
    # data tabItem ----
    tabItem(tabName = "data") # END data tabItem
    
  ) # END tabItems
  
) # END dashboardBody

# combine all into dashboardPage ----
dashboardPage(header, sidebar, body)