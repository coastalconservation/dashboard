# dashboardHeader ----
header <- dashboardHeader(
  
  # title ----
  title = span(img(src = "logos/TNCLogoPrimary_RGB_PNG.png", height = 80, style = "padding-top: 20px;")),
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
  tags$head(tags$link(rel = "stylesheet", 
                      href = "https://fonts.googleapis.com/css2?family=Barlow:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap"),
            tags$link(rel = "stylesheet", 
                      href="https://fonts.googleapis.com/css2?family=Merriweather:ital,opsz,wght@0,18..144,300..900;1,18..144,300..900&display=swap")),
  
  # sidebar adjustments ----
  tags$style(".left-side, .main-sidebar {padding-top: 100px; font-size: 15px; font-family: Barlow;}"),
  width = 300,
  
  # sidebarMenu ----
  sidebarMenu(
    
    # menuItems
    menuItem(text = "Home", tabName = "home"),
    menuItem(text = "Background", tabName = "background"),
    menuItem(text = "Range Edges", tabName = "ranges"),
    menuItem(text = "Contemporary Range Shifts", tabName = "trends"),
    menuItem(text = "Projected Shifts", tabName = "model"),
    menuItem(text = "Priority Monitoring Assessment", tabName = "assessment"),
    menuItem(text = "Data and Limitations", tabName = "data"),
    menuItem(text = "Acknowledgements", tabName = "acknowledgments")
    
  ), # END sidebarMenu
  
  div(style = "position: absolute; bottom: 20px; width: 100%; text-align: center; font-size: 40px;",
      HTML('<a href="https://github.com/coastalconservation" target="_blank" title="GitHub">
        <i class="fa-brands fa-github-alt"></i> </a>') )
  
) # END dashboardSidebar

# dashboardBody ----
body <- dashboardBody(
  
  # body adjustments ----
  tags$style(".content-wrapper, .right-side {padding-top: 25px;}"),
  tags$script(HTML("$(document).on('click', '#read_more_shift', function() {$('#more_text_shift').toggle();});")),
  tags$script(HTML("$(document).on('click', '#read_more_biogeographic', function() {$('#more_text_biogeographic').toggle();});")),
  tags$script(HTML("$(document).on('click', '#read_more_dangermond', function() {$('#more_text_dangermond').toggle();});")),
  tags$script(HTML("$(document).on('click', '#read_more_kellet', function() {$('#more_text_kellet').toggle();});")),
  tags$style(HTML(".bootstrap-select .dropdown-toggle {font-size: 11px; font-family: Merriweather;}")),
  tags$style(HTML(".bootstrap-select .dropdown-menu.inner > li > a {font-size: 11px; font-family: Merriweather;}")),
  
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
                  style = "height: 500px;",
                  
                  # title
                  tags$h1("Welcome",
                          style = "font-family: Barlow; font-weight: bold; color: #05641c; padding-bottom: 10px;"),
                  
                  # welcome markdown
                  div(style = "font-size: 18px; font-family: Merriweather", 
                      includeMarkdown("text/home/welcome.md"))
                  
              ), # END welcomed box
              
              # slickR box
              box(width = 5,
                  style = "height: 500px;",
                  
                  tags$h3("The Jack and Laura Dangermond Preserve",
                          style = "font-family: Barlow; font-weight: bold; padding-bottom: 10px;"),
                  
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
              
              # dangermond image box
              box(width = 5,
                  style = "height: 450px;",
                  
                  # dangermond image
                  imageOutput("dangermond", click = "dangermond_click") %>%
                    withSpinner(color = "#05641C", type = 1, size = 1),
                  
                  # zoom output
                  uiOutput("zoom_dangermond"),
                  
                  # figure caption
                  div(style = "font-size: 16px; font-family: Merriweather; text-align: center; padding-top 10px;",
                      "Photo by The Nature Conservancy")
                  
              ), # END dangermond image box
              
              # dangermond box
              box(width = 5,
                  style = "height: 450px; overflow: hidden;",
                  
                  # container
                  div(style = "font-size: 18px; font-family: Merriweather; height: 100%; display: flex; flex-direction: column;",
                      
                      # title
                      tags$h1("The Last Wild Coast",
                              style = "font-family: Barlow; font-weight: bold; color: #05641c; padding-bottom: 10px;"),
                      
                      # intro
                      p("The Jack and Laura Dangermond Preserve, located in Lompoc, CA, is an important site for conservation."),
                      
                      # read more
                      actionLink("read_more_dangermond", "Read More", style = "color: #05641c; font-weight: bold; margin-bottom: 10px;"),
                      
                      # markdown container
                      div(id = "more_text_dangermond",
                          style = "display: none; overflow-y: auto; flex-grow: 1; padding-right: 10px;",
                          includeMarkdown("text/home/dangermond.md"))
                      
                  ) # END container
                  
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
              
              # biogeographic box
              box(width = 5,
                  style = "height: 500px; overflow: hidden;",
                  
                  # container
                  div(style = "font-size: 18px; font-family: Merriweather; height: 100%; display: flex; flex-direction: column;",
                      
                      # title
                      tags$h1("Biogeographic Barrier",
                              style = "font-family: Barlow; font-weight: bold; color: #05641c; padding-bottom: 10px;"),
                      
                      # intro
                      p("Point Conception is a significant biogeographic barrier for many rocky intertidal species because of its unique topography and its location at the intersection of two ocean currents."),
                      
                      # read more
                      actionLink("read_more_biogeographic", "Read More", style = "color: #05641c; font-weight: bold; margin-bottom: 10px;"),
                      
                      # markdown container
                      div(id = "more_text_biogeographic",
                          style = "display: none; overflow-y: auto; flex-grow: 1; padding-right: 10px;",
                          includeMarkdown("text/background/biogeographic.md"))
                      
                  ) # END container
                  
              ), # END biogeogeaphic box
              
              # currents image box
              box(width = 5,
                  style = "height: 500px;",
                  
                  # image output
                  imageOutput("cal_currents", click = "currents_click") %>%
                    withSpinner(color = "#05641C", type = 1, size = 1),
                  
                  # zoom output
                  uiOutput("zoom_currents"),
                  
                  # figure caption
                  div(style = "font-size: 16px; font-family: Merriweather; padding-top: 10px;",
                      HTML("<strong>Figure 1.</strong> Eddy formation at the biogeographic break, resulting from the convergence of cooler northern currents and warmer southern counter currents."))
                  
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
                  style = "height: 500px;",
                  
                  # range shift image
                  imageOutput("range_shift", click = "shift_click") %>%
                    withSpinner(color = "#05641C", type = 1, size = 1),
                  
                  # zoom output
                  uiOutput("zoom_shift"),
                  
                  # figure caption
                  div(style = "font-size: 16px; font-family: Merriweather; padding-top: 10px;",
                      HTML("<strong>Figure 2.</strong> An illustration depicting a potential habitat range shift for a species successfully adapting to environmental changes."))
                  
              ), # END range shift image box
              
              # range shift box
              box(width = 5,
                  style = "height: 500px; overflow: hidden;",
                  
                  # container
                  div(style = "font-size: 18px; font-family: Merriweather; height: 100%; display: flex; flex-direction: column;",
                      
                      # title
                      tags$h1("Range Shift",
                              style = "font-family: Barlow; font-weight: bold; color: #05641c; padding-bottom: 10px;"),
                      
                      # intro
                      p("Many rocky intertidal species may respond to changing environmental conditions by shifting their geographic range to areas with more suitable habitats."),
                      
                      # read more
                      actionLink("read_more_shift", "Read More", style = "color: #05641c; font-weight: bold; margin-bottom: 10px;"),
                      
                      # markdown container
                      div(id = "more_text_shift",
                          style = "display: none; overflow-y: auto; flex-grow: 1; padding-right: 10px;",
                          includeMarkdown("text/background/range-shift.md"))
                      
                  ) # END container
                  
              ), # END range shift box
              
              # right buffer column
              column(width = 1)
              
            ), # END second fluidRow
            
            # third fluidRow
            fluidRow(
              
              # left buffer column
              column(width = 1),
              
              # kellet's whelk box
              box(width = 5,
                  style = "height: 455px; overflow: hidden;",
                  
                  # container
                  div(style = "font-size: 18px; font-family: Merriweather; height: 100%; display: flex; flex-direction: column;",
                      
                      # title
                      tags$h1("Real-World Example",
                              style = "font-family: Barlow; font-weight: bold; color: #05641c; padding-bottom: 10px;"),
                      
                      # intro
                      p(HTML("One striking example of a climate-driven range shift is the northward expansion of <i>Kelletia kelletii</i>, commonly known as Kellet’s Whelk, a large marine snail native to Southern California.")),
                      
                      # read more
                      actionLink("read_more_kellet", "Read More", style = "color: #05641c; font-weight: bold; margin-bottom: 10px;"),
                      
                      # markdown container
                      div(id = "more_text_kellet",
                          style = "display: none; overflow-y: auto; flex-grow: 1; padding-right: 10px;",
                          includeMarkdown("text/background/kellet.md"))
                      
                  ) # END container
                  
              ), # END kellet's whelk box
              
              # image box
              box(width = 5,
                  style = "height: 455px;",
                  
                  # image output
                  imageOutput("kellet", click = "kellet_click") %>%
                    withSpinner(color = "#05641C", type = 1, size = 1),
                  
                  # zoom output
                  uiOutput("zoom_kellet"),
                  
                  # figure caption
                  div(style = "font-size: 16px; font-family: Merriweather; text-align: center; padding-top: 10px;",
                      HTML("Photo of Kellet's Whelk (<i>Kelletia kelletii</i>) by iNaturalist"))
                  
              ), # END image box
              
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
              
              # info column
              column(width = 10,
                     
                     # title
                     tags$h1("Range Edges",
                             style = "font-family: Barlow; font-weight: bold; color: #05641c; padding-bottom: 10px;"),
                     
                     # markdown
                     div(style = "font-size: 18px; font-family: Merriweather;", 
                         includeMarkdown("text/analyses/range-edges.md"))
                     
              ), # END info column
              
              # right buffer column
              column(width = 1)
              
            ), # END first fluidRow
            
            # second fluidRow
            fluidRow(
              
              # left buffer column
              column(width = 1),
              
              # line column
              column(width = 10,
                     
                     # line
                     tags$hr(style = "border-top: 3px solid; color: #eae8f5; padding-top: 10px; padding-bottom: 10px;")
                     
              ), # END line column
              
              # right buffer column
              column(width = 1)
              
            ), # END second fluidRow
            
            # third fluidRow
            fluidRow(
              
              # left buffer column
              column(width = 1),
              
              # northern range edge map box
              box(width = 5,
                  style = "height: 525px;",
                  
                  # title
                  tags$h4("Northern Range Edges of Rocky Intertidal Species in CA",
                          style = "text-align: center; font-weight: bold; font-family: Barlow; padding-bottom: 10px;"),
                  
                  # row
                  div(style = "display: flex; justify-content: space-between; align-items: center; padding-bottom: 10px;",
                      
                      # instructions
                      div(style = "font-family: Merriweather; font-size: 14px;",
                          "Click on a coastline segment:"),
                      
                      # refresh button
                      actionButton(inputId = "refresh_northern_map", 
                                   label = "Refresh Map",
                                   icon = icon("rotate"))),
                  
                  # northern range edge map
                  leafletOutput(outputId = "northern_range_output",
                                width = "100%") %>%
                    withSpinner(color = "#05641C", type = 1, size = 1)
                  
              ), # END northern range edge map box
              
              # northern range edge DT box
              box(width = 5,
                  style = "height: 525px;",
                  
                  # title
                  tags$h4("Rocky Intertidal Species with Northern Range Edges at",
                          style = "text-align: center; font-weight: bold; font-family: Barlow;"),
                  
                  # dynamic title
                  tags$h4(textOutput("table_header_north"),
                          style = "text-align: center; font-weight: bold; font-family: Barlow; color:#05641c;"),
                  
                  # northern range edge DT
                  div(style = "font-family: Merriweather;", DTOutput("northern_edge_table") %>%
                        withSpinner(color = "#05641C", type = 1, size = 1))
                  
              ), # END northern range edge DT box
              
              # right buffer column
              column(width = 1)
              
            ), # END third fluidRow
            
            # fourth fluidRow
            fluidRow(style = "padding-bottom: 20px;",
                     
                     # left buffer column
                     column(width = 1),
                     
                     # southern range edge map box
                     box(width = 5,
                         style = "height: 525px;",
                         
                         # title
                         tags$h4("Southern Range Edges of Rocky Intertidal Species in CA",
                                 style = "text-align: center; font-weight: bold; font-family: Barlow; padding-bottom: 10px;"),
                         
                         # row
                         div(style = "display: flex; justify-content: space-between; align-items: center; padding-bottom: 10px;",
                             
                             # instructions
                             div(style = "font-family: Merriweather; font-size: 14px;",
                                 "Click on a coastline segment:"),
                             
                             # refresh button
                             actionButton(inputId = "refresh_southern_map", 
                                          label = "Refresh Map",
                                          icon = icon("rotate"))),
                         
                         # southern range edge map
                         leafletOutput(outputId = "southern_range_output",
                                       width = "100%") %>%
                           withSpinner(color = "#05641C", type = 1, size = 1)
                         
                     ), # END southern range edge map box
                     
                     # southern range edge DT box
                     box(width = 5,
                         style = "height: 525px;",
                         
                         # title
                         tags$h4("Rocky Intertidal Species with Southern Range Edges at",
                                 style = "text-align: center; font-weight: bold; font-family: Barlow;"),
                         
                         # dynamic title
                         tags$h4(textOutput("table_header_south"),
                                 style = "text-align: center; font-weight: bold; color:#05641c; font-family: Barlow;"),
                         
                         # southern range edge DT
                         div(style = "font-family: Merriweather;", DTOutput("southern_edge_table") %>%
                               withSpinner(color = "#05641C", type = 1, size = 1))
                         
                     ), # END southern range edge DT box
                     
                     # right buffer column
                     column(width = 1)
                     
            ), # END fourth fluidRow
            
            # fifth fluidRow
            fluidRow(style = "background-color: #05641c; padding-bottom: 50px;",
                     
                     # left buffer column
                     column(width = 1),
                     
                     # stats column
                     column(width = 10,
                            
                            # fluidRow
                            fluidRow(
                              
                              # title
                              tags$h2("Rocky Intertidal Species with Range Edges Near Point Conception",
                                      style = "font-family: Barlow; font-weight: bold; color: #ffffff; text-align: center; padding-bottom: 10px;"),
                              
                              # left-hand column
                              column(width = 6,
                                     
                                     # first fluidRow
                                     fluidRow(
                                       
                                       # value box column
                                       column(width = 12,
                                              
                                              #  northern range edges value box
                                              value_box(title = span("Northern Range Edges",
                                                                     style = "font-family: Barlow; font-size: 22px; font-weight: bold; color: #ffffff;"),
                                                        value = span("23 taxa",
                                                                     style = "font-family: Barlow; font-size: 20px; font-weight: bold; color: #ffc700;"),
                                                        showcase = span(bs_icon("arrow-up"),
                                                                        style = "color: #ffc700;"),
                                                        p("11 in Northern Point Conception", 
                                                          style = "font-family: Merriweather; font-size: 18px; color: #ffffff;"),
                                                        p("12 in Southern Point Conception", 
                                                          style = "font-family: Merriweather; font-size: 18px;color: #ffffff;"),
                                                        style = "background-color: #05641c;",
                                                        height = "140px")
                                              
                                       ), # END value box column
                                       
                                     ), # END first fluidRow
                                     
                                     # second fluidRow
                                     fluidRow(
                                       
                                       # value box column
                                       column(width = 12,
                                              
                                              # southern range edges value box
                                              value_box(title = span("Southern Range Edges",
                                                                     style = "font-family: Barlow; font-size: 22px; font-weight: bold; color: #ffffff;"),
                                                        value = span("37 taxa",
                                                                     style = "font-family: Barlow; font-size: 20px; font-weight: bold; color: #ffc700;"),
                                                        showcase = span(bs_icon("arrow-down"),
                                                                        style = "color: #ffc700;"),
                                                        p("17 in Northern Point Conception", 
                                                          style = "font-family: Merriweather; font-size: 18px; color: #ffffff;"),
                                                        p("20 in Southern Point Conception", 
                                                          style = "font-family: Merriweather; font-size: 18px; color: #ffffff;"),
                                                        style = "background-color: #05641c;",
                                                        height = "140px")  
                                              
                                       ), # END value box column
                                       
                                     ), # END second fluidRow
                                     
                                     # third fluidRow
                                     fluidRow(
                                       
                                       # value box column
                                       column(width = 12,
                                              
                                              # total range edges value box
                                              value_box(title = span("Total Range Edges",
                                                                     style = "font-family: Barlow; font-size: 22px; font-weight: bold; color: #ffffff;"),
                                                        value = span("~ 25%",
                                                                     style = "font-family: Barlow; font-size: 20px; font-weight: bold; color: #ffc700;"),
                                                        showcase = div(
                                                          style = "position: relative; width: 36px; height: 36px;",
                                                          div(style = "position: absolute; top: 0; left: 0; width: 100%; height: 100%; border-radius: 50%; background-color: #ffc700;"),
                                                          div(style = "position: absolute; top: 2px; left: 2px; width: 32px; height: 32px; border-radius: 50%;  background-color: #05641c;"),
                                                          div(style = "position: absolute; top: 2px; left: 2px; width: 16px; height: 16px; background-color: #ffc700; border-top-left-radius: 100%;")),
                                                        p("of California species range’s end near Point Conception", 
                                                          style = "font-family: Merriweather; font-size: 18px;  color: #ffffff;"),
                                                        style = "background-color: #05641c;",
                                                        height = "140px")
                                              
                                       ), # END value box column
                                       
                                     ), # END third fluidRow
                                     
                              ), # END left-hand column
                              
                              # right-hand column
                              column(width = 6,
                                     
                                     # point conception image
                                     div(style = "padding-top: 15px;",
                                         imageOutput("range_edges", click = "edges_click") %>%
                                           withSpinner(color = "#ffc700", type = 1, size = 1)),
                                     
                                     # zoom output
                                     uiOutput("zoom_edges")
                                     
                              ) # END right-hand column
                              
                            ), # END fluidRow
                            
                     ), # END stats column
                     
                     # right buffer column
                     column(width = 1)
                     
            ) # END fifth fluidRow
            
    ), # END range edges tabItem
    
    # contemporary range shifts tabItem ----
    tabItem(tabName = "trends",
            
            # first fluidRow
            fluidRow(
              
              # left buffer column
              column(width = 1),
              
              # info column
              column(width = 10,
                     
                     # title
                     tags$h1("Contemporary Range Shifts",
                             style = "font-family: Barlow; font-weight: bold; color: #05641c; padding-bottom: 10px;"),
                     
                     # markdown
                     div(style = "font-size: 18px; font-family: Merriweather;", 
                         includeMarkdown("text/analyses/range-shifts.md"))
                     
              ), # END info column
              
              # right buffer column
              column(width = 1)
              
            ), # END first fluidRow
            
            # second fluidRow
            fluidRow(
              
              # left buffer column
              column(width = 1),
              
              # line column
              column(width = 10,
                     
                     # line
                     tags$hr(style = "border-top: 3px solid; color: #eae8f5; padding-top: 10px; padding-bottom: 10px;")
                     
              ), # END line column
              
              # right buffer column
              column(width = 1)
              
            ), # END second fluidRow
            
            # third fluidRow
            fluidRow(style = "padding-bottom: 10px;",
                     
                     # left buffer column
                     column(width = 1),
                     
                     # species column
                     column(width = 4,
                            style = "padding-bottom: 25px;",
                            
                            div(style = "font-family: Merriweather; font-size: 14px; padding-bottom: 30px;",
                                pickerInput(inputId = "species", 
                                            label = "Choose an intertidal species/species group:",
                                            choices = unique(target_boundaries$full_name),
                                            selected = unique(target_boundaries$full_name)[1],
                                            multiple = FALSE,
                                            options = pickerOptions(dropupAuto = FALSE, size = 3),
                                            width = "100%")),
                            
                            div(style = "text-align: center; padding-bottom: 10px;",
                                uiOutput("species_image") %>%
                                  withSpinner(color = "#05641C", type = 1, size = 1))
                            
                     ), # END species column
                     
                     # contemporary shift diagram column
                     column(width = 6,
                            style = " padding-top: 85px; padding-bottom: 25px;",
                            
                            # contemporary shift diagram
                            imageOutput("contemp_shift", click = "image_click") %>%
                              withSpinner(color = "#05641C", type = 1, size = 1),
                            
                            # zoom output
                            uiOutput("zoom_contemp")
                            
                     ), # END contemporary shift diagram column
                     
                     # right buffer column
                     column(width = 1),
                     
            ), # END third fluidRow
            
            # fourth fluidRow
            fluidRow(style = "padding-bottom: 20px;",
                     
                     # left buffer column
                     column(width = 1),
                     
                     # plot box
                     box(width = 5,
                         style = "height: 625px;",
                         
                         # dynamic title
                         tags$h4(textOutput("plotly_header"),
                                 style = "font-family: Barlow; font-weight: bold; color: #05641C; text-align: center;"),
                         
                         tags$h4(style = "font-family: Barlow; font-weight: bold; text-align: center; padding-bottom: 10px;",
                                 "Range Distribution Every 5 Years"),
                         
                         # plot output
                         plotlyOutput("species_plot") %>%
                           withSpinner(color = "#05641C", type = 1, size = 1)
                         
                     ), # END plot box
                     
                     # plot box
                     box(width = 5,
                         style = "height: 625px; padding-top: 40px",
                         
                         # image output
                         imageOutput("coastline_distance", click = "distance_click") %>%
                           withSpinner(color = "#05641C", type = 1, size = 1),
                         
                         # zoom output
                         uiOutput("zoom_coastline")
                         
                     ), # END plot box
                     
                     # right buffer column
                     column(width = 1)
                     
            ), # END fourth fluidRow
            
            # fifth fluidRow
            fluidRow(style = "background-color: #05641c; padding-bottom: 50px;",
                     
                     # left buffer column
                     column(width = 1),
                     
                     # stats column
                     column(width = 10,
                            
                            # fluidRow
                            fluidRow(
                              
                              # title
                              tags$h2("Rocky Intertidal Species Contemporary Range Shifts Near Point Conception",
                                      style = "font-family: Barlow; font-weight: bold; color: #ffffff; text-align: center; padding-bottom: 10px;"),
                              
                              # left-hand column
                              column(width = 6,
                                     
                                     # first fluidRow
                                     fluidRow(
                                       
                                       # value box column
                                       column(width = 12,
                                              
                                              # northern edge box
                                              value_box(title = span("Northward Trend in NorthernEdge",
                                                                     style = "font-family: Barlow; font-size: 20px; font-weight: bold; color: #ffffff;"),
                                                        value = span("14",
                                                                     style = "font-family: Barlow; font-size: 20px; font-weight: bold; color: #ffc700;"),
                                                        showcase = span(bs_icon("box-arrow-in-up-right"),
                                                                        style = "color: #ffc700;"),
                                                        p("Taxa moving into Point Conception", 
                                                          style = "font-family: Merriweather; font-size: 16px;  color: #ffffff;"),
                                                        style = "background-color: #05641c;",
                                                        height = "140px")
                                              
                                       ), # END value box column
                                       
                                     ), # END first fluidRow
                                     
                                     # second fluidRow
                                     fluidRow(
                                       
                                       # value box column
                                       column(width = 12,
                                              
                                              # southern edge box
                                              value_box(title = span("Northward Trend in Southern Edge",
                                                                     style = "font-family: Barlow; font-size: 20px; font-weight: bold; color: #ffffff;"),
                                                        value = span("18",
                                                                     style = "font-family: Barlow; font-size: 20px; font-weight: bold; color: #ffc700;"),
                                                        showcase = span(bs_icon("box-arrow-down-left"),
                                                                        style = "color: #ffc700;"),
                                                        p("Taxa moving out of Point Conception", 
                                                          style = "font-family: Merriweather; font-size: 16px;  color: #ffffff;"),
                                                        style = "background-color: #05641c;",
                                                        height = "140px")  
                                              
                                       ), # END value box column
                                       
                                     ), # END second fluidRow
                                     
                                     # third fluidRow
                                     fluidRow(
                                       
                                       # value box column
                                       column(width = 12,
                                              
                                              # percent box
                                              value_box(title = span("Northward Trend",
                                                                     style = "font-family: Barlow; font-size: 20px; font-weight: bold; color: #ffffff;"),
                                                        value = span("~ 54%",
                                                                     style = "font-family: Barlow; font-size: 20px; font-weight: bold; color: #ffc700;"),
                                                        showcase = span(bs_icon("circle-half"),
                                                                        style = "color: #ffc700;"),
                                                        p("of species with range edges at Point Conception have experienced a northward range shift", 
                                                          style = "font-family: Merriweather; font-size: 16px;  color: #ffffff;"),
                                                        style = "background-color: #05641c;",
                                                        height = "140px")
                                              
                                       ), # END value box column
                                       
                                     ), # END third fluidRow
                                     
                              ), # END left-hand column
                              
                              # right-hand column
                              column(width = 6,
                                     
                                     # point conception image
                                     div(style = "padding-top: 15px;",
                                         imageOutput("range_edges2", click = "edges2_click") %>%
                                           withSpinner(color = "#ffc700", type = 1, size = 1)),
                                     
                                     # zoom output
                                     uiOutput("zoom_edges2")
                                     
                              ) # END right-hand column
                              
                            ), # END fluidRow
                            
                     ), # END stats column
                     
                     # right buffer column
                     column(width = 1)
                     
            ) # END fifth fluidRow
            
    ), # END historic range shifts tabItem
    
    # projected shifts tabItem ----
    tabItem(tabName = "model",
            
            # first fluidRow
            fluidRow(
              
              # left buffer column
              column(width = 1),
              
              # info column
              column(width = 10,
                     
                     # title
                     tags$h1("Projected Shifts",
                             style = "font-family: Barlow; font-weight: bold; color: #05641c; padding-bottom: 10px;"),
                     
                     # markdown
                     div(style = "font-size: 18px; font-family: Merriweather;", 
                         includeMarkdown("text/analyses/model-info.md"))
                     
              ), # END info column
              
              # right buffer column
              column(width = 1)
              
            ), # END first fluidRow
            
            # second fluidRow
            fluidRow(
              
              # left buffer column
              column(width = 1),
              
              # line column
              column(width = 10,
                     
                     # line
                     tags$hr(style = "border-top: 3px solid; color: #eae8f5; padding-top: 10px; padding-bottom: 10px;")
                     
              ), # END line column
              
              # right buffer column
              column(width = 1)
              
            ), # END second fluidRow
            
            # third fluidRow
            fluidRow(
              
              # left buffer column
              column(width = 1),
              
              # species column
              column(width = 4,
                     
                     div(style = "font-family: Merriweather; font-size: 14px; padding-bottom: 30px;",
                         pickerInput(inputId = "change_selected_species",
                                     label = "Choose an intertidal species/species group:",
                                     choices = raster_df$full_name,
                                     multiple = FALSE,
                                     options = pickerOptions(dropupAuto = FALSE, size = 3),
                                     width = "100%")),
                     
                     div(style = "text-align: center; padding-bottom: 10px;",
                         uiOutput("species_info_box") %>%
                           withSpinner(color = "#05641C", type = 1, size = 1))
                     
              ), # END species column
              
              # change detection map box
              box(width = 6,
                  
                  # title
                  tags$h4("Map of Habitat Change From 2025 to 2050",
                          style = "text-align: center; font-weight: bold; font-family: Barlow; padding-bottom: 10px;"),
                  
                  # refresh button
                  actionButton(inputId = "refresh_change_detection", 
                               label = "Refresh Map",
                               icon = icon("rotate")),
                  
                  # change detection map
                  leafletOutput(outputId = "change_raster_output") %>% 
                    withSpinner(color = "#05641C", type = 1, size = 1)
                  
              ), # END change detection map box
              
              # right buffer column
              column(width = 1)
              
            ), # END third fluidRow
            
            # fourth fluidRow
            fluidRow(style = "padding-bottom: 20px;",
                     
                     # left buffer column
                     column(width = 1),
                     
                     # current suitability map box
                     box(width = 5,
                         
                         # title
                         tags$h4("Map of Species Habitat In 2025",
                                 style = "text-align: center; font-weight: bold; font-family: Barlow; padding-bottom: 10px;"),
                         
                         # refresh button
                         actionButton(inputId = "refresh_current_suitability", 
                                      label = "Refresh Map",
                                      icon = icon("rotate")),
                         
                         # current suitability map
                         leafletOutput(outputId = "current_raster_output") %>% 
                           withSpinner(color = "#05641C", type = 1, size = 1)
                         
                     ), # END current suitability map box
                     
                     # projected suitability map box
                     box(width = 5,
                         
                         # title
                         tags$h4("Map of Forecasted Species Habitat For 2050",
                                 style = "text-align: center; font-weight: bold; font-family: Barlow; padding-bottom: 10px;"),
                         
                         # refresh button
                         actionButton(inputId = "refresh_projected_suitability", 
                                      label = "Refresh Map",
                                      icon = icon("rotate")),
                         
                         # projected suitability map
                         leafletOutput(outputId = "projected_raster_output") %>% 
                           withSpinner(color = "#05641C", type = 1, size = 1)
                         
                     ), # END projected suitability map box
                     
                     # right buffer column
                     column(width = 1)
                     
            ), # END fourth fluidRow
            
            # fifth fluidRow
            fluidRow(style = "background-color: #05641c; padding-bottom: 50px;",
                     
                     # left buffer column
                     column(width = 1),
                     
                     # stats column
                     column(width = 10,
                            
                            # fluidRow
                            fluidRow(
                              
                              # title
                              tags$h2("Rocky Intertidal Species Habitat Suitability Near Point Conception",
                                      style = "font-family: Barlow; font-weight: bold; color: #ffffff; text-align: center; padding-bottom: 10px;"),
                              
                              # left-hand column
                              column(width = 6,
                                     
                                     # first fluidRow
                                     fluidRow(
                                       
                                       # value box column
                                       column(width = 12,
                                              
                                              #  suitability increase box
                                              value_box(title = span("Suitability Increase",
                                                                     style = "font-family: Barlow; font-size: 22px; font-weight: bold; color: #ffffff;"),
                                                        value = span("22",
                                                                     style = "font-family: Barlow; font-size: 20px; font-weight: bold; color: #ffc700;"),
                                                        showcase = span(bs_icon("graph-up-arrow"),
                                                                        style = "color: #ffc700;"),
                                                        p("Taxa with increased habitat suitability in our projected 2050 models", 
                                                          style = "font-family: Merriweather; font-size: 16px; color: #ffffff;"),
                                                        style = "background-color: #05641c;",
                                                        height = "140px")
                                              
                                       ), # END value box column
                                       
                                     ), # END first fluidRow
                                     
                                     # second fluidRow
                                     fluidRow(
                                       
                                       # value box column
                                       column(width = 12,
                                              
                                              # suitability decrease box
                                              value_box(title = span("Suitability Decrease",
                                                                     style = "font-family: Barlow; font-size: 22px; font-weight: bold; color: #ffffff;"),
                                                        value = span("27",
                                                                     style = "font-family: Barlow; font-size: 20px; font-weight: bold; color: #ffc700;"),
                                                        
                                                        showcase = span(bs_icon("graph-down-arrow"),
                                                                        style = "color: #ffc700;"),
                                                        p("Taxa with decreased habitat suitability in our projected 2050 models", 
                                                          style = "font-family: Merriweather; font-size: 16px; color: #ffffff;"),
                                                        style = "background-color: #05641c;",
                                                        height = "140px")  
                                              
                                       ), # END value box column
                                       
                                     ), # END second fluidRow
                                     
                                     # third fluidRow
                                     fluidRow(
                                       
                                       # value box column
                                       column(width = 12,
                                              
                                              # percent value box
                                              value_box(title = span("Northward Trend",
                                                                     style = "font-family: Barlow; font-size: 22px; font-weight: bold; color: #ffffff;"),
                                                        value = span("~ 83%",
                                                                     style = "font-family: Barlow; font-size: 20px; font-weight: bold; color: #ffc700;"),
                                                        showcase = div(style = "position: relative; width: 36px; height: 36px;",
                                                                       div(style = "position: absolute; top: 0; left: 0; width: 100%; height: 100%; border-radius: 50%; background-color: #ffc700;"),
                                                                       div(style = "position: absolute; top: 2px; left: 2px; width: 32px; height: 32px; border-radius: 50%; background: linear-gradient(30deg, #05641c 50%, #ffc700 50%);"),
                                                                       div(style = "position: absolute; top: 2px; left: 2px; width: 32px; height: 16px; background-color: #ffc700; border-top-left-radius: 16px; border-top-right-radius: 16px;"),
                                                                       div(style = "position: absolute; top: 18px; left: 2px; width: 16px; height: 16px; background-color: #ffc700; border-bottom-left-radius: 16px;")),
                                                        p("of the species with Range Edges in Point Conception have projected suitability changes", 
                                                          style = "font-family: Merriweather; font-size: 16px; color: #ffffff;"),
                                                        style = "background-color: #05641c;",
                                                        height = "140px")
                                              
                                       ), # END value box column
                                       
                                     ), # END third fluidRow
                                     
                              ), # END left-hand column
                              
                              # right-hand column
                              column(width = 6,
                                     
                                     # point conception image
                                     div(style = "padding-top: 15px;",
                                         imageOutput("range_edges3", click = "edges3_click") %>%
                                           withSpinner(color = "#ffc700", type = 1, size = 1)),
                                     
                                     # zoom output
                                     uiOutput("zoom_edges3")
                                     
                              ) # END right-hand column
                              
                            ), # END fluidRow
                            
                     ), # END stats column
                     
                     # right buffer column
                     column(width = 1)
                     
            ) # END fifth fluidRow
            
    ), # END model tabItem
    
    # priority monitoring assessment tabItem ----
    tabItem(tabName = "assessment",
            
            # first fluidRow
            fluidRow(
              
              # left buffer column
              column(width = 1),
              
              # info column
              column(width = 10,
                     
                     # title
                     tags$h1("Priority Monitoring Assessment",
                             style = "font-family: Barlow; font-weight: bold; color: #05641c; padding-bottom: 10px;"),
                     
                     # assessment info
                     div(style = "font-size: 18px; font-family: Merriweather;", 
                         includeMarkdown("text/analyses/assessment.md")),
                     
                     # line
                     tags$hr(style = "border-top: 3px solid #eae8f5; margin-top: 20px; margin-bottom: 20px;")
                     
              ), # END info column
              
              # right buffer column
              column(width = 1)
              
            ), # END first fluidRow
            
            # second fluidRow
            fluidRow(
              
              # left buffer column
              column(width = 1),
              
              # radioButtons column
              column(width = 10,
                     
                     div(style = "text-align: center; font-family: Merriweather; font-size: 16px; font-weight: bold;",
                         radioButtons("range_type", 
                                      label = NULL,
                                      choices = c("Range Contractions" = "contractions", "Range Expansions" = "expansions"),
                                      selected = "contractions", 
                                      inline = TRUE))
                     
              ), # END radioButtons column
              
              # left buffer column
              column(width = 1)
              
            ), # END second fluidRow
            
            # conditionalPanel
            conditionalPanel(condition = "input.range_type == 'contractions'",
                             
                             # third fluidRow
                             fluidRow(
                               
                               # left buffer column
                               column(width = 1),
                               
                               # title column
                               column(width = 10,
                                      
                                      # title
                                      tags$h2("Priority Monitoring For Range Contractions",
                                              style = "font-family: Barlow; font-weight: bold; color: #05641c; padding-bottom: 10px; text-align: center;")
                                      
                               ), # END title column
                               
                               # right buffer column
                               column(width = 1)
                               
                             ), # END third fluidRow
                             
                             # fourth fluidRow
                             fluidRow(
                               
                               # left buffer column
                               column(width = 1),
                               
                               # checkbox column
                               column(width = 10,
                                      
                                      # fluidRow
                                      fluidRow(
                                        
                                        # checkbox
                                        column(width = 3,
                                               
                                               div(style = "font-family: Merriweather;",
                                                   checkboxGroupInput("contraction", "Contraction Monitoring Priority:",
                                                                      choices = c("High" = 3, "Medium" = 2, "Low" = 1),
                                                                      selected = c(3, 2, 1)))
                                               
                                        ), # END checkbox
                                        
                                        # checkbox
                                        column(width = 4,
                                               
                                               div(style = "font-family: Merriweather; text-align: center;",
                                                   tags$div(tags$label("Southern range edge in Dangermond"),
                                                            tags$input(type = "checkbox", checked = "checked", disabled = "disabled")))
                                               
                                        ), # END checkbox
                                        
                                        # checkbox
                                        column(width = 2,
                                               
                                               div(style = "font-family: Merriweather; text-align: center;",
                                                   tags$div(tags$label("Northward trend", `for` = "north_trend_filter_con"),
                                                            tags$input(id = "north_trend_filter_con", type = "checkbox")))
                                               
                                        ), # END checkbox
                                        
                                        # checkbox
                                        column(width = 3,
                                               
                                               div(style = "font-family: Merriweather; text-align: center;",
                                                   tags$div(tags$label("Habitat loss in Dangermond", `for` = "percent_change_filter_con"),
                                                            tags$input(id = "percent_change_filter_con", type = "checkbox")))
                                               
                                        ), # END checkbox
                                        
                                      ) # END fluidRow
                                      
                               ), # END checkbox column
                               
                               # right buffer column
                               column(width = 1)
                               
                             ), # END fourth fluidRow
                             
                             # fifth fluidRow
                             fluidRow(
                               
                               # left buffer column
                               column(width = 1),
                               
                               # DT column
                               column(width = 10, 
                                      style = "padding-bottom: 50px;",
                                      
                                      # DT
                                      div(style = "font-family: Merriweather;",
                                          DTOutput("species_contraction_output") %>%
                                            withSpinner(color = "#05641C", type = 1, size = 1))
                                      
                               ), # END DT column
                               column(width = 1)
                               
                             ) # fifth fluidRow
                             
            ), # END conditionalPanel
            
            # conditionalPanel
            conditionalPanel(condition = "input.range_type == 'expansions'",
                             
                             # sixth fluidRow
                             fluidRow(
                               
                               # left buffer column
                               column(width = 1),
                               
                               # title column
                               column(width = 10,
                                      
                                      # title
                                      tags$h2("Priority Monitoring For Range Expansions",
                                              style = "font-family: Barlow; font-weight: bold; color: #05641c; padding-bottom: 10px; text-align: center;")
                                      
                               ), # END title column
                               
                               # right buffer column
                               column(width = 1)
                               
                             ), # END sixth fluidRow
                             
                             # seventh fluidRow
                             fluidRow(
                               
                               # left buffer column
                               column(width = 1),
                               
                               # column
                               column(width = 10,
                                      
                                      # fluidRow
                                      fluidRow(
                                        
                                        # checkbox column
                                        column(width = 3,
                                               
                                               div(style = "font-family: Merriweather;",
                                                   checkboxGroupInput("expansion", "Expansion Monitoring Priority:",
                                                                      choices = c("High" = 3, "Moderate" = 2, "Low" = 1),
                                                                      selected = c(3, 2, 1)))
                                               
                                        ), # END checkbox column
                                        
                                        # checkbox column
                                        column(width = 4,
                                               
                                               div(style = "font-family: Merriweather; text-align: center;",
                                                   tags$div(tags$label("Northern range edge in Dangermond"),
                                                            tags$input(type = "checkbox", checked = "checked", disabled = "disabled")))
                                               
                                        ), # END checkbox column
                                        
                                        # checkbox column
                                        column(width = 2,
                                               
                                               div(style = "font-family: Merriweather; text-align: center;",
                                                   tags$div(tags$label("Northward trend", `for` = "north_trend_filter_exp"),
                                                            tags$input(id = "north_trend_filter_exp", type = "checkbox")))
                                               
                                        ), # END checkbox column
                                        
                                        # checkbox column
                                        column(width = 3,
                                               
                                               div(style = "font-family: Merriweather; text-align: center;",
                                                   tags$div(tags$label("Habitat gain in Dangermond", `for` = "percent_change_filter_exp"),
                                                            tags$input(id = "percent_change_filter_exp", type = "checkbox")))
                                               
                                        ), # END checkbox column
                                        
                                      ) # END fluidRow
                                      
                               ), # END column
                               
                               # right buffer column
                               column(width = 1)
                               
                             ), # END seventh fluidRow
                             
                             # eighth fluidRow
                             fluidRow(
                               
                               # left buffer column
                               column(width = 1),
                               
                               # DT column
                               column(width = 10, 
                                      style = "padding-bottom: 50px;",
                                      
                                      div(style = "font-family: Merriweather;",
                                          DTOutput("species_expansion_output") %>%
                                            withSpinner(color = "#05641C", type = 1, size = 1))
                                      
                               ), # END DT column
                               
                               # right buffer column
                               column(width = 1)
                               
                             ) # END eighth fluidRow
                             
            ) # END conditionalPanel
            
    ), # End of assessment tabItem
    
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
                              
                              div(style = "font-family: Merriweather; font-size: 18px;  padding-bottom: 10px;", 
                                  includeMarkdown("text/data-limitations/MARINe.md"))
                              
                       ), # END left-hand column
                       
                       # right-hand column
                       column(width = 4,
                              
                              div(style = "text-align: center; padding-bottom: 10px;",
                                  img(src = "logos/MARINe.png", width = 225, height = 225))
                              
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
                     
                     tags$hr(style = "border-top: 3px solid; color: #eae8f5; padding-bottom: 10px;")
                     
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
                              
                              div(style = "font-family: Merriweather; font-size: 18px; padding-bottom: 10px;",
                                  includeMarkdown("text/data-limitations/Bio-ORACLE.md"))
                              
                       ), # END left-hand column
                       
                       # right-hand column
                       column(width = 4,
                              
                              div(style = "text-align: center; padding-bottom: 10px;",
                                  img(src = "logos/Bio-ORACLE.png", width = 225, height = 275))
                              
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
                     
                     tags$hr(style = "border-top: 3px solid; color: #eae8f5; padding-bottom: 10px;")
                     
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
                     
                     div(style = "font-family: Merriweather; font-size: 18px; padding-bottom: 10px;", 
                         includeMarkdown("text/data-limitations/limitations.md"))
                     
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
              
              # title column
              column(width = 10,
                     
                     tags$h1("Acknowledgements",
                             style = "font-family: Barlow; font-weight: bold; color: #05641c; padding-bottom: 10px;")
                     
              ), # END title column
              
              # right buffer column
              column(width = 1)
              
            ), # END first fluidRow
            
            # second fluidRow
            fluidRow(
              
              # left buffer column
              column(width = 1),
              
              # coastalconservation column
              column(width = 10,
                     
                     # fluidRow
                     fluidRow(
                       
                       # left-hand column
                       column(width = 8,
                              
                              div(style = "font-family: Merriweather; font-size: 18px; padding-bottom: 10px;", 
                                  includeMarkdown("text/acknowledgements/coastalconservation.md"))
                              
                       ), # END left-hand column
                       
                       # right-hand column
                       column(width = 4,
                              
                              div(style = "display: flex; justify-content: center; align-items: center; height: 250px;",
                                  img(src = "logos/coastal_conservation_hex.png", width = "225px", height = "250px"))
                              
                       ) # END right-hand column
                       
                     ) # END fluidRow
                     
              ), # END coastalconservation column
              
              # right buffer column
              column(width = 1)
              
            ), # END second fluidRow
            
            # third fluidRow
            fluidRow(
              
              # left buffer column
              column(width = 1),
              
              # line column
              column(width = 10,
                     
                     tags$hr(style = "border-top: 3px solid; color: #eae8f5; padding-bottom: 10px;")
                     
              ), # END line column
              
              # right buffer column
              column(width = 1)
              
            ), # END third fluidRow
            
            # fourth fluidRow
            fluidRow(
              
              # left buffer column
              column(width = 1),
              
              # TNC column
              column(width = 10,
                     
                     # fluidRow
                     fluidRow(
                       
                       # left-hand column
                       column(width = 8,
                              
                              div(style = "font-family: Merriweather; font-size: 18px; padding-bottom: 10px;", 
                                  includeMarkdown("text/acknowledgements/TNC.md"))
                              
                       ), # END left-hand column
                       
                       # right-hand column
                       column(width = 4,
                              
                              div(style = "display: flex; justify-content: center; align-items: center; height: 200px;",
                                  img(src = "logos/TNCLogoIcon_RGB_PNG.png", width = "175px", height = "175px"))
                              
                       ) # END right-hand column
                       
                     ) # END fluidRow
                     
              ), # END TNC column
              
              # right buffer column
              column(width = 1)
              
            ), # END fourth fluidRow
            
            # fifth fluidRow
            fluidRow(
              
              # left buffer column
              column(width = 1),
              
              # line column
              column(width = 10,
                     
                     tags$hr(style = "border-top: 3px solid; color: #eae8f5; padding-bottom: 10px;")
                     
              ), # END line column
              
              # right buffer column
              column(width = 1)
              
            ), # END fifth fluidRow
            
            # sixth fluidRow
            fluidRow(
              
              # left buffer column
              column(width = 1),
              
              # Bren column
              column(width = 10,
                     
                     # fluidRow
                     fluidRow(
                       
                       # left-hand column
                       column(width = 8,
                              
                              div(style = "font-family: Merriweather; font-size: 18px; padding-bottom: 10px;", 
                                  includeMarkdown("text/acknowledgements/Bren.md"))
                              
                       ), # END left-hand column
                       
                       # right-hand column
                       column(width = 4,
                              
                              div(style = "display: flex; justify-content: center; align-items: center; height: 200px;",
                                  img(src = "logos/Bren-LeafArtOnly-FullColor-RGB-transparent.png", width = "200px", height = "200px"))
                              
                       ) # END right-hand column
                       
                     ) # END fluidRow
                     
              ), # END Bren column
              
              # right buffer column
              column(width = 1)
              
            ), # END sixth fluidRow
            
            # seventh fluidRow
            fluidRow(
              
              # left buffer column
              column(width = 1),
              
              # line column
              column(width = 10,
                     
                     tags$hr(style = "border-top: 3px solid; color: #eae8f5; padding-bottom: 10px;")
                     
              ), # END line column
              
              # right buffer column
              column(width = 1)
              
            ), # END seventh fluidRow
            
            # eighth fluidRow
            fluidRow(
              
              # left buffer column
              column(width = 1),
              
              # iNaturalist column
              column(width = 10,
                     
                     # fluidRow
                     fluidRow(
                       
                       # left-hand column
                       column(width = 8,
                              
                              div(style = "font-family: Merriweather; font-size: 18px; padding-bottom: 10px;", 
                                  includeMarkdown("text/acknowledgements/iNaturalist.md"))
                              
                       ), # END left-hand column
                       
                       # right-hand column
                       column(width = 4,
                              
                              div(style = "display: flex; justify-content: center; align-items: center; height: 125px;",
                                  img(src = "logos/bird.png", width = "150px", height = "150px"))
                              
                       ) # END right-hand column
                       
                     ) # END fluidRow
                     
              ), # END iNaturlist column
              
              # right buffer column
              column(width = 1)
              
            ), # END eighth fluidRow
            
            # ninth fluidRow
            fluidRow(
              
              # left buffer column
              column(width = 1),
              
              # line column
              column(width = 10,
                     
                     tags$hr(style = "border-top: 3px solid; color: #eae8f5; padding-bottom: 10px;")
                     
              ), # END line column
              
              # right buffer column
              column(width = 1)
              
            ), # END ninth fluidRow
            
            # tenth fluidRow
            fluidRow(
              
              # left buffer column
              column(width = 1),
              
              # Jordan column
              column(width = 10,
                     
                     # fluidRow
                     fluidRow(
                       
                       # left-hand column
                       column(width = 8,
                              
                              div(style = "font-family: Merriweather; font-size: 18px; padding-bottom: 10px;", 
                                  includeMarkdown("text/acknowledgements/Jordan.md"))
                              
                       ), # END left-hand column
                       
                       # right-hand column
                       column(width = 4,
                              
                              div(style = "display: flex; justify-content: center; align-items: center; height: 125px; padding-bottom: 10px;",
                                  img(src = "logos/jcs_logo.png", width = "125px", height = "125px"))
                              
                       ) # END right-hand column
                       
                     ) # END fluidRow
                     
              ), # END Jordan column
              
              # right buffer column
              column(width = 1)
              
            ) # END tenth fluidRow
            
    ) # END acknowledgements tabItem
    
  ) # END tabItems
  
) # END dashboardBody

# combine all into dashboardPage ----
dashboardPage(header, sidebar, body,
              tags$head(tags$title("CRISP"),
                        tags$link(rel = "shortcut icon", href = "logos/coastal_conservation_hex.ico")))