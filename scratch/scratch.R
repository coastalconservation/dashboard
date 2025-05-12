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