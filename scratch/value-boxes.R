# load packages
library(shinydashboard)
library(bslib)
library(bsicons)

# read data
species_extent <- read_csv("crisp/data/processed/species_extent.csv")

# value boxes (bslib)
value_box(title = span("Northern Range Edges",
                       style = "font-size: 22px; font-weight: bold;"),
          value = span("23 species",
                       style = "font-size: 20px;"),
          showcase = span(bs_icon("arrow-up"),
                          style = "color: black;"),
          p("11 in North Dangermond", style = "font-size: 15px;"),
          p("12 in Southern Dangermond", style = "font-size: 15px;"),
          style = "background-color: #49a842; border-radius: 15px;",
          height = "125px")

value_box(title = span("Southern Range Edges",
                       style = "font-size: 22px; font-weight: bold;"),
          value = span("37 species",
                       style = "font-size: 20px;"),
          showcase = span(bs_icon("arrow-down"),
                          style = "color: black;"),
          p("17 in Northern Dangemond", style = "font-size: 15px;"),
          p("20 in Southern Dangermond", style = "font-size: 15px;"),
          style = "background-color: #01c1e3; border-radius: 15px;",
          height = "125px")  

value_box(title = span("MARINe Survey Sites",
                       style = "font-size: 22px; font-weight: bold;"),
          value = span("16",
                       style = "font-size: 20px;"),
          showcase = span(bs_icon("geo-alt"),
                          style = "color: black;;"),
          style = "background-color: #ffc700; border-radius: 15px;",
          height = "125px") 

# value boxes
valueBox(range_list %>%
           filter(range_edge_category %in% c("Northern Range Edge")) %>%
           filter(segment_name %in% c("Northern Dangermond", "Southern Dangermond")) %>%
           summarize(total = n()),
         subtitle = "Northern Range Edge near Dangermond",
         color = "black")

valueBox(range_list %>%
           filter(range_edge_category %in% c("Southern Range Edge")) %>%
           filter(segment_name %in% c("Northern Dangermond", "Southern Dangermond")) %>%
           summarize(total = n()),
         subtitle = "Southern Range Edge near Dangermond",
         color = "black")

valueBox(range_list %>%
           filter(range_edge_category %in% c("Northern Range Edge", "Southern Range Edge")) %>%
           filter(segment_name %in% c("Northern Dangermond", "Southern Dangermond")) %>%
           summarize(total = n()),
         subtitle = "Range Edge near Dangermond",
         color = "black")