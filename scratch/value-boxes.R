# load packages
library(shinydashboard)
library(bslib)

# read data
species_extent <- read_csv("crisp/data/processed/species_extent.csv")

# value boxes (bslib)
value_box(title = "Northern Range Edge",
          value = "23 species")

value_box(title = "Southern Range Edge",
          value = "37 species")

value_box(title = "Sites",
          value = )


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