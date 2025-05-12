# load packages
library(shinydashboard)

# read data
range_list <- read_csv("shinydashboard/data/range_list.csv")


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
