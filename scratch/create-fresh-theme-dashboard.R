# load libraries ----
library(fresh)

# create theme ----
create_theme(
  
  # dashboardHeader color
  adminlte_color(light_blue = "#EAE8F5"),
  
  # dashboardBody color
  adminlte_global(content_bg = "#FFFFFF"),
  
  # dashboardSidebar color
  adminlte_sidebar(dark_bg = "#EAE8F5",
                   dark_hover_bg = "#05641C",
                   dark_color = "black"),
  
  # Save theme to www directory ----
  output_file = here::here("shinydashboard", "www", "dashboard-fresh-theme.css")
  
)