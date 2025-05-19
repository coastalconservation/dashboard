# load packages
library(tidyverse)

# read data
species_extent <- read_csv("shinydashboard/data/processed/species_extent.csv")

cn_lookup <- c("Abietinaria/Amphisbetia spp"  = "Sea Fir Hydroids", ---
               "Acanthinucella paucilirata" = "Checkered Unicorn",
               "Acanthinucella spp" = " ", ----
               "Acmaea mitra" = "Whitecap Limpet",
               "Acrosiphonia/Cladophora spp" = "Filamentous Green Algae", ----
               "Acrosorium spp" = " ", ----
               "Aeolidia papillosa" = "Shag-rug Nudibranch",
               "Agathistoma eiseni" = ""
)
