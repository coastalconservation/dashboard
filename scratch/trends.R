# Load packages
library(tidyverse)
library(here)
library(ggplot2)
library(lubridate)

# Species list
species_list <- c("CHOCAN", "CLACOL", "EGRMEN", "EISARB", 
                  "ENDMUR", "FUCGAR", "LOTGIG", "MAZAFF", "MYTCAL", 
                  "PISOCH", "POLPOL", "SARMUT", "SILCOM", "TETRUB", 
                  "NEOLAR", "SEMCAR", "HEDSES")

# Species names
species_names <- c(CHOCAN = "Chondracanthus canaliculatus", 
                   CLACOL = "Cladophora columbiana",
                   EGRMEN = "Egregia menziesii", 
                   EISARB = "Eisenia arborea", 
                   ENDMUR = "Endocladia muricata", 
                   FUCGAR = "Fucus distichus", 
                   LOTGIG = "Lottia gigantea", 
                   MAZAFF = "Mazzaella affinis", 
                   MYTCAL = "Mytilus californianus", 
                   PISOCH = "Pisaster ochraceus", 
                   POLPOL = "Pollicipes polymerus", 
                   SARMUT = "Sargassum muticum", 
                   SILCOM = "Silvetia compressa", 
                   TETRUB = "Tetraclita rubescens",
                   NEOLAR = "Neorhodomela larix ", 
                   SEMCAR = "Semibalanus cariosus", 
                   HEDSES = "Hedophyllum sessile")

# Read data (2002-2024)
longterm_data <- read_csv(here("raw-data", "phototranraw_download.csv")) %>%
  filter(state_province == "California" & island == "Mainland") %>%
  filter(lumping_code %in% species_list) %>%
  mutate(species_name = recode(lumping_code, !!!species_names)) %>%
  mutate(year = year(survey_date))

# Average Percent Cover Overtime (including 0s)
longterm_data %>%
  filter(lumping_code == "PISOCH") %>%
  group_by(year) %>%
  summarize(avg_percent_cover = mean(percent_cover, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = avg_percent_cover, group = 1)) +
  geom_point(col = "#0066cc", size = 2) +
  geom_line(col = "#0066cc", linewidth = 1) +
  scale_x_continuous(breaks = seq(2002, 2024, by = 5), expand = c(0, 0.1)) +
  labs(title =  "Changes in Percent Cover of Intertidal Species Along the CA Coast",
       y = "Average Percent Cover (%)") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        plot.title = element_text(size = 12,
                                  face = "bold"))

