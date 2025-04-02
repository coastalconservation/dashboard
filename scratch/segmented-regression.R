# load packages
library(here)
library(ggplot2)
library(tidyverse)
library(segmented)

# Read data
longterm_data <- read_csv(here("raw-data", "phototranraw_download.csv")) %>%
  filter(state_province == "California" & island == "Mainland") %>%
  mutate(year = year(survey_date))

# filter species
species <- longterm_data %>%
  filter(lumping_code == "MAZAFF" & latitude > 34.03853 & latitude < 35.88387) %>% 
  group_by(year) %>%
  summarize(avg_pct_cover = mean(percent_cover, na.rm = TRUE))

# linear model
lm_model <- lm(avg_pct_cover ~ year, data = species)

# segmented model
seg_model <- segmented(lm_model, seg.Z = ~year, npsi = 2)

# model output
summary(seg_model)

# add predicted values from segmented model
species$predicted <- predict(seg_model)

# extract estimated breakpoints
breakpoints <- seg_model$psi[, "Est."]

# plot model
ggplot(data = species, aes(x = year, y = avg_pct_cover)) +
  geom_point(color = "darkgrey") +
  geom_line(aes(y = predicted), color = "blue", linewidth = 1) +
  geom_vline(xintercept = breakpoints, linetype = "dashed", color = "black", linewidth = 0.5) +
  labs(x = "Year",
       y = "Average Percent Cover (%)") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw()
