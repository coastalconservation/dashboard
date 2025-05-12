# load packages ----
library(slickR)

# carousel images file path ----
file_path <- list.files("shinydashboard/www/carousel_images", full.names = TRUE, pattern = "jpg")

# image captions ---- 
captions <- c("Jordan",
              "Amanda",
              "Ian",
              "Matteo",
              "Bruce",
              "Erica",
              "Max")

# slickR carousel ----
slickR(file_path,
       slideType = "img", 
       slideId = "Carousel",
       height = 300,
       width = "100%") %synch%
  (slickR(captions, 
          slideType = "p",
          height = 50,
          width = "100%") + 
     settings(arrows = FALSE)) +
  settings(slidesToShow = 1,
           slidesToScroll = 1,
           arrows = TRUE,
           autoplay = TRUE,
           autoplaySpeed = 3000)