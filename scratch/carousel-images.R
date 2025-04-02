library(slickR)

# image path ----
image_list <- list.files("shinydashboard/www/images", full.names = TRUE, pattern = "jpg")

# carousel images
slickR(
  image_list,  
  height = 300,  
  width = "100%",  
  slideId = "Carousel"
) + 
  settings(
    slidesToShow = 1,
    slidesToScroll = 1,
    arrows = FALSE,
    dots = TRUE,
    autoplay = TRUE,
    autoplaySpeed = 3000
  )
