
# install_github("thomasp85/tweenr")
# install_github("thomasp85/transformr")
# install_github("thomasp85/gganimate")
# install.packages("C:/Users/rtimpe/Downloads/gganimate_0.9.9.9999.tar.gz", repos = NULL, type="source")

library(tidyverse)
library(jpeg)
library(gganimate)

#1 SCALE IMAGE ----
# Adapted from LEGO mosaics project
scale_image <- function(image, img_size){
  #Convert image to a data frame with RGB values
  img <- bind_rows(
    list(
      (as.data.frame(image[, , 1]) %>% 
         mutate(y=row_number(), channel = "R")),
      (as.data.frame(image[, , 2]) %>% 
         mutate(y=row_number(), channel = "G")),
      (as.data.frame(image[, , 3]) %>% 
         mutate(y=row_number(), channel = "B"))
    )
  ) %>% 
    gather(x, value, -y, -channel) %>% 
    mutate(x = as.numeric(gsub("V", "", x))) %>% 
    spread(channel, value)
  
  img_size <- round(img_size, 0)
  
  #Wide or tall image? Shortest side should be `img_size` pixels
  if(max(img$x) > max(img$y)){
    img_scale_x <-  max(img$x) / max(img$y)
    img_scale_y <- 1
  } else {
    img_scale_x <- 1
    img_scale_y <-  max(img$y) / max(img$x)
  }
  
  #If only 1 img_size value, create a square image
  if(length(img_size) == 1){
    img_size2 <- c(img_size, img_size)
  } else {
    img_size2 <- img_size[1:2]
    img_scale_x <- 1
    img_scale_y <- 1
  }
  
  #Rescale the image
  img2 <- img %>% 
    mutate(y_scaled = (y - min(y))/(max(y)-min(y))*img_size2[2]*img_scale_y + 1,
           x_scaled = (x - min(x))/(max(x)-min(x))*img_size2[1]*img_scale_x + 1) %>% 
    select(-x, -y) %>% 
    group_by(y = ceiling(y_scaled), x = ceiling(x_scaled)) %>% 
    #Get average R, G, B and convert it to hexcolor
    summarize_at(vars(R, G, B), funs(mean(.))) %>% 
    rowwise() %>% 
    mutate(color = rgb(R, G, B)) %>% 
    ungroup() %>% 
    #Center the image
    filter(x <= median(x) + img_size2[1]/2, x > median(x) - img_size2[1]/2,
           y <= median(y) + img_size2[2]/2, y > median(y) - img_size2[2]/2) %>%
    #Flip y
    mutate(y = (max(y) - y) + 1)
  
  out_list <- list()
  out_list[["Img_scaled"]] <- img2
  
  return(out_list)
}

radius <- 50
img_raw <- readJPEG("Einstein.jpg") %>% 
  scale_image(radius*2)

# FUnction for equidistant points on a spiral

spiral_cartesian <- function(img_df, spiral_radius, num_coils, chord_length, rotation){
  img <- img_df$Img_scaled
  
  #Derive additional spiral specifications
  centerX <- median(img$x)
  centerY <- median(img$y)
  
  thetaMax <- num_coils * 2 * pi
  awayStep <- spiral_radius / thetaMax
  
  #While loop to keep drawing spiral until we hit thetaMax
  spiral <- tibble()
  theta <- chord_length/awayStep
  
  while(theta <= thetaMax){
    #How far away from center
    away = awayStep * theta
    
    #How far around the center
    around = theta + rotation
    
    #Convert 'around' and 'away' to X and Y.
    x = centerX + cos(around) * away
    y = centerY + sin(around) * away
    
    spiral <- spiral %>% 
      bind_rows(tibble(x=x, y=y))
    
    theta = theta + chord_length/away
  }
  
  return(c(img_df, list(spiral = spiral)))
}


img_spiral <- img_raw %>% 
  spiral_cartesian(radius, 50, 2, 0)


#Project the image onto the spiral
project_image <- function(img_df){
  dat <- img_df$spiral %>% 
    #Round each spiral point to nearest whole number
    mutate(xs = round(x), ys = round(y)) %>% 
    #Join on the rounded points
    left_join(img_df$Img_scaled %>% rename(xs=x, ys=y)) %>% 
    #Creat greyscale - 0 is lightest, 1 is darkest
    mutate(grey = R+G+B,
           grey = (1- (grey / max(grey))))
    
  return(c(img_df, list(projected_spiral = dat)))
}

img_spiral2 <- img_spiral %>% 
  project_image()

ggplot(img_spiral2$projected_spiral, aes(x=x, y=y, size = grey)) +
  geom_path() +
  scale_size_continuous(range = c(0.1, 1.8))+
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "none"
  )
