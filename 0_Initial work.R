
# Set path of Rtools
Sys.setenv(PATH = paste(Sys.getenv("PATH"), "*InstallDirectory*/Rtools/bin/",
                        "*InstallDirectory*/Rtools/mingw_64/bin", sep = ";")) #for 64 bit version
Sys.setenv(BINPREF = "*InstallDirectory*/Rtools/mingw_64/bin")
library(devtools)

#Manually "force" version to be accepted 
assignInNamespace("version_info", c(devtools:::version_info, list("3.5" = list(version_min = "3.3.0", version_max = "99.99.99", path = "bin"))), "devtools")
find_rtools() # is TRUE now


install_github("thomasp85/tweenr")
install_github("thomasp85/gganimate")
# install.packages("C:/Users/rtimpe/Downloads/gganimate_0.9.9.9999.tar.gz", repos = NULL, type="source")


library(tidyverse)
library(jpeg)
library(gganimate)

#1 SCALE IMAGE ----
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

#Image
size = 50
img_raw <- readJPEG("Einstein.jpg") %>% 
  scale_image(size*2)
img_raw <- readJPEG("JP.jpg") %>% 
  scale_image(size*2)

img_raw <- readJPEG("GoldenGirls.jpg") %>% 
  scale_image(size*2)

#https://stackoverflow.com/questions/13894715/draw-equidistant-points-on-a-spiral

#Equidistant version
img <- img_raw$Img_scaled

centerX <- median(img$x)
centerY <- median(img$y)
radius <- size
sides <- 500
coils <- 50
rotation <- 0

thetaMax <- coils * 2 * pi
awayStep <- radius / thetaMax
chord <- 2

spiral <- tibble()
theta <- chord/awayStep
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
    
    theta = theta + chord/away
}

#Image spiral
img_spiral <- spiral %>% 
  mutate(xs = round(x), ys = round(y)) %>% 
  left_join(img %>% rename(xs=x, ys=y)) %>% 
  mutate(xp = c(rep(1:coils, nrow(.) %/% coils), 1:(nrow(.) %% coils)), 
         yp = 1:nrow(.)) %>% 
  mutate(grey = R+G+B,
         grey = grey / max(grey))

ggplot(img_spiral, aes(x=x, y=y, size = 1-grey)) +
  geom_path() +
  scale_size_continuous(range = c(0.1, 1.5))+
  coord_fixed() +
  theme_minimal() +
  theme(
    legend.position = "none"
  )

ggplot(img_spiral, aes(x=x, y=y, size = 1-grey, color = color)) +
  geom_point() +
  scale_size_continuous(range = c(0.1, 1.5))+
  scale_color_identity() +
  # coord_polar() +
  coord_fixed() +
  theme_minimal()+
  theme(
    legend.position = "none"
  )

ggplot(img_spiral, aes(x=x, y=y,  color = color)) +
  geom_point() +
  # scale_size_continuous(range = c(0.1, 1.5))+
  scale_color_identity() +
  coord_fixed() +
  theme_minimal()+
  theme(
    legend.position = "none"
  )

#Animated
n = 1
nn = 3920
arr = c()
while(length(arr) < nn){
  arr <- c(arr, rep(n, n))
  n <- n+1
}
arr <- arr[1:nn]

img_spiral %>% 
  mutate(time = row_number(), group = 1) %>% 
  ggplot(aes(x=x, y=y, size = (1-grey)^(3/2), group = group)) +
  geom_path() +
  scale_size_continuous(range = c(0.01, 2))+
  # coord_fixed() +
  theme_void() +
  theme(
    legend.position = "none"
  ) +
  transition_reveal(group, time)


img_spiral %>% 
  mutate(time = row_number(), group = 1,
         r = ((x - max(x)/2)^2 + (y - max(y)/2)^2)^(1/2),
         theta = atan((y - max(y)/2)/(x - max(x)/2))
         ) %>% 
  ggplot(aes(x=r, y=r, size = (1-grey)^(3/2), group = group)) +
  geom_path() +
  scale_size_continuous(range = c(0.01, 2)) +
  coord_polar(theta = "y") +
  theme_void() +
  theme(
    legend.position = "none"
  ) +
  transition_reveal(group, time)


img_spiral %>% 
  mutate(time = row_number(), group = 1) %>% 
  ggplot(aes(x=x, y=y, size = (1-grey)^(3/2), 
             group = group, color = color)) +
  geom_path() +
  scale_size_continuous(range = c(0.01, 2))+
  scale_color_identity() +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "none"
  ) +
  transition_reveal(group, time) +
  ease_aes("circular-in")

anim_save("gg_color.gif")

img_spiral %>% 
  mutate(time = row_number(), group = 1) %>% 
  ggplot(aes(x=x, y=y,color = color, group = group)) +
  geom_point(size = 3) +
  scale_color_identity() +
  coord_fixed() +
  theme_void()+
  theme(
    legend.position = "none"
  ) +
  transition_reveal(time, time)

img_spiral %>% 
  mutate(time = row_number()) %>% 
  ggplot(aes(x=xs, y=ys,  fill = color)) +
  geom_raster() +
  # scale_size_continuous(range = c(0.1, 1.5))+
  scale_fill_identity() +
  coord_fixed() +
  theme_void()+
  theme(
    legend.position = "none"
  ) +
  transition_reveal(time, time)



