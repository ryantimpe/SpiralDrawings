source("Functions.R")

#Image
size = 50
einstein <- readJPEG("Einstein.jpg") %>% 
  scale_image(size*2) %>% 
  spiral_cartesian(spiral_radius = size, num_coils = 50, 
                   chord_length = 2, rotation = 0) %>% 
  project_image()

#Go-to color set
gg_colors <- sel_color <- c(
  "#9affd0", #Aqua
  "#ffb5f5", #Pink
  "#5384ff", #Blue
  "#ff9e53", #Orange
  #"#ffed89", #Yellow
  "#de89ff", #Purple
  "#00436b", #RT blue
  "#ff6141", #Red/Orange
  "#ff25ab" #Bright pink
)

header_spiral <- c(300, 600, 900, 1200, 2000, 2900, nrow(einstein$projected_spiral)) %>% 
  map2_df(
    sample(gg_colors, 7),
    function(ii, cc){
      dat <- einstein$projected_spiral %>% 
        filter(row_number() <= ii) %>% 
        mutate(spir_group = ii,
               fill = cc)
      
      return(dat)
    })

ggplot(header_spiral, aes(x=x, y=y, size = grey)) +
  geom_path(aes(color = fill)) +
  scale_size_continuous(range = c(0.1, 1.5))+
  scale_color_identity() +
  coord_fixed() +
  facet_grid(cols = vars(spir_group)) +
  theme_void() +
  theme(
    strip.text = element_blank(),
    legend.position = "none"
  )
