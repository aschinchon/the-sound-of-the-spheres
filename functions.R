########################################################################
# Creates an arc between two points
# (x, y) initial point
# (xend, yend) ending point
# (x0, y0) origin
########################################################################
create_arc <- function(x, y, xend, yend, x0, y0){
  radius <- (sqrt((x - x0)^2 + (y - y0)^2) + sqrt((xend - x0)^2 + (yend - y0)^2))/2
  ini_angle <- atan2(y - y0, x - x0)
  end_angle <- atan2(yend - y0, xend - x0)
  if ((end_angle - ini_angle) < - pi) end_angle <- end_angle + 2 * pi
  if ((end_angle - ini_angle) > + pi) ini_angle <- ini_angle + 2 * pi
  
  end_angle <- ini_angle + 2*pi
  
  angles <- seq(from = ini_angle, to = end_angle, by = sign(end_angle - ini_angle) * 1 * pi / 180) 
  
  tibble(r = radius + c(0, cumsum(rev(diff(seq_log(radius, radius + 325, length.out = 75)))))) %>% 
    split(seq(nrow(.))) %>%
    lapply(function(z) tibble(x = z$r * cos(angles) + x0,
                              y = z$r * sin(angles) + y0,
                              part = "edge")) %>% 
    bind_rows(.id = "id_orbit") %>% 
    mutate(id_orbit = as.numeric(id_orbit)) #%>% 

}

########################################################################
# Locate nodes in an arc
# list_ids: list with nodes to be located
# (x0, y0): origin
# range: between 0 and 2*pi
########################################################################
locate_nodes <- function(list_ids, x0, y0, range){
  nodes_ids <- list_ids %>% unlist
  anchor_all <- nodes %>% filter(id %in% nodes_ids)
  nodes_to_locate <- setdiff(nodes_ids, anchor_all %>% pull(id))
  if (length(nodes_to_locate) > 0){
    anchor_first <- anchor_all %>% slice(1)
    radius <- anchor_first %>% 
      transmute(sqrt((x-x0)^2 + (y-y0)^2)) %>% 
      as.numeric
    angle <- anchor_first %>% 
      transmute(atan2(y - y0, x - x0)) %>% 
      as.numeric
    
    angles <- seq(from = angle, 
                  to = angle + range, 
                  length.out = (length(nodes_to_locate) + 2))[2:(length(nodes_to_locate) + 1)]
    
    tibble(id = nodes_to_locate,
           x = x0 + radius * cos(angles),
           y = y0 + radius * sin(angles))
  }
  
}

########################################################################
# Creates an asteroid through concentrin circles
# (x0, y0): origin
# r: radius
# name: just to distinguish the sun
########################################################################
make_asteroid <- function(x0, y0, r, name){
  
  if(name == "sun"){
    pal1 <- c("#BE0128", "#BB0F2E", "#FC1A20",
              "#FE3503", "#FB7B02", "#FCC929",
              "#FFF9DF")
    pal2 <- darken(pal1, 0.2)
  }else{
    color <- "#B4B4B4"
    palette <- colorRampPalette(c(color,"#FFFFFF"))
    pal1 <- palette(7)
    pal2 <- darken(pal1, 0.4)
  }
  
  n <- length(pal1)
  # pick a random angle
  angle <- runif(1, 0, 2*pi)
  
  # pick a random point on the radius close to the center
  rc <- runif(1, min = 0, max = r * 0.6)
  
  
  tibble(x = x0 + (seq(from = 0, to = rc, length.out = n)) * cos(angle),
         y = y0 + (seq(from = 0, to = rc, length.out = n)) * sin(angle),
         r = seq(from = r, to = jitter(0.15, factor = 8) * r, length.out = n),
         col_fill = pal1,
         col_line = pal2) %>% 
    arrange(-r) %>% mutate(grupo = row_number())
  
}

########################################################################
# To decide the color of lines depending on the filling color
# https://stackoverflow.com/questions/19689952/how-to-determine-luminance
########################################################################
Luminance <- function(color) {
  RGB <- col2rgb(color)
  R <- as.numeric(RGB[1,1])
  G <- as.numeric(RGB[2,1])
  B <- as.numeric(RGB[3,1])
  ((0.2126*R) + (0.7152*G) + (0.0722*B)) / 255
}
  

