library(tidyverse)
library(ggforce)
library(colorspace)
library(bigsnpr)

source("functions.R")

# Parameters to play with
max_ratio   <- 48 # how big is he sun
nu_nodes    <- 35 # no of planets
nu_branches <- 17 # no of orbits

# There must be less orbits than planets!
if (nu_branches >= nu_nodes) stop("No. of branches must be <= No. of nodes - 1")

# I assign a initial node to each branch (orbit)
nodes_raw_init <- tibble(id = sample(1:nu_nodes, nu_branches), 
       branch = 1:nu_branches)

# I complete the branches with the rest of nodes still not assigned
# so nodes_raw divides all nodes (planets) into branches
tibble(id = 1:nu_nodes) %>% 
  anti_join(nodes_raw_init, by = "id") %>% 
  mutate(branch = sample(1:nu_branches, 
                         nrow(.), 
                         replace = TRUE)) %>% 
  bind_rows(nodes_raw_init) -> nodes_raw

# list_branches is the detailed configuration of each orbit
# first node (planet) is the seed of the orbit and must be
# already located in some previous orbit
list_branches <- list()

for(i in 1:nu_branches){
  seed <- nodes_raw %>% 
    filter(branch <= (i-1)) %>% 
    slice(sample(1:nrow(.), 1)) %>% 
    pull(id)
  list_branches[[i]] <- c(seed, nodes_raw %>% filter(branch == i) %>% pull(id))
}


# nodes_partial adds the center (x0, y0) of a circumference and a 
# range (amplitude) in radians to help to calculate the exact location
# of each planet in a later step; is initialized locating the first 
# orbit in a circle centered at origin with ratio equal to 1.5*max_ratio
nodes_partial <- tibble(ids = list(list_branches[[1]]), x0 = 0, y0 = 0, range = 2 * pi)

# nodes contains the exact location of each planet ans is initialized
# with the seed (1st element) of the 2nd orbit or with the 1st element
# of 1st orbit in case of nu_branches = 1
nodes <- tibble(id = list_branches[[min(nu_branches, 2)]][1], 
                x = 0, y = 1.5 * max_ratio)

# Here I calculate the origin of each orbit to complete nodes_partial
for(i in 1:nu_branches){
    row <- nodes_partial %>% slice(i)
    nodes_to_add <- locate_nodes(row$ids, row$x0, row$y0, row$range)
    nodes %>% bind_rows(nodes_to_add) -> nodes
    if((nu_branches >= 2) & (i < nu_branches)){
      seed <- list_branches[[i+1]][1]
      seed_xy <- nodes %>% filter(id == seed)
      angle <- seed_xy %>% mutate(angle = atan2(y, x)) %>% pull(angle)
      nodes_partial %>% 
        add_row(ids = list(list_branches[[i+1]]), 
                x0 = seed_xy$x + runif(1, min = 1.5, max = 4)*max_ratio*cos(angle - pi), 
                y0 = seed_xy$y + runif(1, min = 1.5, max = 4)*max_ratio*sin(angle - pi),
                range = runif(1, min = pi/2, max = 2*pi)*sample(c(-1,1),1)) -> nodes_partial
    }
}

# edges_without_origin is the specification of the connections 
# between nodes (planets)
lapply(list_branches, function(x){
  tibble(from = x, to = lead(x))}) %>% 
  bind_rows() %>% 
  na.omit() %>% 
  filter(from != to) -> edges_without_origin

# I add a column called ids with a list of from-to
edges_without_origin %>% 
  rowwise() %>% 
  mutate(ids = list(c(from, to))) %>% 
  ungroup() -> edges_without_origin_list

# edges contains the origin of the arc between from-to planets
edges <- tibble(from = numeric(), 
                to = numeric(),
                x0 = numeric(), 
                y0 = numeric())

for(i in 1:nrow(edges_without_origin_list)){
  edges_without_origin_list %>% slice(i) -> fila
  fila %>% pull(ids) %>% unlist -> ids_vec_edges
  match <- FALSE
  j <- 0
  while(!match & j < nrow(nodes_partial)){
    j <- j + 1
    ids_vec_edges_partial <- nodes_partial %>% slice(j) %>% pull(ids) %>% unlist 
    if(length(setdiff(ids_vec_edges, ids_vec_edges_partial)) == 0) {
      i1 <- which(ids_vec_edges[1] == ids_vec_edges_partial)
      i2 <- which(ids_vec_edges[2] == ids_vec_edges_partial)
     # if((i2 - i1) == 1 | j == 1) match <- TRUE
      if((i2 - i1) == 1 ) match <- TRUE
    }
  }
  # When I find a match I add a row to edges
   select(fila, from, to) %>% 
     bind_cols(nodes_partial %>% slice(j) %>% select(x0, y0)) %>% 
     bind_rows(edges) -> edges
}

# Add location of initial and ending node to each edge
edges %>%
  inner_join(nodes, by = c("from" = "id")) %>%
  inner_join(nodes, by = c("to" = "id"), suffix = c("ini", "end")) -> edges_xy

# Calculate inner points for each edge
edges_xy %>%
  split(seq(nrow(.))) %>%
  lapply(function(row) create_arc(row$xini, 
                            row$yini, 
                            row$xend, 
                            row$yend, 
                            row$x0, 
                            row$y0) %>% 
           mutate(from = row$from,
                  to = row$to)) %>%
  bind_rows(.id = "edge_id") %>% 
  mutate(edge_id = as.numeric(edge_id)) -> edges_points

# asteroids: planets are defined by semi-concentric circles  
nodes %>% 
  mutate(name = "planet",
         r = abs(rnorm(nrow(.), mean = 14, sd = 3))) %>% 
  add_row(id = 0, x = 0, y = 0, name = "sun", r = max_ratio) %>% 
  arrange(id) %>% 
  group_by(id) %>% 
  group_split() %>% 
  lapply(function(r) { 
       make_asteroid(r$x, r$y, r$r, r$name)
    }) %>% 
  bind_rows(.id = "id") -> asteroids

# To frame the plot
asteroids %>% 
  summarise (xM = max(x + r),
             xm = min(x - r),
             yM = max(y + r),
             ym = min(y - r)) -> ranges
xlimits <- ranges %>% select(xm, xM) %>% slice(1)
ylimits <- ranges %>% select(ym, yM) %>% slice(1)
hx <- max(abs(xlimits))
hy <- max(abs(ylimits))
d <- min(hx, hy) * 0.12

# The plot at last
ggplot() + 
  geom_path(aes(x,y, group = interaction(edge_id, id_orbit)), 
            data = edges_points,
            linetype = "solid",
            color = "gray80") +

  geom_circle(aes(x0 = x, 
                  y0 = y, 
                  r = r, 
                  fill = col_fill, 
                  colour = col_line, 
                  group = id),
              asteroids %>% filter(id == 1),
              linewidth = 1) +
  
  geom_circle(aes(x0 = x, 
                  y0 = y, 
                  r = r, 
                  fill = col_fill, 
                  colour = col_line, 
                  group = id),
              data = asteroids %>% filter(id != 1)) +
  scale_colour_identity() +
  scale_fill_identity() +

  scale_x_continuous(limits = c(xlimits$xm-d, xlimits$xM+d),
                     expand = c(0,0)) +
  
  scale_y_continuous(limits =  c(ylimits$ym-d, ylimits$yM+d),
                     expand = c(0,0)) + 
  coord_equal() +
  theme(legend.position='none',
        panel.grid = element_blank(),
        axis.title =element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_rect(color="black", 
                                        fill = "#F2EECB")) -> plot


# If you like the plot, you can save it here
f <- ((ylimits[2]+d)-(ylimits[1]-d))/((xlimits[2]+d)-(xlimits[1]-d))
# randon name
name <- paste0(paste(sample(letters, 4), collapse = ''),".jpg")
ggsave(name, plot, height = as.numeric(15*f), width = 15, dpi = 600)

